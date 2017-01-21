#[macro_use]
extern crate log;
extern crate irc;
extern crate hyper;
extern crate slack;
#[macro_use]
extern crate lazy_static;
extern crate regex;
extern crate toml;
extern crate rustc_serialize;
extern crate multimap;
extern crate time;

use std::env;
use std::fmt::Debug;
use std::collections::HashMap;
use std::thread;
use std::sync::{Arc, Mutex};
use std::sync::mpsc::{Sender, channel};
use std::default::Default;
use std::fs::File;
use std::io::prelude::*;
use std::time::Duration;

use log::{LogRecord, LogMetadata, LogLevelFilter};
use irc::client::prelude::{Command, Response, IrcServer, Server, ServerExt};
use regex::Regex;
use rustc_serialize::json::Json;
use multimap::MultiMap;
use time::Tm;

struct StdoutLogger;

impl log::Log for StdoutLogger {
    fn enabled(&self, _: &LogMetadata) -> bool {
        true
    }

    fn log(&self, record: &LogRecord) {
        println!("[{}] [{}] {}", time::now().strftime("%Y-%m-%d %T").unwrap(), record.level(), record.args());
    }
}

#[derive(Debug, RustcDecodable)]
struct Config {
    irc_nick: String,
    irc_server: String,
    irc_port: Option<u16>,
    irc_ssl: Option<bool>,
    irc_password: Option<String>,
    slack_user: String,
    slack_token: String,
}

#[derive(Debug)]
enum ToIrc {
    Message { to: String, msg: String },
    MeMessage { to: String, msg: String },
    Raw(String),
    Away(bool),
    Join(String),
    Part(String),
    Topic { chan: String, topic: String },
}

#[derive(Debug)]
enum ToSlack {
    Message { to: String, from: String, msg: String },
    Topic { by: Option<String>, chan: String, topic: Option<String> },
    Kick { by: String, chans: Vec<String>, nicks: Vec<String>, reason: Option<String> },
    Join { nick: String, chans: Vec<String> },
    Part { nick: String, chans: Vec<String>, reason: Option<String> },
    Quit { nick: String, chans: Vec<String>, reason: Option<String> },
    Nick { old_nick: String, new_nick: String, chans: Vec<String> },
    Mode { by: String, name: String, modes: String, params: Option<String> },
    Whois(String, Whois),
    Error(String),
    UpdateChannels,
}

#[derive(Debug)]
enum Whois {
    User { user: String, host: String, real_name: String },
    Host(String),
    Channels(Vec<String>),
    Server { server: String, server_info: String },
    Modes(String),
    Away(String),
    Account { msg: String, account: String },
    Registered(String),
    Secure(String),
    Idle { seconds: u32, signon: Tm },
    End(String),
}

struct SlackHandler<'a> {
    irc_tx: &'a Sender<ToIrc>,
    slack_tx: &'a Sender<ToSlack>,
    user_id: &'a str,
    bot_channel: &'a str,
}

fn get_channel_with_id(cli: &slack::RtmClient, id: &str) -> Option<slack::Channel> {
    cli.get_channels().into_iter().find(|channel| channel.id == id)
}

fn get_user_with_id(cli: &slack::RtmClient, id: &str) -> Option<slack::User> {
    cli.get_users().into_iter().find(|user| user.id == id)
}

fn log_err<T, E: Debug>(res: Result<T, E>) {
    if let Err(err) = res {
        error!("{:?}", err);
    }
}

fn parse_slack_text(text: &str, cli: &slack::RtmClient) -> String {
    lazy_static! {
        static ref REPLACEMENTS: Vec<(Regex, &'static str)> = vec![
            (Regex::new(r"\n|\r\n|\r").unwrap(), " "),
            (Regex::new(r"&amp;").unwrap(), "&"),
            (Regex::new(r"&lt;").unwrap(), "<"),
            (Regex::new(r"&gt;").unwrap(), ">"),
            (Regex::new(r"<!channel>").unwrap(), "@channel"),
            (Regex::new(r"<!group>").unwrap(), "@group"),
            (Regex::new(r"<!everyone>").unwrap(), "@everyone"),
            // http://google.com|google.com -> google.com
            (Regex::new(r"\bhttps?://\S+\|(\S+)").unwrap(), "$1"),
            // mailto:someone@example.com|someone@example.com -> someone@example.com
            (Regex::new(r"\bmailto:\S+\|(\S+)").unwrap(), "$1"),
        ];
        static ref EMOJIS: HashMap<&'static str, &'static str> = {
            let mut m = HashMap::new();
            m.insert("smile", ":)");
            m.insert("simple_smile", ":)");
            m.insert("smiley", ":-)");
            m.insert("grin", ":D");
            m.insert("wink", ";)");
            m.insert("smirk", ";)");
            m.insert("blush", ":$");
            m.insert("stuck_out_tongue", ":P");
            m.insert("stuck_out_tongue_winking_eye", ";P");
            m.insert("stuck_out_tongue_closed_eyes", "xP");
            m.insert("disappointed", ":(");
            m.insert("astonished", ":O");
            m.insert("open_mouth", ":O");
            m.insert("heart", "<3");
            m.insert("broken_heart", "</3");
            m.insert("confused", ":S");
            m.insert("angry", ">:(");
            m.insert("cry", ":,(");
            m.insert("frowning", ":(");
            m.insert("imp", "]:(");
            m.insert("innocent", "o:)");
            m.insert("joy", ":,)");
            m.insert("kissing", ":*");
            m.insert("laughing", "x)");
            m.insert("neutral_face", ":|");
            m.insert("no_mouth", ":-");
            m.insert("rage", ":@");
            m.insert("smiling_imp", "]:)");
            m.insert("sob", ":,'(");
            m.insert("sunglasses", "8)");
            m.insert("sweat", ",:(");
            m.insert("sweat_smile", ",:)");
            m.insert("unamused", ":$");
            m
        };
        static ref FUNCTIONS: Vec<(Regex, Box<Fn(&regex::Captures, &slack::RtmClient) -> String + Sync>)> = vec![
            (Regex::new(r"<#(C\w+)\|?([\w-]+)?>").unwrap(), Box::new(|captures: &regex::Captures, cli: &slack::RtmClient| {
                if let Some(id) = captures.at(1) {
                    if let Some(channel) = get_channel_with_id(cli, id) {
                        return format!("#{}", channel.name).into()
                    }
                }
                captures.at(2).unwrap_or("").into()
            })),
            (Regex::new(r"<@(U\w+)\|?([\w-]+)?>").unwrap(), Box::new(|captures: &regex::Captures, cli: &slack::RtmClient| {
                if let Some(id) = captures.at(1) {
                    if let Some(user) = get_user_with_id(cli, id) {
                        return format!("@{}", user.name).into()
                    }
                }
                captures.at(2).unwrap_or("").into()
            })),
            (Regex::new(r"<([^!]\S+)>").unwrap(), Box::new(|captures: &regex::Captures, _| {
                captures.at(1).unwrap_or("").into()
            })),
            (Regex::new(r"<!(\w+)\|?(\w+)?>").unwrap(), Box::new(|captures: &regex::Captures, _| {
                format!("<{}>", captures.at(2).or(captures.at(1)).unwrap_or("")).into()
            })),
            (Regex::new(r":(\w+):").unwrap(), Box::new(|captures: &regex::Captures, _| {
                if let Some(&emoji) = EMOJIS.get(captures.at(1).unwrap()) {
                    emoji.into()
                } else {
                    captures.at(0).unwrap().into()
                }
            })),
        ];
    }

    let text = REPLACEMENTS.iter().fold(text.into(), |text: String, &(ref re, replacement)| {
        re.replace_all(&text, replacement)
    });

    FUNCTIONS.iter().fold(text, |text, &(ref re, ref replacement)| {
        re.replace_all(&text, |captures: &regex::Captures| {
            replacement(captures, cli)
        })
    })
}

fn parse_irc_text(text: &str) -> String {
    lazy_static! {
        static ref REPLACEMENTS: Vec<(Regex, &'static str)> = vec![
            // strip formatting; via irc-colors.js
            // stripStyle str.replace(/([\x0F\x02\x16\x1F])(.+)\1/g, '$2')
            (Regex::new(r"\x0F(.+)\x0F").unwrap(), "$1"),
            (Regex::new(r"\x02(.+)\x02").unwrap(), "$1"), // bold
            (Regex::new(r"\x16(.+)\x16").unwrap(), "$1"),
            (Regex::new(r"\x1D(.+)\x1D").unwrap(), "$1"), // italics
            (Regex::new(r"\x1F(.+)\x1F").unwrap(), "$1"), // underline
            // stripColors str.replace(/\x03\d{0,2}(,\d{0,2}|\x02\x02)?/g, '')
            (Regex::new(r"\x03\d{0,2}(,\d{0,2}|\x02\x02)?").unwrap(), ""),
        ];
    }

    REPLACEMENTS.iter().fold(text.into(), |text, &(ref re, replacement)| {
        re.replace_all(&text, replacement)
    })
}

impl<'a> slack::EventHandler for SlackHandler<'a> {
    fn on_event(&mut self, cli: &mut slack::RtmClient, event: Result<&slack::Event, slack::Error>, raw_json: &str) {
        match event {
            Ok(&slack::Event::Message(ref message)) => match message {
                &slack::Message::Standard { channel: Some(ref channel), user: Some(ref user), text: Some(ref text), .. } if user == self.user_id => {
                    lazy_static! {
                        static ref PM_RE: Regex = Regex::new(r"^(\S+):\s+(.+)").unwrap();
                    }

                    let text = parse_slack_text(&text, cli);

                    if text.starts_with("%") {
                        self.irc_tx.send(ToIrc::Raw(text[1..].into())).unwrap();
                        log_err(cli.send_message(channel, "_sent raw command_"));
                    } else if channel == self.bot_channel {
                        if let Some(captures) = PM_RE.captures(&text) {
                            self.irc_tx.send(ToIrc::Message { to: captures.at(1).unwrap().into(), msg: captures.at(2).unwrap().into() }).unwrap();
                        } else {
                            log_err(cli.send_message(channel, "_no message sent, are you missing a `user: ` prefix?_"));
                        }
                    } else {
                        self.irc_tx.send(ToIrc::Message { to: format!("#{}", get_channel_with_id(cli, channel).expect("channel with id").name), msg: text.clone() }).unwrap();
                    }
                },
                &slack::Message::MeMessage { ref channel, ref user, ref text, .. } if user == self.user_id => {
                    let text = parse_slack_text(&text, cli);

                    self.irc_tx.send(ToIrc::MeMessage { to: format!("#{}", get_channel_with_id(cli, channel).expect("channel with id").name), msg: text.clone() }).unwrap();
                },
                &slack::Message::ChannelTopic { ref user, ref topic, .. } if user == self.user_id => {
                    if let Some(channel) = Json::from_str(raw_json).expect("ChannelTopic message json").find("channel").and_then(|j| j.as_string()) {
                        self.irc_tx.send(ToIrc::Topic { chan: format!("#{}", get_channel_with_id(cli, channel).expect("channel with id").name), topic: topic.clone() }).unwrap();
                    }
                },
                _ => {
                    debug!("[SLACK] {:?}", event);
                },
            },
            Ok(&slack::Event::PresenceChange { ref user, ref presence }) if user == self.user_id => {
                self.irc_tx.send(ToIrc::Away(presence != "active")).unwrap();
            },
            Ok(&slack::Event::ChannelJoined { ref channel }) => {
                if cli.get_channels().into_iter().find(|c| c.id == channel.id).is_none() {
                    // we haven't seen this channel before, so update channels
                    cli.update_channels().expect("updating channels");
                    // also update the other `cli` instance
                    self.slack_tx.send(ToSlack::UpdateChannels).unwrap();
                }
                self.irc_tx.send(ToIrc::Join(format!("#{}", channel.name))).unwrap();
            },
            Ok(&slack::Event::ChannelLeft { ref channel }) => {
                self.irc_tx.send(ToIrc::Part(format!("#{}", get_channel_with_id(cli, channel).expect("channel with id").name))).unwrap();
            },
            evt => {
                debug!("[SLACK] {:?}", evt);
            },
        }
    }

    fn on_ping(&mut self, _cli: &mut slack::RtmClient) {}

    fn on_close(&mut self, _cli: &mut slack::RtmClient) {
        warn!("Disconnected from Slack");
    }

    fn on_connect(&mut self, _cli: &mut slack::RtmClient) {}
}

fn post_message(cli: &slack::RtmClient, token: &str, to: &str, text: &str, username: Option<&str>) {
    let to: &str = if to.starts_with("#") {
        match cli.get_channel_id(&to[1..]) {
            Some(id) => id,
            None => {
                error!("Failed to find channel '{}'", &to[1..]);
                return;
            },
        }
    } else {
        to
    };

    let client = hyper::Client::new();
    let icon_url = username.map(|username| format!("http://api.adorable.io/avatars/48/{}.png", username));
    let icon_url = icon_url.as_ref().map(|s| s.as_ref());
    log_err(slack::api::chat::post_message(
        &client,
        token,
        to,
        text,
        username,
        Some(username.is_none()), // if no username, send `as_user`
        None,
        Some(false),
        None,
        None,
        None,
        icon_url,
        None,
    ));
}

fn main() {
    log::set_logger(|max_log_level| {
        max_log_level.set(LogLevelFilter::Info);
        Box::new(StdoutLogger)
    }).unwrap();

    let c: Arc<Config> = {
        let mut s: String = String::new();
        let file_path = env::args().skip(1).next().unwrap_or("config.toml".into());
        File::open(file_path).and_then(|mut f| f.read_to_string(&mut s)).unwrap();
        toml::decode_str::<Config>(&s).expect("parse config").into()
    };

    let (slack_tx, slack_rx) = channel::<ToSlack>();
    let (irc_tx, irc_rx) = channel::<ToIrc>();

    let slack_thread = {
        let c = c.clone();
        let slack_tx = slack_tx.clone();
        thread::Builder::new().name("slack".into()).spawn(move || {
            let mut cli = slack::RtmClient::new(&c.slack_token);
            cli.login().expect("logging into Slack");

            // auto-join channels the bot has been invited to
            for channel in cli.get_channels().into_iter().filter(|c| c.is_member) {
                irc_tx.send(ToIrc::Join(format!("#{}", channel.name))).unwrap();
            }

            let user_id = cli.get_user_id(&c.slack_user).expect("user id of Slack user").clone();
            let bot_channel = cli.im_open(&user_id).expect("IM channel with Slack bot").channel.id;

            let recv_thread = {
                let c = c.clone();
                let bot_channel = bot_channel.clone();
                thread::Builder::new().name("slack_recv".into()).spawn(move || {
                    let mut cli = slack::RtmClient::new(&c.slack_token);
                    let mut handler = SlackHandler {
                        irc_tx: &irc_tx,
                        slack_tx: &slack_tx,
                        user_id: &user_id,
                        bot_channel: &bot_channel,
                    };

                    while let Err(e) = cli.login_and_run(&mut handler) {
                        slack_tx.send(ToSlack::Error(format!("{:?}", e))).unwrap();
                        thread::sleep(Duration::from_secs(1));
                    }
                }).unwrap()
            };

            for msg in slack_rx {
                match msg {
                    ToSlack::Message { to, from, msg } => {
                        let to: &str = if to.starts_with("#") {
                            match cli.get_channel_id(&to[1..]) {
                                Some(ref id) => id,
                                None => return,
                            }
                        } else {
                            &bot_channel
                        };

                        post_message(&cli, &c.slack_token, to, &msg, Some(&from));
                    },
                    ToSlack::Topic { by, chan, topic } => {
                        if let Some(by) = by {
                            post_message(&cli, &c.slack_token, &chan, &format!("*{}* has changed the topic", by), None);
                        }
                        log_err(cli.set_topic(&chan, &topic.unwrap_or("".into()).chars().take(250).collect::<String>()));
                    },
                    ToSlack::Kick { by, chans, nicks, reason } => {
                        let reason = &reason.unwrap_or("".into());

                        for chan in chans {
                            for nick in &nicks {
                                post_message(&cli, &c.slack_token, &chan, &format!("*{}* has kicked *{}* (_{}_)", by, nick, reason), None);
                            }
                        }
                    },
                    ToSlack::Join { nick, chans } => {
                        for chan in chans {
                            post_message(&cli, &c.slack_token, &chan, &format!("*{}* has joined", nick), None);
                        }
                    },
                    ToSlack::Part { nick, chans, reason } => {
                        let reason = &reason.unwrap_or("".into());

                        for chan in chans {
                            post_message(&cli, &c.slack_token, &chan, &format!("*{}* has left (_{}_)", nick, reason), None);
                        }
                    },
                    ToSlack::Quit { nick, chans, reason } => {
                        let reason = &reason.unwrap_or("".into());

                        for chan in chans {
                            post_message(&cli, &c.slack_token, &chan, &format!("*{}* has quit (_{}_)", nick, reason), None);
                        }
                    },
                    ToSlack::Nick { old_nick, new_nick, chans } => {
                        for chan in chans {
                            post_message(&cli, &c.slack_token, &chan, &format!("*{}* is now known as *{}*", old_nick, new_nick), None);
                        }
                    },
                    ToSlack::Mode { by, name, modes, params } => {
                        if name.starts_with("#") {
                            post_message(&cli, &c.slack_token, &name, &format!("*{}* sets mode *{}* on _{}_", by, modes, params.unwrap_or(name.clone())), None);
                        }
                    },
                    ToSlack::Whois(nick, info) => {
                        let info = match info {
                            Whois::User { user, host, real_name } => {
                                format!("(_{}@{}_): _{}_", user, host, real_name)
                            },
                            Whois::Channels(chans) => {
                                format!("is in channels {}", chans.join(", "))
                            },
                            Whois::Server { server, server_info } => {
                                format!("_{}_ :_{}_", server, server_info)
                            },
                            Whois::Away(reason) => {
                                format!("is away (_{}_)", reason)
                            },
                            Whois::Account { msg, account } => {
                                format!("{} _{}_", msg, account)
                            },
                            Whois::Idle { seconds, signon } => {
                                format!("idle {}s, signon: {}", seconds, signon.to_local().rfc822())
                            },
                            Whois::Host(msg) | Whois::Modes(msg) | Whois::Registered(msg) | Whois::Secure(msg) | Whois::End(msg) => {
                                msg
                            },
                        };

                        post_message(&cli, &c.slack_token, &bot_channel, &format!("[*{}*] {}", nick, info), None);
                    },
                    ToSlack::Error(msg) => {
                        post_message(&cli, &c.slack_token, &bot_channel, &msg, None);
                    },
                    ToSlack::UpdateChannels => {
                        cli.update_channels().expect("updating channels");
                    },
                }
            }

            recv_thread.join().expect("joining Slack recv thread");
        }).unwrap()
    };

    let irc_thread = {
        let c = c.clone();
        thread::Builder::new().name("irc".into()).spawn(move || {
            let nick_to_chan = Arc::new(Mutex::new(MultiMap::<String, String>::new()));

            let config = irc::client::data::Config {
                nickname: Some(c.irc_nick.clone()),
                server: Some(c.irc_server.clone()),
                port: c.irc_port,
                password: c.irc_password.clone(),
                use_ssl: c.irc_ssl.or(Some(true)),
                ..Default::default()
            };

            let server = IrcServer::from_config(config).unwrap();

            server.identify().expect("logging into IRC");

            let server_thread = {
                let nick_to_chan = nick_to_chan.clone();
                let server = server.clone();
                thread::Builder::new().name("irc_server".into()).spawn(move || {
                    for message in server.iter() {
                        let message = message.expect("IRC message");
                        // extract the nick from the `nick!nick@hostname.com`
                        let sender = message.prefix.and_then(|prefix| prefix.split("!").nth(0).map(Into::into)).unwrap_or("".into()); // only `None` for `Ping`, AFAICT
                        match message.command {
                            Command::ERROR(msg) => {
                                slack_tx.send(ToSlack::Error(msg)).unwrap();
                            },
                            Command::PRIVMSG(to, text) | Command::NOTICE(to, text) => {
                                lazy_static! {
                                    static ref ACTION_RE: Regex = Regex::new("^\x01ACTION (.+)\x01$").unwrap();
                                }

                                let text = parse_irc_text(&text);

                                if ACTION_RE.is_match(&text) {
                                    slack_tx.send(ToSlack::Message {
                                        to: to,
                                        from: sender,
                                        msg: format!("_{}_", ACTION_RE.captures(&text).unwrap().at(1).unwrap())
                                    }).unwrap();
                                } else {
                                    slack_tx.send(ToSlack::Message { to: to, from: sender, msg: text }).unwrap();
                                }
                            },
                            Command::TOPIC(channel, topic) => {
                                // topic set by a user while we're in the channel
                                slack_tx.send(ToSlack::Topic { by: Some(sender), chan: channel, topic: topic }).unwrap();
                            },
                            Command::KICK(channels, users, reason) => {
                                let chans = channels.split(",").map(Into::into).collect::<Vec<_>>();
                                let nicks = users.split(",").map(Into::into).collect();

                                {
                                    let mut nick_to_chan = nick_to_chan.lock().unwrap();
                                    for nick in &nicks {
                                        if let Some(mut v) = nick_to_chan.get_vec_mut(nick) {
                                            v.retain(|c| !chans.contains(c));
                                        }
                                    }
                                }

                                slack_tx.send(ToSlack::Kick { by: sender, chans: chans, nicks: nicks, reason: reason }).unwrap();
                            },
                            Command::JOIN(channels, _, _) => {
                                let chans = channels.split(",").map(Into::into).collect::<Vec<String>>();

                                {
                                    let mut nick_to_chan = nick_to_chan.lock().unwrap();
                                    for chan in &chans {
                                        nick_to_chan.insert(sender.clone(), chan.clone());
                                    }
                                }

                                slack_tx.send(ToSlack::Join { nick: sender, chans: chans }).unwrap();
                            },
                            Command::PART(channels, reason) => {
                                let chans = channels.split(",").map(Into::into).collect::<Vec<_>>();

                                if let Some(mut v) = nick_to_chan.lock().unwrap().get_vec_mut(&sender) {
                                    v.retain(|c| !chans.contains(c));
                                }

                                slack_tx.send(ToSlack::Part { nick: sender, chans: chans, reason: reason }).unwrap();
                            },
                            Command::QUIT(reason) => {
                                let mut nick_to_chan = nick_to_chan.lock().unwrap();
                                let chans = nick_to_chan.get_vec(&sender).map(|v| v.clone()).unwrap_or(vec![]);

                                nick_to_chan.remove(&sender);

                                slack_tx.send(ToSlack::Quit { nick: sender, chans: chans, reason: reason }).unwrap();
                            },
                            Command::NICK(nick) => {
                                let mut nick_to_chan = nick_to_chan.lock().unwrap();
                                let chans = nick_to_chan.get_vec(&sender).map(|v| v.clone()).unwrap_or(vec![]);

                                if let Some(mut v) = nick_to_chan.remove(&sender) {
                                    match nick_to_chan.entry(nick.clone()) {
                                        multimap::Entry::Occupied(mut e) => {
                                            warn!("'{}' changed nick to extant nick '{}'", sender, nick);
                                            let mut v_ = e.get_vec_mut();
                                            v_.clear();
                                            v_.append(&mut v);
                                        },
                                        multimap::Entry::Vacant(e) => {
                                            e.insert_vec(v);
                                        },
                                    }
                                }

                                slack_tx.send(ToSlack::Nick { old_nick: sender, new_nick: nick, chans: chans }).unwrap();
                            },
                            Command::MODE(name, modes, params) => {
                                slack_tx.send(ToSlack::Mode { by: sender, name: name, modes: modes, params: params }).unwrap();
                            },
                            Command::Response(resp, args, suffix) => if resp.is_error() {
                                // error responses
                                slack_tx.send(ToSlack::Error(format!("{:?}: {} <{}>", resp, suffix.unwrap_or("".into()), args.join(" ")))).unwrap();
                            } else {
                                match resp {
                                    Response::RPL_NOTOPIC | Response::RPL_TOPIC => {
                                        // response to topic request
                                        slack_tx.send(ToSlack::Topic { by: None, chan: args[1].clone(), topic: suffix }).unwrap();
                                    },
                                    Response::RPL_NAMREPLY => {
                                        let ref chan = args[2];
                                        let mut nick_to_chan = nick_to_chan.lock().unwrap();
                                        for nick in suffix.unwrap().trim().replace("@", "").replace("+", "").split(" ").map(Into::into) {
                                            if nick_to_chan.get_vec(&nick).map(|v| !v.contains(&chan)).unwrap_or(true) {
                                                nick_to_chan.insert(nick, chan.clone());
                                            }
                                        }
                                    },
                                    Response::RPL_WHOISUSER => {
                                        slack_tx.send(ToSlack::Whois(args[1].clone(), Whois::User {
                                            user: args[2].clone(),
                                            host: args[3].clone(),
                                            real_name: suffix.unwrap()
                                        })).unwrap();
                                    },
                                    Response::RPL_WHOISCHANNELS => {
                                        slack_tx.send(ToSlack::Whois(args[1].clone(), Whois::Channels(
                                            suffix.unwrap().trim().replace("@", "").replace("+", "").split(" ").map(Into::into).collect()
                                        ))).unwrap();
                                    },
                                    Response::RPL_WHOISSERVER => {
                                        slack_tx.send(ToSlack::Whois(args[1].clone(), Whois::Server {
                                            server: args[2].clone(),
                                            server_info: suffix.unwrap()
                                        })).unwrap();
                                    },
                                    Response::RPL_AWAY => {
                                        slack_tx.send(ToSlack::Whois(args[1].clone(), Whois::Away(suffix.unwrap()))).unwrap();
                                    },
                                    Response::RPL_WHOISIDLE => {
                                        slack_tx.send(ToSlack::Whois(args[1].clone(), Whois::Idle {
                                            seconds: args[2].parse().expect("seconds from idle time"),
                                            signon: time::strptime(&args[3], "%s").expect("time from signon time"),
                                        })).unwrap();
                                    },
                                    Response::RPL_ENDOFWHOIS => {
                                        slack_tx.send(ToSlack::Whois(args[1].clone(), Whois::End(suffix.unwrap()))).unwrap();
                                    },
                                    _ => {
                                        debug!("[IRC] {:?}", Command::Response(resp, args, suffix));
                                    },
                                }
                            },
                            Command::Raw(id, args, suffix) => match id.as_str() {
                                "307" => { // RPL_WHOISREGNICK
                                    slack_tx.send(ToSlack::Whois(args[1].clone(), Whois::Registered(suffix.unwrap()))).unwrap();
                                },
                                "330" => { // RPL_WHOISACCOUNT
                                    slack_tx.send(ToSlack::Whois(args[1].clone(), Whois::Account {
                                        msg: suffix.unwrap(),
                                        account: args[2].clone()
                                    })).unwrap();
                                },
                                "378" => { // RPL_WHOISHOST
                                    slack_tx.send(ToSlack::Whois(args[1].clone(), Whois::Host(suffix.unwrap()))).unwrap();
                                },
                                "379" => { // RPL_WHOISMODES
                                    slack_tx.send(ToSlack::Whois(args[1].clone(), Whois::Modes(suffix.unwrap()))).unwrap();
                                },
                                "671" => { // RPL_WHOISSECURE
                                    slack_tx.send(ToSlack::Whois(args[1].clone(), Whois::Secure(suffix.unwrap()))).unwrap();
                                },
                                _ => {
                                    debug!("[IRC] {:?}", Command::Raw(id, args, suffix));
                                },
                            },
                            msg => {
                                debug!("[IRC] {:?}", msg);
                            },
                        }
                    }
                }).unwrap()
            };

            for msg in irc_rx {
                match msg {
                    ToIrc::Message { to, msg } => {
                        server.send_privmsg(&to, &msg).unwrap();
                    },
                    ToIrc::MeMessage { to, msg } => {
                        server.send_privmsg(&to, &format!("\x01ACTION {}\x01", msg)).unwrap();
                    },
                    ToIrc::Raw(msg) => {
                        server.send(Command::Raw(msg, vec![], None)).unwrap();
                    },
                    ToIrc::Away(is_away) => {
                        server.send(Command::AWAY(Some(if is_away { " ".into() } else { "".into() }))).unwrap();
                    },
                    ToIrc::Join(chan) => {
                        server.send_join(&chan).unwrap();
                    },
                    ToIrc::Part(chan) => {
                        for (_, v) in nick_to_chan.lock().unwrap().iter_all_mut() {
                            v.retain(|c| c != &chan);
                        }

                        server.send(Command::PART(chan, None)).unwrap();
                    },
                    ToIrc::Topic { chan, topic } => {
                        server.send_topic(&chan, &topic).unwrap();
                    },
                }
            }

            server_thread.join().unwrap();
        }).unwrap()
    };

    slack_thread.join().expect("join Slack thread");
    irc_thread.join().expect("join IRC thread");
}
