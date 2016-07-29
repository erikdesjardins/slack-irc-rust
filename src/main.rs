extern crate irc;
extern crate hyper;
extern crate slack;
#[macro_use] extern crate lazy_static;
extern crate regex;
extern crate toml;
extern crate rustc_serialize;

use std::fmt::Debug;
use std::collections::HashMap;
use std::thread;
use std::sync::Arc;
use std::sync::mpsc::{Sender, channel};
use std::default::Default;
use std::fs::File;
use std::io::prelude::*;

use irc::client::prelude::{Command, IrcServer, Server, ServerExt};
use regex::Regex;

#[derive(Debug, RustcDecodable)]
struct Config {
    irc_nick: String,
    irc_server: String,
    slack_user: String,
    slack_token: String,
}

#[derive(Debug)]
enum SlackToIrc {
    Message { to: String, msg: String },
    MeMessage { to: String, msg: String },
    Raw(String),
    Away(bool),
    Join(String),
    Part(String),
}

#[derive(Debug)]
enum IrcToSlack {
    Message { to: String, from: Option<String>, msg: String },
    Topic { chan: String, topic: Option<String> },
    Kick { by: Option<String>, chans: String, nicks: String, reason: Option<String> },
    Join { nick: String, chans: String },
    Part { nick: String, chans: String, reason: Option<String> },
    Quit { nick: String, reason: Option<String> },
    Nick { old_nick: String, new_nick: String },
    Mode { by: Option<String>, name: String, modes: String, params: Option<String> },
    Error(String),
}

struct SlackHandler<'a, T: 'a> {
    tx: &'a Sender<T>,
    user_id: &'a str,
    bot_channel: &'a str,
}

fn get_channel_with_id(cli: &slack::RtmClient, id: &str) -> Option<slack::Channel> {
    cli.get_channels().into_iter().find(|channel| {
        channel.id == id
    })
}

fn get_user_with_id(cli: &slack::RtmClient, id: &str) -> Option<slack::User> {
    cli.get_users().into_iter().find(|user| {
        user.id == id
    })
}

fn log_err<T, E: Debug>(res: Result<T, E>) {
    if let Err(err) = res {
        println!("[ERROR]: {:?}", err);
    }
}

fn parse_slack_text(text: &str, cli: &slack::RtmClient) -> String {
    lazy_static!{
        static ref REPLACEMENTS: Vec<(Regex, &'static str)> = vec![
            (Regex::new(r"\n|\r\n|\r").unwrap(), " "),
            (Regex::new(r"&amp;").unwrap(), "&"),
            (Regex::new(r"&lt;").unwrap(), "<"),
            (Regex::new(r"&gt;").unwrap(), ">"),
            (Regex::new(r"<!channel>").unwrap(), "@channel"),
            (Regex::new(r"<!group>").unwrap(), "@group"),
            (Regex::new(r"<!everyone>").unwrap(), "@everyone"),
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
            (Regex::new(r"<#(C\w+)\|?(\w+)?>").unwrap(), Box::new(|captures: &regex::Captures, cli: &slack::RtmClient| {
                if let Some(id) = captures.at(1) {
                    if let Some(channel) = get_channel_with_id(cli, id) {
                        return format!("#{}", channel.name).to_owned()
                    }
                }
                captures.at(2).unwrap_or("").to_owned()
            })),
            (Regex::new(r"<@(U\w+)\|?(\w+)?>").unwrap(), Box::new(|captures: &regex::Captures, cli: &slack::RtmClient| {
                if let Some(id) = captures.at(1) {
                    if let Some(user) = get_user_with_id(cli, id) {
                        return format!("@{}", user.name).to_owned()
                    }
                }
                captures.at(2).unwrap_or("").to_owned()
            })),
            (Regex::new(r"<([^!]\S+)>").unwrap(), Box::new(|captures: &regex::Captures, _| {
                captures.at(1).unwrap_or("").to_owned()
            })),
            (Regex::new(r"<!(\w+)\|?(\w+)?>").unwrap(), Box::new(|captures: &regex::Captures, _| {
                format!("<{}>", captures.at(2).or(captures.at(1)).unwrap_or("")).to_owned()
            })),
            (Regex::new(r":(\w+):").unwrap(), Box::new(|captures: &regex::Captures, _| {
                if let Some(&emoji) = EMOJIS.get(captures.at(1).unwrap()) {
                    emoji.to_owned()
                } else {
                    captures.at(0).unwrap().to_owned()
                }
            })),
        ];
    }

    let text = REPLACEMENTS.iter().fold(text.to_owned(), |text, &(ref re, replacement)| {
        re.replace_all(&text, replacement)
    });

    FUNCTIONS.iter().fold(text, |text, &(ref re, ref replacement)| {
        re.replace_all(&text, |captures: &regex::Captures| {
            replacement(captures, cli)
        })
    })
}

impl<'a> slack::EventHandler for SlackHandler<'a, SlackToIrc> {
    fn on_event(&mut self, cli: &mut slack::RtmClient, event: Result<&slack::Event, slack::Error>, _raw_json: &str) {
        match event {
            Ok(&slack::Event::Message(ref message)) => match message {
                &slack::Message::Standard { channel: Some(ref channel), user: Some(ref user), text: Some(ref text), .. } if user == self.user_id => {
                    lazy_static!{
                        static ref PM_RE: Regex = Regex::new(r"^(\S+):\s+(.+)").unwrap();
                    }

                    let text = parse_slack_text(&text, cli);

                    if text.starts_with("%") {
                        self.tx.send(SlackToIrc::Raw(text[1..].to_owned())).unwrap();
                        log_err(cli.send_message(channel, "_sent raw command_"));
                    } else if channel == self.bot_channel {
                        if let Some(captures) = PM_RE.captures(&text) {
                            self.tx.send(SlackToIrc::Message { to: captures.at(1).unwrap().to_owned(), msg: captures.at(2).unwrap().to_owned() }).unwrap();
                        } else {
                            log_err(cli.send_message(channel, "_no message sent, are you missing a `user: ` prefix?_"));
                        }
                    } else {
                        self.tx.send(SlackToIrc::Message { to: format!("#{}", get_channel_with_id(&cli, channel).unwrap().name), msg: text.clone() }).unwrap();
                    }
                },
                &slack::Message::MeMessage { ref channel, ref user, ref text, .. } if user == self.user_id => {
                    self.tx.send(SlackToIrc::MeMessage { to: channel.clone(), msg: text.clone() }).unwrap();
                },
                _ => (),
            },
            Ok(&slack::Event::PresenceChange { ref user, ref presence }) if user == self.user_id => {
                self.tx.send(SlackToIrc::Away(presence == "active")).unwrap();
            },
            Ok(&slack::Event::ChannelJoined { ref channel }) => {
                self.tx.send(SlackToIrc::Join(format!("#{}", channel.name))).unwrap();
            },
            Ok(&slack::Event::ChannelLeft { ref channel }) => {
                self.tx.send(SlackToIrc::Part(format!("#{}", get_channel_with_id(&cli, channel).unwrap().name))).unwrap();
            },
            _ => (),
        }
    }

    fn on_ping(&mut self, _cli: &mut slack::RtmClient) {}

    fn on_close(&mut self, _cli: &mut slack::RtmClient) {
        println!("Disconnected from Slack");
    }

    fn on_connect(&mut self, _cli: &mut slack::RtmClient) {
        println!("Connected to Slack");
    }
}

fn post_message(cli: &slack::RtmClient, token: &str, to: &str, text: &str, username: Option<&str>) {
    let to: &str = if to.starts_with("#") {
        &cli.get_channel_id(&to[1..]).unwrap()
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

fn get_member_channels(cli: &slack::RtmClient) -> Box<Iterator<Item = slack::Channel>> {
    Box::new(cli.get_channels().into_iter().filter(|c| c.is_member))
}

fn main() {
    let c: Arc<Config> = {
        let mut s: String = String::new();
        File::open(&"config.toml").and_then(|mut f| f.read_to_string(&mut s)).unwrap();
        toml::decode_str::<Config>(&s).unwrap().into()
    };

    let (slack_tx, slack_rx) = channel();
    let (irc_tx, irc_rx) = channel();

    let slack_thread = {
        let c = c.clone();
        thread::Builder::new().name("slack".to_owned()).spawn(move || {
            let mut cli = slack::RtmClient::new(&c.slack_token);
            cli.login().unwrap();

            let user_id = cli.get_user_id(&c.slack_user).unwrap().clone();
            let bot_channel = cli.im_open(&user_id).unwrap().channel.id;

            let recv_thread = {
                let c = c.clone();
                let bot_channel = bot_channel.clone();
                thread::Builder::new().name("slack_recv".to_owned()).spawn(move || {
                    let mut cli = slack::RtmClient::new(&c.slack_token);
                    let (client, rx) = cli.login().unwrap();

                    // auto-join channels the bot has been invited to
                    for channel in get_member_channels(&cli) {
                        irc_tx.send(SlackToIrc::Join(format!("#{}", channel.name))).unwrap();
                    }

                    let mut handler = SlackHandler {
                        tx: &irc_tx,
                        user_id: &user_id,
                        bot_channel: &bot_channel,
                    };

                    cli.run(&mut handler, client, rx).unwrap();
                }).unwrap()
            };

            for msg in slack_rx {
                match msg {
                    IrcToSlack::Message { to, from, msg } => {
                        let to: &str = if to.starts_with("#") {
                            match cli.get_channel_id(&to[1..]) {
                                Some(ref id) => id,
                                None => return,
                            }
                        } else {
                            &bot_channel
                        };

                        let from = from.as_ref().map(|s| s.as_ref());

                        post_message(&cli, &c.slack_token, to, &msg, from);
                    },
                    IrcToSlack::Topic { chan, topic } => {
                        cli.set_topic(&chan, &topic.unwrap_or("".to_owned())).unwrap();
                    },
                    IrcToSlack::Kick { by, chans, nicks, reason } => {
                        let by = &by.unwrap_or("server".to_owned());
                        let reason = &reason.unwrap_or("".to_owned());

                        for chan in chans.split(",") {
                            for nick in nicks.split(",") {
                                post_message(&cli, &c.slack_token, &chan, &format!("*{}* has kicked *{}* (_{}_)", by, nick, reason), None);
                            }
                        }
                    },
                    IrcToSlack::Join { nick, chans } => {
                        for chan in chans.split(",") {
                            post_message(&cli, &c.slack_token, &chan, &format!("*{}* has joined", nick), None);
                        }
                    },
                    IrcToSlack::Part { nick, chans, reason } => {
                        let reason = &reason.unwrap_or("".to_owned());

                        for chan in chans.split(",") {
                            post_message(&cli, &c.slack_token, &chan, &format!("*{}* has left (_{}_)", nick, reason), None);
                        }
                    },
                    IrcToSlack::Quit { nick, reason } => {
                        let reason = &reason.unwrap_or("".to_owned());

                        for chan_id in get_member_channels(&cli).map(|c| c.id) {
                            post_message(&cli, &c.slack_token, &chan_id, &format!("*{}* has quit (_{}_)", nick, reason), None);
                        }
                    },
                    IrcToSlack::Nick { old_nick, new_nick } => {
                        for chan_id in get_member_channels(&cli).map(|c| c.id) {
                            post_message(&cli, &c.slack_token, &chan_id, &format!("*{}* is now known as *{}*", old_nick, new_nick), None);
                        }
                    },
                    IrcToSlack::Mode { by, name, modes, params } => {
                        if name.starts_with("#") {
                            post_message(&cli, &c.slack_token, &name, &format!("*{}* sets mode *{}* on _{}_", by.unwrap_or("server".to_owned()), modes, params.unwrap_or(name.clone())), None);
                        }
                    },
                    IrcToSlack::Error(msg) => {
                        post_message(&cli, &c.slack_token, &bot_channel, &msg, None);
                    },
                }
            }

            recv_thread.join().unwrap();
        }).unwrap()
    };

    let irc_thread = {
        let c = c.clone();
        thread::Builder::new().name("irc".to_owned()).spawn(move || {
            let config = irc::client::data::Config {
                nickname: Some(c.irc_nick.to_owned()),
                server: Some(c.irc_server.to_owned()),
                use_ssl: Some(true),
                ..Default::default()
            };

            let server = IrcServer::from_config(config).unwrap();

            server.identify().unwrap();

            let server_thread = {
                let server = server.clone();
                thread::Builder::new().name("irc_server".to_owned()).spawn(move || {
                    for message in server.iter() {
                        let message = message.unwrap();
                        match message.command {
                            Command::ERROR(msg) => {
                                slack_tx.send(IrcToSlack::Error(msg)).unwrap();
                            },
                            Command::PRIVMSG(to, text) | Command::NOTICE(to, text) => {
                                lazy_static!{
                                static ref ACTION_RE: Regex = Regex::new("^\x01ACTION (.+)\x01$").unwrap();
                            }

                                if ACTION_RE.is_match(&text) {
                                    slack_tx.send(IrcToSlack::Message {
                                        to: to,
                                        from: message.prefix,
                                        msg: format!("_{}_", ACTION_RE.captures(&text).unwrap().at(1).unwrap())
                                    }).unwrap();
                                } else {
                                    slack_tx.send(IrcToSlack::Message { to: to, from: message.prefix, msg: text }).unwrap();
                                }
                            },
                            Command::TOPIC(channel, topic) => {
                                slack_tx.send(IrcToSlack::Topic { chan: channel, topic: topic }).unwrap();
                            },
                            Command::KICK(channels, users, reason) => {
                                slack_tx.send(IrcToSlack::Kick { by: message.prefix, chans: channels, nicks: users, reason: reason }).unwrap();
                            },
                            Command::JOIN(channels, _, _) => {
                                slack_tx.send(IrcToSlack::Join { nick: message.prefix.unwrap(), chans: channels }).unwrap();
                            },
                            Command::PART(channels, reason) => {
                                slack_tx.send(IrcToSlack::Part { nick: message.prefix.unwrap(), chans: channels, reason: reason }).unwrap();
                            },
                            Command::QUIT(reason) => {
                                slack_tx.send(IrcToSlack::Quit { nick: message.prefix.unwrap(), reason: reason }).unwrap();
                            },
                            Command::NICK(nick) => {
                                slack_tx.send(IrcToSlack::Nick { old_nick: message.prefix.unwrap(), new_nick: nick }).unwrap();
                            },
                            Command::MODE(name, modes, params) => {
                                slack_tx.send(IrcToSlack::Mode { by: message.prefix, name: name, modes: modes, params: params }).unwrap();
                            }
                            _ => (),
                        }
                    }
                }).unwrap()
            };

            for msg in irc_rx {
                match msg {
                    SlackToIrc::Message { to, msg } => {
                        server.send_privmsg(&to, &msg).unwrap();
                    },
                    SlackToIrc::MeMessage { to, msg } => {
                        server.send_privmsg(&to, &format!("\x01ACTION {}\x01", msg)).unwrap();
                    },
                    SlackToIrc::Raw(msg) => {
                        server.send(Command::Raw(msg, vec![], None)).unwrap();
                    },
                    SlackToIrc::Away(is_away) => {
                        server.send(Command::AWAY(Some(if is_away { " ".to_owned() } else { "".to_owned() }))).unwrap();
                    },
                    SlackToIrc::Join(chan) => {
                        server.send_join(&chan).unwrap();
                    },
                    SlackToIrc::Part(chan) => {
                        server.send(Command::PART(chan, None)).unwrap();
                    },
                }
            }

            server_thread.join().unwrap();
        }).unwrap()
    };

    slack_thread.join().unwrap();
    irc_thread.join().unwrap();
}
