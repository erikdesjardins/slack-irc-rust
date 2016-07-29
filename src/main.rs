extern crate irc;
extern crate hyper;
extern crate slack;
#[macro_use] extern crate lazy_static;
extern crate regex;

use std::collections::HashMap;
use std::thread;
use std::sync::mpsc::{Sender, channel};
use std::default::Default;
use irc::client::prelude::*;
use regex::Regex;

#[derive(Debug)]
enum SlackToIrc {
    Message { to: String, msg: String },
    MeMessage { to: String, msg: String },
    Raw(String),
    Away(bool),
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
    chan: &'a Sender<T>,
}

fn get_channel_with_id(id: &str, channels: Vec<slack::Channel>) -> Option<slack::Channel> {
    channels.into_iter().find(|channel| {
        channel.id == id
    })
}

fn get_user_with_id(id: &str, users: Vec<slack::User>) -> Option<slack::User> {
    users.into_iter().find(|user| {
        user.id == id
    })
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
                    if let Some(channel) = get_channel_with_id(id, cli.get_channels()) {
                        return format!("#{}", channel.name).to_owned()
                    }
                }
                captures.at(2).unwrap_or("").to_owned()
            })),
            (Regex::new(r"<@(U\w+)\|?(\w+)?>").unwrap(), Box::new(|captures: &regex::Captures, cli: &slack::RtmClient| {
                if let Some(id) = captures.at(1) {
                    if let Some(user) = get_user_with_id(id, cli.get_users()) {
                        return format!("@{}", user.name).to_owned()
                    }
                }
                captures.at(2).unwrap_or("").to_owned()
            })),
            (Regex::new(r"<(?!!)(\S+)>").unwrap(), Box::new(|captures: &regex::Captures, _| {
                captures.at(1).unwrap_or("").to_owned()
            })),
            (Regex::new(r"<!(\w+)\|?(\w+)?>").unwrap(), Box::new(|captures: &regex::Captures, _| {
                format!("<{}>", captures.at(2).or(captures.at(1)).unwrap_or("")).to_owned()
            })),
            (Regex::new(r"\:(\w+)\:").unwrap(), Box::new(|captures: &regex::Captures, _| {
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
        let slack_user = &"TEMP_REPLACE_ME";
        let bot_channel = &"TEMP_REPLACE_ME";
        match event {
            Ok(&slack::Event::Message(ref message)) => match message {
                &slack::Message::Standard { channel: Some(ref channel), user: Some(ref user), text: Some(ref text), .. } if user == slack_user => {
                    lazy_static!{
                        static ref PM_RE: Regex = Regex::new(r"^(\S+):\s+(.+)").unwrap();
                    }

                    let text = parse_slack_text(&text, cli);

                    if text.starts_with("%") {
                        self.chan.send(SlackToIrc::Raw(text[1..].to_owned())).unwrap();
                        cli.send_message(channel, "_sent raw command_").unwrap();
                    } else if channel == bot_channel {
                        if let Some(captures) = PM_RE.captures(&text) {
                            self.chan.send(SlackToIrc::Message { to: captures.at(1).unwrap().to_owned(), msg: captures.at(2).unwrap().to_owned() }).unwrap();
                        } else {
                            cli.send_message(channel, "_no message sent, are you missing a `user: ` prefix?_").unwrap();
                        }
                    } else {
                        self.chan.send(SlackToIrc::Message { to: channel.clone(), msg: text.clone() }).unwrap();
                    }
                },
                &slack::Message::MeMessage { ref channel, ref user, ref text, .. } if user == slack_user => {
                    self.chan.send(SlackToIrc::MeMessage { to: channel.clone(), msg: text.clone() }).unwrap();
                },
                _ => (),
            },
            Ok(&slack::Event::PresenceChange { ref user, ref presence }) if user == slack_user => {
                self.chan.send(SlackToIrc::Away(presence == "active")).unwrap();
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

fn post_message(cli: &slack::RtmClient, token: &str, to: &str, text: &str, username: Option<&str>) -> Result<slack::api::chat::PostMessageResponse, slack::Error> {
    let to: &str = if to.starts_with("#") {
        &cli.get_channel_id(&to[1..]).unwrap()
    } else {
        to
    };

    let client = hyper::Client::new();
    let icon_url = username.map(|username| format!("http://api.adorable.io/avatars/48/{}.png", username));
    let icon_url = icon_url.as_ref().map(|s| s.as_ref());
    slack::api::chat::post_message(
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
    ).map_err(|e| e.into())
}

fn main() {
    let (slack_tx, slack_rx) = channel();
    let (irc_tx, irc_rx) = channel();

    let slack_thread = thread::Builder::new().name("slack".to_owned()).spawn(move || {
        let slack_token = "SLACK_TOKEN_REPLACE_ME";

        let recv_thread = thread::Builder::new().name("slack_recv".to_owned()).spawn(move || {
            let mut handler = SlackHandler { chan: &irc_tx };
            let mut cli = slack::RtmClient::new(&slack_token);
            cli.login_and_run(&mut handler).unwrap();
        }).unwrap();

        let mut cli = slack::RtmClient::new(&slack_token);
        cli.login().unwrap();

        let all_channels = ["ALL_CHANNELS_REPLACE_ME"];
        let dm_channel = "SLACK_DM_CHANNEL_TODO_REPLACE_ME";

        for msg in slack_rx {
            match msg {
                IrcToSlack::Message { to, from, msg } => {
                    let to: &str = if to.starts_with("#") {
                        dm_channel
                    } else {
                        &to
                    };

                    let from = from.as_ref().map(|s| s.as_ref());

                    post_message(&cli, &slack_token, to, &msg, from).unwrap();
                },
                IrcToSlack::Topic { chan, topic } => {
                    cli.set_topic(&chan, &topic.unwrap_or("".to_owned())).unwrap();
                },
                IrcToSlack::Kick { by, chans, nicks, reason } => {
                    let by = &by.unwrap_or("server".to_owned());
                    let reason = &reason.unwrap_or("".to_owned());

                    for chan in chans.split(",") {
                        for nick in nicks.split(",") {
                            post_message(&cli, &slack_token, &chan, &format!("*{}* has kicked *{}* (_{}_)", by, nick, reason), None).unwrap();
                        }
                    }
                },
                IrcToSlack::Join { nick, chans } => {
                    for chan in chans.split(",") {
                        post_message(&cli, &slack_token, &chan, &format!("*{}* has joined", nick), None).unwrap();
                    }
                },
                IrcToSlack::Part { nick, chans, reason } => {
                    let reason = &reason.unwrap_or("".to_owned());

                    for chan in chans.split(",") {
                        post_message(&cli, &slack_token, &chan, &format!("*{}* has left (_{}_)", nick, reason), None).unwrap();
                    }
                },
                IrcToSlack::Quit { nick, reason } => {
                    let reason = &reason.unwrap_or("".to_owned());

                    for chan in &all_channels {
                        post_message(&cli, &slack_token, &chan, &format!("*{}* has quit (_{}_)", nick, reason), None).unwrap();
                    }
                },
                IrcToSlack::Nick { old_nick, new_nick } => {
                    for chan in &all_channels {
                        post_message(&cli, &slack_token, &chan, &format!("*{}* is now known as *{}*", old_nick, new_nick), None).unwrap();
                    }
                },
                IrcToSlack::Mode { by, name, modes, params } => {
                    if name.starts_with("#") {
                        post_message(&cli, &slack_token, &name, &format!("*{}* sets mode *{}* on _{}_", by.unwrap_or("server".to_owned()), modes, params.unwrap_or(name.clone())), None).unwrap();
                    }
                },
                IrcToSlack::Error(msg) => {
                    post_message(&cli, &slack_token, im_channel, &msg, None).unwrap();
                },
            }
        }

        recv_thread.join().unwrap();
    }).unwrap();

    let irc_thread = thread::Builder::new().name("irc".to_owned()).spawn(move || {
        let config = Config {
            nickname: Some("NICK_REPLACE_ME".to_owned()),
            server: Some("SERVER_REPLACE_ME".to_owned()),
            channels: Some(vec!["CHANNELS_REPLACE_ME".to_owned()]),
            port: Some(6697),
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
            }
        }

        server_thread.join().unwrap();
    }).unwrap();

    slack_thread.join().unwrap();
    irc_thread.join().unwrap();
}
