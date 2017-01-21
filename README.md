# slack-irc-rust

Rust port of [erikdesjardins/slack-irc](https://github.com/erikdesjardins/slack-irc), which was forked from [ekmartin/slack-irc](https://github.com/ekmartin/slack-irc) (and modified for a single user).

## Usage

- Compile from source with `cargo build --release` (if you don't have a Rust toolchain installed, use [rustup](https://github.com/rust-lang-nursery/rustup.rs))
- Run with `./slack_irc_rust path/to/config.toml` (defaults to `config.toml` in the CWD)
  - see `example_config.toml`

## Features

- Send raw commands by prefixing your message with `%` (e.g. `%PRIVMSG #test testing` in any channel)
- Send "/msg" messages by IMing the bot with their nick prefixed (e.g. `someones_nick: hi` in the IM channel)
- Receive "/msg" messages in the same IM channel
- Invite the bot to a Slack channel to join the IRC channel of the same name (and kick the bot to part)
- Native slack `/me` messages work in IRC
- Slack active/away status sent to IRC
- Slack channel topic synced to/from IRC
