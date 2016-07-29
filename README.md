# slack-irc-rust
Based on https://github.com/ekmartin/slack-irc, but in Rust.

There must be a `config.toml` in the same directory as the executable, see `example_config.toml` for an example.

## Features

- Send raw commands by prefixing your message with `%` (e.g. `%PRIVMSG #test testing` in any channel)
- Send "/msg" messages by IMing the bot with their nick prefixed (e.g. `someones_nick: hi` in the IM channel)
- Receive "/msg" messages in the same IM channel
- Invite the bot to a Slack channel to join the IRC channel of the same name (and kick the bot to part)
- Native slack `/me` messages work in IRC
- Slack active/away status sent to IRC
- Slack channel topic synced to/from IRC
