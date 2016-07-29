# slack-irc-rust
Based on https://github.com/ekmartin/slack-irc, but in Rust.

There must be a `config.toml` in the same directory as the executable, see `example_config.toml` for an example.

## Features

- Send raw commands by prefixing your message with `%` (e.g. `%PRIVMSG #test testing` in any channel)
- Send "/msg" messages directly to users by IMing the bot with their name prefixed (e.g. `someones_nick: hi` in the IM channel)
- Receive "/msg" messages from users in the same IM channel
- Invite the bot to a Slack channel to join the IRC channel of the same name (kick the bot to part)
- Native slack `/me` messages work in IRC
- Slack active/away status sent to IRC
- Slack channel topic set to each IRC channel topic
