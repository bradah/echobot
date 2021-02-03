# echobot
![botlogo](https://i.ibb.co/CWq976m/Pin-Clipart-com-cute-heart-clipart-60934.png)

This is a Haskell implementation of echo bot for Telegram messenger. This bot merely echoes every message it gets using long polling. Under the hood it uses [`servant`](https://github.com/haskell-servant/servant) to describe [Telegram API](https://core.telegram.org/bots/api), `mtl`-style `Bot` monad, `generics` and `template-haskell` for automatic derivation of `ToJSON` and `FromJSON` typeclasses and wonderful [`co-log`](https://github.com/kowainik/co-log) library for logging functionality.

This bot supports following message types:
- plain text
- stickers
- photos
- voice messages
- videos
- videonotes
- music
- documents
- animation

And couple of commands:
- /start or /help
- /repeat

## Getting started
To run this bot, write your bot token to `echobot.conf`. Compile this project using `stack build`, hit `stack exec echobot-exe` and enjoy :)
You can set minimal allowed logging level and destination (`stdout`, `Handle`, both or none) in `Main.hs`.
## Documentation
To get better documentation use `stack haddock`.
