# echobot
![botlogo](https://i.ibb.co/CWq976m/Pin-Clipart-com-cute-heart-clipart-60934.png)

This is a Haskell implementation of echo bot for Telegram and Vk messenger. This bot merely echoes every message it gets using long polling. Under the hood it uses [`freer`](https://hackage.haskell.org/package/freer-simple) to define all necessary effects in a pure way and interpreters for them, [`req`](https://hackage.haskell.org/package/req-3.9.0) to make HTTPS requests, `template-haskell` for automatic derivation of `ToJSON` and `FromJSON` typeclasses.

Supported commands:
- `/start` or `/help`
- `/repeat`

## Getting started
To run this bot, fill out config file using `config-template.json`. Compile this project using `stack build`, hit `stack run` and enjoy :)
You can set minimal allowed logging level and destination (`stdout`, `file`, `both` or `none`) in `config.json`.

## Documentation
To get better documentation use `stack haddock`.
