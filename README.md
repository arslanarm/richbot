# RichBot

This project's main goal is to be able to get faster and simpler access to 24u data about products in the vending machines, leveraging the simplicity of telegram bots.

## Prerequisites

* Cabal

## Environmental Variables for running

1) USERNAME - username in 24u app
2) PASSWORD - password in 24u app
3) DEVID - android device id
4) ADDRESS - address of the 24u API, use - https://app.24-u.co.uk
5) TELEGRAM_TOKEN - token of the telegram bot

## Running

```bash
# Using cabal
cabal run

# Using executable
cabal build

./dist-newstyle/build/.../richbot/richbot
```