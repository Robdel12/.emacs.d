# My own Emacs config

If you'd like to use my config, simply clone this repo into `~/.emacs.d`.

If you make any changes, including adding snippets, features, or languages, feel
free to contribute them back here!

### Prerequisites

This list is probably not definitive. If you find you need to install other
things to get the config to work properly, please create an issue so I can add
it to the list.

- An Emacs server is auto-started when one is not already running, update your
  `$EDITOR` environment variable to `emacsclient` (optionally with `-c`).

- I use [Fira Code](https://github.com/tonsky/FiraCode) with ligatures and
  Operator Mono for cursive keywords. The config for these is only initialized
  when the fonts exist.

- [Select GitHub-style emojis](https://gitmoji.carloscuesta.me/) are transformed
  into real unicode emojis. If using official Emacs (without multi-color font
  support), [update this line](https://github.com/wwilsman/emacs.d/blob/master/lisp/init-emojis.el#L12-L13).
  Or install [`emacs-plus`](https://github.com/d12frosted/homebrew-emacs-plus)
  which enables multi-color fonts, a.k.a. emoji fonts.

- To use spell-checking, install `ispell`: `brew install ispell`

- To use [prettier](https://prettier.io/), install it: `npm install -g prettier`

- To use ag, install `the_silver_searcher`: `brew install
  the_silver_searcher`

- To see flycheck errors in JS, install `eslint`: `npm install -g eslint`

- For Ruby development install `rbenv` and the `solargraph` gem for LSP
  integration: `gem install solargraph`
- Common Ruby snippets live in `snippets/ruby-mode` for quick expansion.

- For GPG signing, pinentry is installed and automatically started. You'll have
  to add `allow-emacs-pinentry` and `allow-loopback-pinentry` to `.gnupg/gpg-agent.conf`
  then reload gpg-agent with `gpgconf --reload gpg-agent`.
