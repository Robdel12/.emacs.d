# Modern Emacs Configuration

A modern, performant Emacs configuration focused on web development, featuring the latest completion frameworks and development tools.

If you'd like to use this config, simply clone this repo into `~/.emacs.d`.

If you make any changes, including adding snippets, features, or languages, feel free to contribute them back here!

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

- Search functionality uses `consult` with various backends. For best performance, install `ripgrep`: `brew install ripgrep`

- To see flycheck errors in JS, install `eslint`: `npm install -g eslint`

- For Ruby development install `rbenv` and the `solargraph` gem for LSP
  integration: `gem install solargraph`
- Common Ruby snippets live in `snippets/ruby-mode` for quick expansion.

- For GPG signing, pinentry is installed and automatically started. You'll have
  to add `allow-emacs-pinentry` and `allow-loopback-pinentry` to `.gnupg/gpg-agent.conf`
  then reload gpg-agent with `gpgconf --reload gpg-agent`.

## Features

### Modern Completion Framework

This configuration uses a modern completion stack for fast, fuzzy matching:

- **Vertico** - Fast, minimal completion UI
- **Orderless** - Flexible completion style (space-separated terms)
- **Consult** - Enhanced commands for searching and navigation
- **Marginalia** - Rich annotations for completion candidates
- **Embark** - Contextual actions on completion candidates

### Language Support

- **JavaScript/TypeScript** - LSP integration, web-mode for JSX/TSX
- **Ruby** - Full LSP support with Solargraph
- **Markdown/MDX** - GitHub-flavored markdown with live preview
- **Astro** - Component syntax highlighting
- **Web Technologies** - HTML, CSS, handlebars templates

### Development Tools

- **LSP Mode** - Language server integration for intelligent editing
- **Treemacs** - File explorer sidebar
- **Projectile** - Project-aware commands
- **Magit** - Excellent Git integration
- **Flycheck** - Real-time syntax checking
- **Company** - Text completion
- **GitHub Copilot** - AI-powered code suggestions

## Key Bindings

### Global Bindings

| Key | Command | Description |
|-----|---------|-------------|
| `C-s` | `consult-line` | Search within current buffer |
| `C-;` | `embark-act` | Contextual actions on item at point |
| `M-x` | `execute-extended-command` | Command palette (enhanced with completion) |
| `C-x C-f` | `find-file` | Find file (enhanced with completion) |
| `C-x b` | `consult-buffer` | Switch buffers (enhanced with completion) |
| `C-x r b` | `consult-bookmark` | Jump to bookmark |
| `C-c u` | `consult-focus-lines` | Focus matching lines |
| `M-g g` | `consult-goto-line` | Go to line number |
| `M-g i` | `consult-imenu` | Navigate to definitions |
| `C-c h` | `consult-history` | Search command history |
| `C-c m` | `consult-mode-command` | Search mode-specific commands |

### Projectile (Project Management)

| Key | Command | Description |
|-----|---------|-------------|
| `s-p` or `C-c p` | `projectile-command-map` | Projectile command prefix |
| `C-c p f` | `projectile-find-file` | Find file in project |
| `C-c p p` | `projectile-switch-project` | Switch to another project |
| `C-c p b` | `consult-project-buffer` | Switch to project buffer |
| `C-c p s r` | `projectile-replace` | Search and replace in project |

### LSP (Language Server)

| Key | Command | Description |
|-----|---------|-------------|
| `C-c l` | `lsp-keymap-prefix` | LSP command prefix |
| `C-c l r` | `lsp-rename` | Rename symbol |
| `C-c l f` | `lsp-format-buffer` | Format current buffer |
| `C-c l a` | `lsp-execute-code-action` | Execute code action |
| `C-c l g g` | `lsp-find-definition` | Go to definition |
| `C-c l g r` | `lsp-find-references` | Find references |

### Magit (Git)

| Key | Command | Description |
|-----|---------|-------------|
| `C-x g` | `magit-status` | Open Magit status buffer |
| `C-c g` | `magit-file-dispatch` | Git actions for current file |

### Editing

| Key | Command | Description |
|-----|---------|-------------|
| `C-c c` | `comment-line` | Comment/uncomment current line |
| `C-a` | Smart beginning of line | Move to indentation or line start |
| `C-w` | `kill-region-or-line` | Kill region or current line |
| `M-w` | `copy-region-or-line` | Copy region or current line |
| `C-S-<backspace>` | `kill-whole-line` | Delete entire line |

### Window Management

| Key | Command | Description |
|-----|---------|-------------|
| `M-o` | `other-window` | Switch to next window |
| `C-x 1` | `delete-other-windows` | Make current window fill frame |
| `C-x 2` | `split-window-below` | Split window horizontally |
| `C-x 3` | `split-window-right` | Split window vertically |

### Embark Actions

Press `C-;` on any completion candidate or text to see contextual actions:

- On files: Open, rename, delete, copy path
- On buffers: Switch, kill, save
- On symbols: Find definition, find references
- On URLs: Open in browser
- On regions: Search web, translate, etc.

### Completion Tips

- **Orderless matching**: Type space-separated terms in any order (`pro buf` matches "project-buffer")
- **Regexp support**: Use `.*` for wildcards, `^term` for prefix matching
- **Category filtering**: In `consult-buffer`, type `b ` for buffers only, `f ` for files only
- **Preview**: Many consult commands show live preview while navigating candidates
- **Annotations**: Marginalia shows helpful context for all completion candidates
