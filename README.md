# My Emacs config

Fast, modern Emacs tuned for web development (JS/TS, HTML/CSS, Docker, APIs, Markdown, Ruby), with a clean UI,
powerful search, unified formatting, and comprehensive development tools.

Clone into `~/.emacs.d` and start Emacs. An Emacs server auto-starts; set `$EDITOR` to `emacsclient`
if you like.

## Highlights

- **Modeline**: `doom-modeline` with icons, LSP/VC/diagnostics.
- **Completion**: Vertico + Orderless + Corfu + Cape, plus Marginalia, Embark, and Consult.
- **Floating prompts**: minibuffer/command palette appears top‑center via `mini-frame`.
- **Search**: `consult-ripgrep` bound to `s-f` (project root via Projectile).
- **Formatting**: Apheleia global on‑save formatting (prettierd/prettier, eslint_d, RuboCop, Black). EditorConfig respected.
- **LSP**: `lsp-mode` with consult integration; diagnostics via Flycheck.
- **UI polish**: Modern coding fonts (JetBrains Mono), base16 theme, pixel‑precise resize, smooth scrolling.
- **Files**: Treemacs with Nerd Icons.
- **Navigation**: Avy jump (`jj` char, `jk` word, `jl` line), multiple cursors, expand-region.
- **Git/GitHub**: Magit + Forge for PR/issue management directly in Emacs.
- **Web Development**: NPM integration, Node.js REPL, REST client for API testing.
- **Containers**: Docker management and file editing in containers.
- **Remote Editing**: TRAMP for SSH file editing and remote project support.
- **Modern Undo**: Vundo with visual undo tree (replaces undo-tree).

## Prerequisites (recommended)

- **System**: `brew install ripgrep ispell` (search and spell‑check).
- **Fonts**: Install JetBrains Mono: `brew install font-jetbrains-mono` (best coding font with ligatures).
- **Icons**: Install a Nerd Font (for modeline + Treemacs icons).
- **Node.js**: `npm i -g @fsouza/prettierd eslint_d typescript-language-server` (faster format/lint/LSP).
- **Ruby**: `gem install solargraph rubocop`; prefer Bundler in projects.
- **Python**: `pip install black`.
- **Docker**: Install Docker Desktop for container management features.
- **GitHub**: Set up personal access token for Forge (PR/issue management).
- **Emoji**: If your Emacs build doesn't support color fonts, adjust `lisp/init-emojis.el` or use `emacs-plus`.

## Key Bindings

### Search/Project
- `s-f`: `consult-ripgrep` (project ripgrep)
- `M-x`: command palette (top‑center floating)
- `C-s`: `consult-line` (in‑buffer search)
- `s-p` / `C-c p`: Projectile prefix (find file, switch project, etc.)

### Navigation/Editing
- `jj`: `avy-goto-char`, `jk`: `avy-goto-word-1`, `jl`: `avy-goto-line`
- `uu`: Vundo visual undo tree (`C-n`/`C-p` to navigate)
- `M-o`: `ace-window` (pick window)
- `C-;`: Embark action menu for thing at point

### Git/GitHub
- `C-x g`: `magit-status`
- GitHub workflow: Use browser or `M-x browse-url-at-point` on URLs in Magit

### Web Development
- `C-c n r`: Run NPM script
- `C-c n i`: NPM install packages
- `C-c C-z`: Node.js REPL (in JS files)
- `C-c C-c`: Send HTTP request (in `.http` files)

### Docker
- `C-c d`: Docker management interface

### Remote Files (TRAMP)
- `C-c t d`: Open Dired on Pi
- `C-c t p`: Connect to Pi (interactive prompt)
- `C-c t h`: Return to local home directory
- `C-c t b`: Show TRAMP debug buffer

### LSP
- `C-c l`: LSP prefix (rename, code actions, etc.)

## Formatting

Apheleia runs on save with sensible defaults:

- JS/TS/TSX/JSON/CSS/SCSS/Markdown/YAML → `prettierd` (or `prettier`) and `eslint_d` when
  appropriate.
- Ruby → `rubocop` (via Bundler when available).
- Python → `black`.

Disable per buffer with `M-x apheleia-mode` or customize `apheleia-mode-alist`.

## UI Notes

- Floating minibuffer: uses `mini-frame`. If native macOS fullscreen hides child frames, try
  maximized (not fullscreen) or ask for a `vertico-posframe` setup.
- Icons depend on a Nerd Font and `nerd-icons`. Install a Nerd Font in your OS and select it in your
  Emacs font settings if necessary.

## Structure

- `init.el` – entrypoint that requires the `lisp/init-*.el` modules
- `early-init.el` – startup performance optimizations

### Core Modules
- `lisp/init-ui.el` – UI polish, fonts, smooth scrolling
- `lisp/init-modeline.el` – doom‑modeline
- `lisp/init-minibuffer.el` – Vertico/Orderless/Consult + Corfu/Cape
- `lisp/init-float-minibuffer.el` – top‑center floating prompts
- `lisp/init-editing.el` – editing enhancements, vundo, multiple cursors

### Language Support
- `lisp/init-javascript.el` – JS/Node.js + NPM integration
- `lisp/init-web.el` – HTML/CSS/JSX with Emmet
- `lisp/init-ruby.el` – Ruby + RuboCop
- `lisp/init-markdown.el` – Markdown editing
- `lisp/init-lsp.el` – Language Server Protocol

### Development Tools
- `lisp/init-vcs.el` – Git (Magit) + GitHub (Forge)
- `lisp/init-http.el` – REST client for API testing
- `lisp/init-docker.el` – Docker management and container editing
- `lisp/init-tramp.el` – Remote file editing via SSH
- `lisp/init-format.el` – Apheleia unified formatting
- `lisp/init-projectile.el` – Project management
- `lisp/init-treemacs.el` – File tree with icons

## New Features Guide

### REST Client (API Testing)
Create `.http` files to test APIs directly in Emacs:
```http
GET https://api.github.com/user
Authorization: token YOUR_TOKEN

###

POST https://jsonplaceholder.typicode.com/posts
Content-Type: application/json

{
  "title": "Test Post",
  "body": "Testing API",
  "userId": 1
}
```
Use `C-c C-c` to send requests.

### GitHub Integration
**Note**: Forge package disabled due to compatibility issues.

**GitHub Workflow:**
- Use Magit for all Git operations (`C-x g`)
- For GitHub PRs/issues: Use `M-x browse-url-at-point` on GitHub URLs in Magit
- Or use browser-based GitHub workflow
- Alternative: Install GitHub CLI (`brew install gh`) for terminal-based GitHub operations

### NPM Integration
- `C-c n r`: Interactive script runner (shows all package.json scripts)
- `C-c n i`: Install packages with completion
- `C-c n t`: Run tests

### Docker
- `C-c d`: Docker management (images, containers, networks)
- Edit files in containers: `/docker:container_name:/path/to/file`

### Remote File Editing (TRAMP)
Edit files on remote servers via SSH:
- `C-c t d`: Quick connect to Pi
- `C-c t p`: Interactive connection (custom user/host/path)
- `C-c t h`: Return to local filesystem
- Manual: `C-x C-f /sshx:user@host:/path/to/file`

Works with Dired for remote directory browsing and Projectile for remote projects. EditorConfig automatically disabled for remote files to prevent timeouts.

### Visual Undo (Vundo)
- `uu` or `C-x u`: Open visual undo tree
- `C-n`/`C-p`: Navigate, `RET`: Apply, `q`: Quit

## Tips

- Use space‑separated terms with Orderless (e.g., `proj buf`).
- Embark actions work everywhere: try `C-;` during any completion.
- Project roots are resolved via Projectile for Consult commands.
- JetBrains Mono font provides excellent ligatures for `=>`, `!=`, etc.

## Misc

- Session state like `recentf`, `desktop/`, and `places` is ignored in git.
- Contributions and PRs to improve languages and ergonomics are welcome.
