# My Emacs config

Fast, modern Emacs tuned for web development (JS/TS, HTML/CSS, Markdown, Ruby), with a clean UI,
powerful search, and unified formatting.

Clone into `~/.emacs.d` and start Emacs. An Emacs server auto-starts; set `$EDITOR` to `emacsclient`
if you like.

## Highlights

- Modeline: `doom-modeline` with icons, LSP/VC/diagnostics.
- Completion: Vertico + Orderless + Corfu + Cape, plus Marginalia, Embark, and Consult.
- Floating prompts: minibuffer/command palette appears top‑center via `mini-frame`.
- Search: `consult-ripgrep` bound to `s-f` (project root via Projectile).
- Formatting: Apheleia global on‑save formatting (prettierd/prettier, eslint_d, RuboCop, Black). EditorConfig respected.
- LSP: `lsp-mode` with consult integration; diagnostics via Flycheck.
- UI polish: base16 theme, pixel‑precise resize, smooth scrolling, `hl-line`, context menu.
- Files: Treemacs with Nerd Icons.
- Navigation: Avy jump (`jj` char, `jk` word, `jl` line), multiple cursors, expand-region.

## Prerequisites (recommended)

- macOS: `brew install ripgrep ispell` (search and spell‑check).
- Icons: install a Nerd Font (for modeline + Treemacs icons).
- Node tooling: `npm i -g @fsouza/prettierd eslint_d` (faster format/lint). Project‑local binaries are preferred when present.
- Ruby: `gem install solargraph rubocop`; prefer Bundler in projects.
- Python: `pip install black`.
- Emoji: if your Emacs build doesn’t support color fonts, adjust `lisp/init-emojis.el` or use `emacs-plus`.

## Key Bindings

- Search/project
  - `s-f`: `consult-ripgrep` (project ripgrep)
  - `M-x`: command palette (top‑center floating)
  - `C-s`: `consult-line` (in‑buffer search)
- Projectile
  - `s-p` / `C-c p`: Projectile prefix (find file, switch project, etc.)
- Jump/navigation
  - `jj`: `avy-goto-char`, `jk`: `avy-goto-word-1`, `jl`: `avy-goto-line`
  - `M-o`: `ace-window` (pick window)
- Git
  - `C-x g`: `magit-status`
- LSP
  - `C-c l`: LSP prefix (rename, code actions, etc.)
- Embark
  - `C-;`: action menu for the thing at point/candidate under cursor

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
- Notable modules:
  - `lisp/init-modeline.el` – doom‑modeline
  - `lisp/init-minibuffer.el` – Vertico/Orderless/Consult + Corfu/Cape
  - `lisp/init-float-minibuffer.el` – top‑center floating prompts
  - `lisp/init-format.el` – Apheleia unified formatting
  - `lisp/init-lsp.el` – LSP + UI
  - `lisp/init-treemacs.el` – Treemacs + Nerd Icons
  - `lisp/init-ui.el` – UI polish

## Tips

- Use space‑separated terms with Orderless (e.g., `proj buf`).
- Embark actions work everywhere: try `C-;` during any completion.
- Project roots are resolved via Projectile for Consult commands.

## Misc

- Session state like `recentf`, `desktop/`, and `places` is ignored in git.
- Contributions and PRs to improve languages and ergonomics are welcome.
