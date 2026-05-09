;;; gitmoji.el --- Gitmoji selector.
;;; Commentary:
;;; Code:

(defvar gitmojis-list
  '(("Improve structure / format of the code." ":art:" "🎨")
    ("Improve performance." ":zap:" "⚡")
    ("Remove code or files." ":fire:" "🔥")
    ("Fix a bug." ":bug:" "🐛")
    ("Critical hotfix." ":ambulance:" "🚑")
    ("Introduce new features." ":sparkles:" "✨")
    ("Add or update documentation." ":memo:" "📝")
    ("Deploy stuff." ":rocket:" "🚀")
    ("Add or update the UI and style files." ":lipstick:" "💄")
    ("Begin a project." ":tada:" "🎉")
    ("Add, update, or pass tests." ":white_check_mark:" "✅")
    ("Fix security issues." ":lock:" "🔒")
    ("Release / Version tags." ":bookmark:" "🔖")
    ("Fix compiler / linter warnings." ":rotating_light:" "🚨")
    ("Work in progress." ":construction:" "🚧")
    ("Fix CI Build." ":green_heart:" "💚")
    ("Downgrade dependencies." ":arrow_down:" "⬇")
    ("Upgrade dependencies." ":arrow_up:" "⬆")
    ("Pin dependencies to specific versions." ":pushpin:" "📌")
    ("Add or update CI build system." ":construction_worker:" "👷")
    ("Add or update analytics or track code." ":chart_with_upwards_trend:" "📈")
    ("Refactor code." ":recycle:" "♻")
    ("Add a dependency." ":heavy_plus_sign:" "➕")
    ("Remove a dependency." ":heavy_minus_sign:" "➖")
    ("Add or update configuration files." ":wrench:" "🔧")
    ("Add or update development scripts." ":hammer:" "🔨")
    ("Internationalization and localization." ":globe_with_meridians:" "🌐")
    ("Fix typos." ":pencil2:" "✏")
    ("Write bad code that needs to be improved." ":poop:" "💩")
    ("Revert changes." ":rewind:" "⏪")
    ("Merge branches." ":twisted_rightwards_arrows:" "🔀")
    ("Add or update compiled files or packages." ":package:" "📦")
    ("Update code due to external API changes." ":alien:" "👽")
    ("Move or rename resources (e.g.: files, paths, routes)." ":truck:" "🚚")
    ("Add or update license." ":page_facing_up:" "📄")
    ("Introduce breaking changes." ":boom:" "💥")
    ("Add or update assets." ":bento:" "🍱")
    ("Improve accessibility." ":wheelchair:" "♿")
    ("Add or update comments in source code." ":bulb:" "💡")
    ("Write code drunkenly." ":beers:" "🍻")
    ("Add or update text and literals." ":speech_balloon:" "💬")
    ("Perform database related changes." ":card_file_box:" "🗃")
    ("Add or update logs." ":loud_sound:" "🔊")
    ("Remove logs." ":mute:" "🔇")
    ("Add or update contributor(s)." ":busts_in_silhouette:" "👥")
    ("Improve user experience / usability." ":children_crossing:" "🚸")
    ("Make architectural changes." ":building_construction:" "🏗")
    ("Work on responsive design." ":iphone:" "📱")
    ("Mock things." ":clown_face:" "🤡")
    ("Add or update an easter egg." ":egg:" "🥚")
    ("Add or update a .gitignore file." ":see_no_evil:" "🙈")
    ("Add or update snapshots." ":camera_flash:" "📸")
    ("Perform experiments." ":alembic:" "⚗")
    ("Improve SEO." ":mag:" "🔍")
    ("Add or update types." ":label:" "🏷")
    ("Add or update seed files." ":seedling:" "🌱")
    ("Add, update, or remove feature flags." ":triangular_flag_on_post:" "🚩")
    ("Catch errors." ":goal_net:" "🥅")
    ("Add or update animations and transitions." ":dizzy:" "💫")
    ("Deprecate code that needs to be cleaned up." ":wastebasket:" "🗑")
    ("Work on code related to authorization, roles and permissions." ":passport_control:" "🛂")
    ("Simple fix for a non-critical issue." ":adhesive_bandage:" "🩹")
    ("Data exploration/inspection." ":monocle_face:" "🧐")
    ("Remove dead code." ":coffin:" "⚰")
    ("Add a failing test." ":test_tube:" "🧪")
    ("Add or update business logic" ":necktie:" "👔"))
  "Generated with:
 curl -s https://raw.githubusercontent.com/carloscuesta/gitmoji/master/src/data/gitmojis.json | \\
 jq -r \\='.gitmojis | map([\"(\", ([.description, .code, .emoji] | join(\"\\\" \\\"\")), \")\"] | join(\"\\\"\")) | join(\"\\n\")\\='")

(defvar gitmoji--insert-utf8-emoji t
  "When nil, inserts a github-style emoji.
Example: :zap: instead of \u26a1.
Default: t.")

(defvar gitmoji--display-utf8-emoji t
  "When nil, does not use utf8 in the emoji list.
Default: t.")

(defun gitmoji-insert--candidates ()
  "Return a list of gitmojis to choose from."
  (mapcar (lambda (g)
            (let ((description (car g))
                  (shortcode (cadr g))
                  (utf8 (caddr g)))
              (cons
               (concat
                (if gitmoji--display-utf8-emoji utf8 shortcode)
                " \u2014 "
                description)
               g)))
          gitmojis-list))

(defun gitmoji-insert--action (g)
  "Insert a gitmoji, G, into the current buffer."
  (let ((shortcode (cadr g))
        (utf8 (caddr g)))
    (insert (if gitmoji--insert-utf8-emoji utf8 shortcode) " ")))

(defun gitmoji-insert ()
  "Choose a gitmoji and insert it in the current buffer."
  (interactive)
  (let* ((candidates (gitmoji-insert--candidates))
         (choice (completing-read "Choose a gitmoji: " candidates nil t)))
    (gitmoji-insert--action (cdr (assoc choice candidates)))))

;;;###autoload
(define-minor-mode gitmoji-commit-mode
  "Toggle gitmoji-commit mode."
  :global t
  :init-value nil
  :lighter " gitmoji"
  (if gitmoji-commit-mode
      (add-hook 'git-commit-setup-hook 'gitmoji-insert)
    (remove-hook 'git-commit-setup-hook 'gitmoji-insert)))

(provide 'gitmoji)
;;; gitmoji.el ends here
