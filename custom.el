(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-map (ansi-color-make-color-map) t)
 '(appt-display-duration 30)
 '(appt-display-format (quote window))
 '(browse-url-browser-function (quote browse-url-generic))
 '(browse-url-generic-program "browser-nw")
 '(clean-buffer-list-kill-buffer-names
   (quote
    ("*Help*" "*Apropos*" "*Man " "*Buffer List*" "*Compile-Log*" "*info*" "*vc*" "*vc-diff*" "*diff*" "*Org Agenda*")))
 '(clean-buffer-list-kill-never-buffer-names
   (quote
    ("*scratch*" "*Messages*" "*server*" "TODO.org" "*Org Agenda*")))
 '(clean-buffer-list-kill-never-regexps (quote ("^ \\*Minibuf-.*\\*$")))
 '(column-number-mode t)
 '(compilation-message-face (quote default))
 '(compilation-scroll-output t)
 '(compile-command "waf-build.sh")
 '(cua-global-mark-cursor-color "#2aa198")
 '(cua-normal-cursor-color "#839496")
 '(cua-overwrite-cursor-color "#b58900")
 '(cua-read-only-cursor-color "#859900")
 '(custom-safe-themes
   (quote
    ("5e32a3ac5ebdb2a5b96aad1277c8bf485f90cf1fc6b7204e953347369e4d5671" "846ef3695c42d50347884515f98cc359a7a61b82a8d0c168df0f688cf54bf089" "6145e62774a589c074a31a05dfa5efdf8789cf869104e905956f0cbd7eda9d0e" "73ad471d5ae9355a7fa28675014ae45a0589c14492f52c32a4e9b393fcc333fd" "264b639ee1d01cd81f6ab49a63b6354d902c7f7ed17ecf6e8c2bd5eb6d8ca09c" "3de3f36a398d2c8a4796360bfce1fa515292e9f76b655bb9a377289a6a80a132" "16dd114a84d0aeccc5ad6fd64752a11ea2e841e3853234f19dc02a7b91f5d661" "b8929cff63ffc759e436b0f0575d15a8ad7658932f4b2c99415f3dde09b32e97" "a62f0662e6aa7b05d0b4493a8e245ab31492765561b08192df61c9d1c7e1ddee" "d2bd16a8bcf295dce0b70e1d2b5c17bb34cb28224a86ee770d56e6c22a565013" "dd4628d6c2d1f84ad7908c859797b24cc6239dfe7d71b3363ccdd2b88963f336" "c968804189e0fc963c641f5c9ad64bca431d41af2fb7e1d01a2a6666376f819c" "9be1d34d961a40d94ef94d0d08a364c3d27201f3c98c9d38e36f10588469ea57" "6daa09c8c2c68de3ff1b83694115231faa7e650fdbb668bc76275f0f2ce2a437" "b3bcf1b12ef2a7606c7697d71b934ca0bdd495d52f901e73ce008c4c9825a3aa" "a24c5b3c12d147da6cef80938dca1223b7c7f70f2f382b26308eba014dc4833a" "115d42fa02a5ce6a759e38b27304e833d57a48422c2408d5455f14450eb96554" "36282815a2eaab9ba67d7653cf23b1a4e230e4907c7f110eebf3cdf1445d8370" "cea3ec09c821b7eaf235882e6555c3ffa2fd23de92459751e18f26ad035d2142" "722e1cd0dad601ec6567c32520126e42a8031cd72e05d2221ff511b58545b108" "b25964a29dbf00628876a845ee6b5f26784928f4d1a10576ac3765495ea59c64" "addfaf4c6f76ef957189d86b1515e9cf9fcd603ab6da795b82b79830eed0b284" "6383f86cac149fb10fc5a2bac6e0f7985d9af413c4be356cab4bfea3c08f3b42" "59171e7f5270c0f8c28721bb96ae56d35f38a0d86da35eab4001aebbd99271a8" "ef04dd1e33f7cbd5aa3187981b18652b8d5ac9e680997b45dc5d00443e6a46e3" "a0feb1322de9e26a4d209d1cfa236deaf64662bb604fa513cca6a057ddf0ef64" "0598c6a29e13e7112cfbc2f523e31927ab7dce56ebb2016b567e1eff6dc1fd4f" "a8245b7cc985a0610d71f9852e9f2767ad1b852c2bdea6f4aadc12cce9c4d6d0" "4ea1959cfaa526b795b45e55f77724df4be982b9cd33da8d701df8cdce5b2955" "eb1a15c88f3c262359d20241acd62b008ffd6e8a8a66deb8ab4240d430ee4a1c" "9cbb886d748c9c42ee432da5157ec67c738f72c944568c361c7c256e09ea5fcc" "9c5d36ca885acd224c9f8f0278a5182eb0720297213feb50fb51b44fbef31e6f" "93dd6770345b2f6c64e7848380cef352b7bfb7975932a160c220b52b3b1fa659" "4748082ee975f1b4f9698375649b35c42af8beb398778c2cf10dda57ab9abf4f" "e70a258438ec48b6fd541375f4987c43096fc50567996f066f503b7832dee87e" "c6b278aedb31dc254997e760360e6cb5732742462ba33332f1b70db9b96c500d" "0bb364a4715732ad139b05bcb1e98db9bc6fa4c7c71871f6d43c6e23a2110399" "7df9f0c538ab8aa07c595235d99b5252d6e17130d879d1d4186355a8207bcc65" "7e46ffcc3e974dfb4bf8baeac4772138ef3c8cd81cb1e00c45a689ae11957c6c" "999e080beb4a279260362d3876a345a69c9261d2a72d364f77399af2ea805c1e" "687ecd1cacfd3db42db0634eda71e69f5a8759c9892cc0330110eff2afc5a62f" "193e913b98a205e4954c7515c2bdde2998145766e6c58e789b30ee8c7b5eb0f9" "4154caa8409ff2eb6f74c913741420e7103b9ea26c3c7d1a5a16592d0d2f43e0" "6c7db7fdf356cf6bde4236248b17b129624d397a8e662cf1264e41dab87a4a9a" "5d3e0746023fc5e246eb3e0e48c1ccb5ce0387fc4273896c6cf02ee349c2eba8" "41c926d688a69c7d3c7d2eeb54b2ea3c32c49c058004483f646c1d7d1f7bf6ac" "868f73b5cf78e72ca2402e1d48675e49cc9a9619c5544af7bf216515d22b58e7" "ff7625ad8aa2615eae96d6b4469fcc7d3d20b2e1ebc63b761a349bebbb9d23cb" "cc60d17db31a53adf93ec6fad5a9cfff6e177664994a52346f81f62840fe8e23" "7feeed063855b06836e0262f77f5c6d3f415159a98a9676d549bfeb6c49637c4" "77bd459212c0176bdf63c1904c4ba20fce015f730f0343776a1a14432de80990" "9b402e9e8f62024b2e7f516465b63a4927028a7055392290600b776e4a5b9905" "aae95fc700f9f7ff70efbc294fc7367376aa9456356ae36ec234751040ed9168" "6f11ad991da959fa8de046f7f8271b22d3a97ee7b6eca62c81d5a917790a45d9" "c50a672a129e71b9362b209c63d4e203ccc88a388c370411535b8b54ecc878bc" "365d9553de0e0d658af60cff7b8f891ca185a2d7ba3fc6d29aadba69f5194c7f" "0b6cb9b19138f9a859ad1b7f753958d8a36a464c6d10550119b2838cedf92171" "d61fc0e6409f0c2a22e97162d7d151dee9e192a90fa623f8d6a071dbf49229c6" "ba9be9caf9aa91eb34cf11ad9e8c61e54db68d2d474f99a52ba7e87097fa27f5" "1bd383f15ee7345c270b82c5e41554754b2a56e14c2ddaa2127c3590d0303b95" "611e38c2deae6dcda8c5ac9dd903a356c5de5b62477469133c89b2785eb7a14d" "b81bfd85aed18e4341dbf4d461ed42d75ec78820a60ce86730fc17fc949389b2" "1b1e54d9e0b607010937d697556cd5ea66ec9c01e555bb7acea776471da59055" "0e33022384e4db1374827f51e3d9e9a2d56282c2e3568c22f1c12ad80e20cf0c" "fad38808e844f1423c68a1888db75adf6586390f5295a03823fa1f4959046f81" "2f0a552a9d14fe8ddaaacdb7b82a0eee1ea1f7f5d0850789915e5b04a1b9669f" "89531935dc9c4620a06579e525d5365f94639cb4c5c328bcabbd22bc39a862ed" "8ec2e01474ad56ee33bc0534bdbe7842eea74dccfb576e09f99ef89a705f5501" "551596f9165514c617c99ad6ce13196d6e7caa7035cea92a0e143dbe7b28be0e" "7ceb8967b229c1ba102378d3e2c5fef20ec96a41f615b454e0dc0bfa1d326ea6" "3e644bef0aaf755a08cfe62e11a32787af86f9c411b529452bbec44dad7bbeab" "6ee6f99dc6219b65f67e04149c79ea316ca4bcd769a9e904030d38908fd7ccf9" "cc0dbb53a10215b696d391a90de635ba1699072745bf653b53774706999208e3" "ed8cf6d52a2ba9ed7a29a8aac81d83c362a9b62f48b558932a77130163fe9972" "bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" "7356632cebc6a11a87bc5fcffaa49bae528026a78637acd03cae57c091afd9b9" "ab04c00a7e48ad784b52f34aa6bfa1e80d0c3fcacc50e1189af3651013eb0d58" "bcc6775934c9adf5f3bd1f428326ce0dcd34d743a92df48c128e6438b815b44f" "15348febfa2266c4def59a08ef2846f6032c0797f001d7b9148f30ace0d08bcf" "d6922c974e8a78378eacb01414183ce32bc8dbf2de78aabcc6ad8172547cb074" "04dd0236a367865e591927a3810f178e8d33c372ad5bfef48b5ce90d4b476481" "d79ece4768dfc4bab488475b85c2a8748dcdc3690e11a922f6be5e526a20b485" "47744f6c8133824bdd104acc4280dbed4b34b85faa05ac2600f716b0226fb3f6" "4cf3221feff536e2b3385209e9b9dc4c2e0818a69a1cdb4b522756bcdf4e00a4" "3448e3f5d01b39ce75962328a5310438e4a19e76e4b691c21c8e04ca318a5f62" "e269026ce4bbd5b236e1c2e27b0ca1b37f3d8a97f8a5a66c4da0c647826a6664" "718fb4e505b6134cc0eafb7dad709be5ec1ba7a7e8102617d87d3109f56d9615" "a2e7b508533d46b701ad3b055e7c708323fb110b6676a8be458a758dd8f24e27" "4aee8551b53a43a883cb0b7f3255d6859d766b6c5e14bcb01bed572fcbef4328" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "6de7c03d614033c0403657409313d5f01202361e35490a3404e33e46663c2596" "ed317c0a3387be628a48c4bbdb316b4fa645a414838149069210b66dd521733f" "e9460a84d876da407d9e6accf9ceba453e2f86f8b86076f37c08ad155de8223c" "83db918b06f0b1df1153f21c0d47250556c7ffb5b5e6906d21749f41737babb7" "938d8c186c4cb9ec4a8d8bc159285e0d0f07bad46edf20aa469a89d0d2a586ea" "3b31ebd74082c6a3043dfd8112069163978330e21cfc9e6ff2c9798dfd6d6505" "715fdcd387af7e963abca6765bd7c2b37e76154e65401cd8d86104f22dd88404" default)))
 '(dabbrev-case-fold-search nil)
 '(diary-entry-marker (quote font-lock-variable-name-face))
 '(emms-mode-line-icon-image-cache
   (quote
    (image :type xpm :ascent center :data "/* XPM */
static char *note[] = {
/* width height num_colors chars_per_pixel */
\"    10   11        2            1\",
/* colors */
\". c #1ba1a1\",
\"# c None s None\",
/* pixels */
\"###...####\",
\"###.#...##\",
\"###.###...\",
\"###.#####.\",
\"###.#####.\",
\"#...#####.\",
\"....#####.\",
\"#..######.\",
\"#######...\",
\"######....\",
\"#######..#\" };")))
 '(ensime-sem-high-faces
   (quote
    ((var :foreground "#9876aa" :underline
          (:style wave :color "yellow"))
     (val :foreground "#9876aa")
     (varField :slant italic)
     (valField :foreground "#9876aa" :slant italic)
     (functionCall :foreground "#a9b7c6")
     (implicitConversion :underline
                         (:color "#808080"))
     (implicitParams :underline
                     (:color "#808080"))
     (operator :foreground "#cc7832")
     (param :foreground "#a9b7c6")
     (class :foreground "#4e807d")
     (trait :foreground "#4e807d" :slant italic)
     (object :foreground "#6897bb" :slant italic)
     (package :foreground "#cc7832")
     (deprecated :strike-through "#a9b7c6"))))
 '(evil-symbol-word-search t)
 '(fci-rule-color "#383838" t)
 '(flycheck-color-mode-line-face-to-color (quote mode-line-buffer-id))
 '(flycheck-disabled-checkers
   (quote
    (go-gofmt systemd-analyze go-vet emacs-lisp emacs-lisp-checkdoc)))
 '(flycheck-go-golint-executable "revive")
 '(flycheck-pycheckers-checkers (quote (pylint pep8)))
 '(flycheck-pycheckers-max-line-length 150)
 '(flymake-number-of-errors-to-display 3)
 '(font-use-system-font t)
 '(frame-background-mode (quote dark))
 '(frame-brackground-mode (quote dark))
 '(fringe-mode 10 nil (fringe))
 '(global-flycheck-mode t)
 '(global-hl-line-mode t)
 '(gnus-logo-colors (quote ("#4c8383" "#bababa")) t)
 '(gnus-mode-line-image-cache
   (quote
    (image :type xpm :ascent center :data "/* XPM */
static char *gnus-pointer[] = {
/* width height num_colors chars_per_pixel */
\"    18    13        2            1\",
/* colors */
\". c #1ba1a1\",
\"# c None s None\",
/* pixels */
\"##################\",
\"######..##..######\",
\"#####........#####\",
\"#.##.##..##...####\",
\"#...####.###...##.\",
\"#..###.######.....\",
\"#####.########...#\",
\"###########.######\",
\"####.###.#..######\",
\"######..###.######\",
\"###....####.######\",
\"###..######.######\",
\"###########.######\" };")) t)
 '(godoc-command "godoc")
 '(grep-files-aliases
   (quote
    (("all" . "* .*")
     ("el" . "*.el")
     ("c" . "*.cc *.[ch]xx *.[ch]pp *.[cChH] *.CC *.HH *.[ch]++ *.inc")
     ("h" . "*.h")
     ("l" . "[Cc]hange[Ll]og*")
     ("m" . "[Mm]akefile*")
     ("tex" . "*.tex")
     ("texi" . "*.texi")
     ("asm" . "*.[sS]"))))
 '(highlight-changes-colors (quote ("#ff8eff" "#ab7eff")))
 '(highlight-symbol-colors
   (--map
    (solarized-color-blend it "#002b36" 0.25)
    (quote
     ("#b58900" "#2aa198" "#dc322f" "#6c71c4" "#859900" "#cb4b16" "#268bd2"))))
 '(highlight-symbol-foreground-color "#93a1a1")
 '(highlight-tail-colors
   (quote
    (("#424748" . 0)
     ("#63de5d" . 20)
     ("#4BBEAE" . 30)
     ("#1DB4D0" . 50)
     ("#9A8F21" . 60)
     ("#A75B00" . 70)
     ("#F309DF" . 85)
     ("#424748" . 100))))
 '(hippie-expand-try-functions-list
   (quote
    (try-expand-dabbrev try-expand-dabbrev-all-buffers try-expand-dabbrev-from-kill try-complete-file-name-partially try-complete-file-name try-expand-all-abbrevs try-expand-list try-complete-lisp-symbol-partially try-complete-lisp-symbol try-expand-line)))
 '(hl-bg-colors
   (quote
    ("#7B6000" "#8B2C02" "#990A1B" "#93115C" "#3F4D91" "#00629D" "#00736F" "#546E00")))
 '(hl-fg-colors
   (quote
    ("#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36")))
 '(hl-paren-background-colors (quote ("#2492db" "#95a5a6" nil)))
 '(hl-paren-colors
   (quote
    ("#B9F" "#B8D" "#B7B" "#B69" "#B57" "#B45" "#B33" "#B11")))
 '(hl-sexp-background-color "#1c1f26")
 '(ibuffer-deletion-face (quote diredp-deletion-file-name))
 '(ibuffer-marked-face (quote diredp-flag-mark))
 '(ido-default-buffer-method (quote selected-window))
 '(ido-default-file-method (quote selected-window))
 '(ido-enable-flex-matching t)
 '(initial-buffer-choice t)
 '(ispell-dictionary "british")
 '(ispell-program-name "/usr/bin/ispell")
 '(jdee-db-active-breakpoint-face-colors (cons "#1c1f24" "#51afef"))
 '(jdee-db-requested-breakpoint-face-colors (cons "#1c1f24" "#7bc275"))
 '(jdee-db-spec-breakpoint-face-colors (cons "#1c1f24" "#484854"))
 '(jedi:complete-on-dot nil)
 '(jedi:tooltip-method nil)
 '(js2-auto-indent-flag nil)
 '(js2-basic-offset 4)
 '(js2-electric-keys (quote nil))
 '(js2-mirror-mode nil)
 '(large-file-warning-threshold 100000000)
 '(linum-format " %6d ")
 '(lsp-auto-guess-root t)
 '(lsp-gopls-server-args nil)
 '(lsp-session-file "/home/menno/.emacs.d/var/.lsp-session-v1")
 '(lsp-ui-doc-enable nil)
 '(lsp-ui-imenu-enable nil)
 '(lsp-ui-peek-peek-height 30)
 '(lsp-ui-sideline-enable nil)
 '(magit-diff-use-overlays nil)
 '(magit-push-arguments nil)
 '(main-line-color1 "#222232")
 '(main-line-color2 "#333343")
 '(markdown-command
   "markdown_py -x markdown.extensions.fenced_code -x markdown.extensions.sane_lists -x markdown.extensions.tables")
 '(menu-bar-mode nil)
 '(midnight-hook
   (quote
    (refresh-file-cache recentf-save-list clean-buffer-list tramp-cleanup-all-connections)))
 '(midnight-mode t nil (midnight))
 '(mouse-avoidance-mode nil nil (avoid))
 '(nord-uniform-mode-lines t)
 '(nrepl-message-colors
   (quote
    ("#CC9393" "#DFAF8F" "#F0DFAF" "#7F9F7F" "#BFEBBF" "#93E0E3" "#94BFF3" "#DC8CC3")))
 '(org-agenda-files nil)
 '(org-ellipsis "  ")
 '(org-fontify-done-headline t)
 '(org-fontify-quote-and-verse-blocks t)
 '(org-fontify-whole-heading-line t)
 '(org-hide-leading-stars t)
 '(org-odd-levels-only t)
 '(package-selected-packages
   (quote
    (base16-theme alect-themes nord-theme gnu-elpa-keyring-update cython-mode typescript-mode go-eldoc org-re-reveal company-lsp flycheck-rust yasnippet pyenv-mode csharp-mode vue-html-mode vue-mode web-mode systemd powerline web-beautify pungi macrostep s ripgrep multiple-cursors flx company smex ido-vertical-mode flx-ido markdown-mode toml-mode slime-repl racer projectile-ripgrep popup metalspleen-theme flycheck evil-magit company-jedi clojure-mode)))
 '(pastebin-default-domain "paste.ubuntu.com")
 '(pastebin-domain-versions
   (quote
    (("pastebin.com" "/api_public.php")
     ("pastebin.example.com" "/pastebin.php")
     ("paste.ubuntu.com" ""))))
 '(pos-tip-background-color "#E6DB74")
 '(pos-tip-foreground-color "#242728")
 '(powerline-color1 "#222232")
 '(powerline-color2 "#333343")
 '(projectile-cache-file "/home/menno/.emacs.d/var/projectile.cache")
 '(projectile-globally-ignored-directories
   (quote
    (".idea" ".ensime_cache" ".eunit" ".git" ".hg" ".fslckout" "_FOSSIL_" ".bzr" "_darcs" ".tox" ".svn" ".stack-work" "vendor" "node_modules")))
 '(projectile-tags-command "ctags -Re --exclude=node_modules -f \"%s\" %s ")
 '(recentf-max-saved-items 100)
 '(recentf-save-file "~/.emacs.d/var/recentf")
 '(rg-custom-type-aliases (quote (("template" . "*.tmpl"))))
 '(ropemacs-enable-shortcuts nil)
 '(safe-local-variable-values (quote ((firestarter . ert-run-tests-interactively))))
 '(select-enable-primary t)
 '(shell-file-name "/bin/bash")
 '(show-paren-mode t)
 '(size-indication-mode t)
 '(smartrep-mode-line-active-bg (solarized-color-blend "#859900" "#073642" 0.2))
 '(sml-modeline-borders nil)
 '(sml-modeline-len 14)
 '(sml-modeline-mode t)
 '(sml-modeline-numbers (quote line-numbers))
 '(sml/active-background-color "#34495e")
 '(sml/active-foreground-color "#ecf0f1")
 '(sml/inactive-background-color "#dfe4ea")
 '(sml/inactive-foreground-color "#34495e")
 '(tags-revert-without-query t)
 '(term-default-bg-color "#002b36")
 '(term-default-fg-color "#839496")
 '(text-scale-mode-step 1.1)
 '(tool-bar-mode nil)
 '(tramp-ssh-controlmaster-options "" t)
 '(transient-history-file "~/.emacs.d/var/transient/history.el")
 '(uniquify-buffer-name-style (quote post-forward-angle-brackets) nil (uniquify))
 '(uniquify-min-dir-content 0)
 '(vc-annotate-background "#2B2B2B")
 '(vc-annotate-background-mode nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#BC8383")
     (40 . "#CC9393")
     (60 . "#DFAF8F")
     (80 . "#D0BF8F")
     (100 . "#E0CF9F")
     (120 . "#F0DFAF")
     (140 . "#5F7F5F")
     (160 . "#7F9F7F")
     (180 . "#8FB28F")
     (200 . "#9FC59F")
     (220 . "#AFD8AF")
     (240 . "#BFEBBF")
     (260 . "#93E0E3")
     (280 . "#6CA0A3")
     (300 . "#7CB8BB")
     (320 . "#8CD0D3")
     (340 . "#94BFF3")
     (360 . "#DC8CC3"))))
 '(vc-annotate-very-old-color "#DC8CC3")
 '(visible-bell t)
 '(weechat-color-list
   (unspecified "#242728" "#424748" "#F70057" "#ff0066" "#86C30D" "#63de5d" "#BEB244" "#E6DB74" "#40CAE4" "#06d8ff" "#FF61FF" "#ff8eff" "#00b2ac" "#53f2dc" "#f8fbfc" "#ffffff"))
 '(window-divider-default-right-width 1)
 '(window-divider-mode t)
 '(xgit-use-index (quote always))
 '(xterm-color-names
   ["#073642" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#eee8d5"])
 '(xterm-color-names-bright
   ["#002b36" "#cb4b16" "#586e75" "#657b83" "#839496" "#6c71c4" "#93a1a1" "#fdf6e3"])
 '(yaml-indent-offset 4))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "#020a07" :foreground "#8693ae" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 105 :width normal :foundry "JB  " :family "JetBrains Mono"))))
 '(ethan-wspace-face ((t (:background "#020a07" :foreground "firebrick" :underline (:color foreground-color :style wave)))))
 '(lazy-highlight ((t (:background "#264b4b"))))
 '(markdown-code-face ((t (:inherit fixed-pitch :background "#003300"))))
 '(markdown-header-face-1 ((t (:inherit markdown-header-face :foreground "deep sky blue" :height 1.0))))
 '(mmm-default-submode-face ((t (:background "gray10"))))
 '(mode-line ((t (:background "#2a3235" :foreground "#7b87a0" :box (:line-width 4 :color "black")))))
 '(org-level-1 ((t (:foreground "dark orange" :weight normal))))
 '(org-level-2 ((t (:foreground "deep sky blue" :weight normal))))
 '(org-level-3 ((t (:foreground "DodgerBlue3" :weight bold)))))
