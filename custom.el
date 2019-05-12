;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ahs-case-fold-search nil t)
 '(command-log-mode-window-size 50)
 '(company-dabbrev-minimum-length 2)
 '(company-dabbrev-other-buffers nil)
 '(company-show-numbers t)
 '(company-statistics-auto-restore nil)
 '(ctags-update-delay-seconds 1024)
 '(diff-default-read-only t)
 '(erc-nick "darcylee")
 '(erc-port 6666)
 '(evil-want-C-i-jump t)
 '(evil-want-Y-yank-to-eol nil)
 '(exec-path-from-shell-arguments (quote ("-i")))
 '(exec-path-from-shell-check-startup-files nil)
 '(expand-region-contract-fast-key "V")
 '(expand-region-exclude-text-mode-expansions (quote (html-mode nxml-mode web-mode)))
 '(expand-region-reset-fast-key "r")
 '(fill-column 100)
 '(global-command-log-mode nil)
 '(ivy-height 18)
 '(lua-documentation-url "http://www.lua.org/manual/5.3/manual.html")
 '(magit-use-overlays nil)
 '(only-global-abbrevs t)
 '(org-agenda-custom-commands nil)
 '(org-agenda-files nil)
 '(org-agenda-ndays 1)
 '(org-agenda-show-all-dates t)
 '(org-agenda-skip-deadline-if-done t)
 '(org-agenda-skip-deadline-prewarning-if-scheduled t)
 '(org-agenda-skip-scheduled-if-done t)
 '(org-agenda-start-on-weekday nil)
 '(org-agenda-text-search-extra-files (quote (agenda-archives)))
 '(org-deadline-warning-days 14)
 '(org-fast-tag-selection-single-key (quote expert))
 '(org-log-into-drawer t)
 '(org-pomodoro-play-sounds nil)
 '(org-reverse-note-order t)
 '(package-selected-packages
   (quote
    (zenburn-theme yang-mode symbol-overlay ace-window smartparens notmuch transient yasnippet async swiper flycheck request avy org-plus-contrib writeroom-mode visual-fill-column doom-themes tern iedit ivy company helm helm-core projectile magit git-commit ghub with-editor markdown-mode evil hydra yasnippet-snippets typescript-mode youdao-dictionary yapfify yaml-mode ws-butler wrap-region winum window-purpose which-key web-mode web-beautify visual-regexp-steroids uuidgen use-package unfill toc-org tiny tide tagedit string-inflection spaceline slim-mode shell-pop scss-mode sass-mode rjsx-mode reveal-in-osx-finder restart-emacs rainbow-mode rainbow-identifiers pyvenv pytest pyenv-mode py-isort pug-mode protobuf-mode prodigy prettier-js popwin plantuml-mode pippel pipenv pip-requirements persp-mode peep-dired pcre2el password-generator paradox overseer osx-trash osx-dictionary origami org-pomodoro org-mime org-brain open-junk-file nodejs-repl nameless mwim multi-term move-text mmm-mode markdown-toc magit-svn macrostep live-py-mode lispy link-hint launchctl json-navigator json-mode js2-refactor js-doc indent-guide importmagic impatient-mode ibuffer-projectile hungry-delete hl-todo highlight-parentheses highlight-numbers highlight-global helm-xref helm-pydoc helm-projectile helm-mode-manager helm-gtags helm-gitignore helm-github-stars helm-git-grep helm-flx helm-descbinds helm-css-scss helm-company helm-ag graphviz-dot-mode google-c-style golden-ratio godoctor go-tag go-rename go-impl go-guru go-gen-test go-fill-struct go-eldoc gnuplot gitignore-templates gitconfig-mode gitattributes-mode git-timemachine git-messenger git-link ggtags fuzzy flyspell-correct flycheck-pos-tip flycheck-gometalinter find-file-in-project fill-column-indicator expand-region evil-visualstar evil-visual-mark-mode evil-tutor evil-surround evil-org evil-numbers evil-nerd-commenter evil-matchit evil-magit evil-lion evil-iedit-state evil-goggles evil-cleverparens evil-anzu eval-sexp-fu eshell-z eshell-prompt-extras esh-help engine-mode emmet-mode elisp-slime-nav editorconfig dumb-jump dts-mode dotenv-mode doom-modeline discover-my-major diminish deft cython-mode company-web company-tern company-statistics company-lua company-go company-c-headers company-anaconda column-enforce-mode color-identifiers-mode cmake-font-lock centered-cursor-mode bind-map auto-yasnippet auto-highlight-symbol auto-compile aggressive-indent ace-link)))
 '(paradox-github-token t)
 '(ring-bell-function (quote ignore))
 '(send-mail-function (quote sendmail-send-it))
 '(sp-show-pair-from-inside t)
 '(spacemacs-theme-comment-bg nil)
 '(spacemacs-theme-comment-italic t)
 '(tab-width 4)
 '(tags-revert-without-query t)
 '(vc-follow-symlinks t)
 '(web-mode-markup-indent-offset 2)
 '(ycmd-extra-conf-handler (quote load))
 '(ycmd-extra-conf-whitelist (quote ("/work/*"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ahs-definition-face ((t (:foreground "#d33682" :slant normal :weight bold))))
 '(ahs-face ((t (:foreground "#d33682" :weight bold))))
 '(command-log-command ((t (:foreground "dark magenta"))))
 '(command-log-key ((t (:foreground "dark cyan"))))
 '(company-tooltip-common ((t (:inherit company-tooltip :weight bold :underline nil))))
 '(company-tooltip-common-selection ((t (:inherit company-tooltip-selection :weight bold :underline nil))))
 '(git-gutter-fr:added ((t (:foreground "#859900" :weight bold :width extra-expanded))))
 '(iedit-occurrence ((t (:inherit cursor))))
 '(ivy-virtual ((t (:background "gray" :foreground "dark magenta"))))
 '(js2-external-variable ((t (:foreground "plum3"))))
 '(mc/cursor-bar-face ((t (:background "chartreuse3"))))
 '(show-paren-match ((t (:background "dark gray" :foreground "#d33682" :weight bold))))
 '(sp-show-pair-match-face ((t (:background "#272822" :foreground "gray" :inverse-video t :weight normal))))
 '(web-mode-current-element-highlight-face ((t (:background "dark gray")))))
;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.
