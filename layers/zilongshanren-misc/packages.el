;;; packages.el --- zilongshanren Layer packages File for Spacemacs
;;
;; Copyright (c) 2014-2016 zilongshanren
;;
;; Author: zilongshanren <guanghui8827@gmail.com>
;; URL: https://github.com/zilongshanren/spacemacs-private
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq zilongshanren-misc-packages
      '(
        projectile
        prodigy
        find-file-in-project
        multiple-cursors
        visual-regexp
        visual-regexp-steroids
        command-log
        evil
        discover-my-major
        ;; ace-window
        avy
        persp-mode
        tiny
        expand-region
        flyspell-correct
        ;; peep-dired
        markdown-mode
        swiper
        magit
        ;; gist
        hydra
        ;; wrap-region
        counsel
        ivy
        ivy-rich
        mu4e
        mu4e-alert
        golden-ratio
        helm
        (highlight-global :location (recipe :fetcher github :repo "glen-dai/highlight-global"))
        ))

(defun zilongshanren-misc/post-init-expand-region ()
  (with-eval-after-load 'expand-region
    (defadvice er/prepare-for-more-expansions-internal
        (around helm-ag/prepare-for-more-expansions-internal activate)
      ad-do-it
      (let ((new-msg (concat (car ad-return-value)
                             ", / to search in project, "
                             "f to search in files, "
                             "b to search in opened buffers"))
            (new-bindings (cdr ad-return-value)))
        (cl-pushnew
         '("/" (lambda ()
                 (call-interactively
                  'spacemacs/search-project-auto-region-or-symbol)
                 (evil-normal-state)))
         new-bindings)
        (cl-pushnew
         '("f" (lambda ()
                 (call-interactively
                  'spacemacs/search-auto-region-or-symbol)
                 (evil-normal-state)))
         new-bindings)
        (cl-pushnew
         '("b" (lambda ()
                 (call-interactively
                  'darcylee/swiper-all-region-or-symbol)))
         new-bindings)
        (setq ad-return-value (cons new-msg new-bindings))))))

(defun zilongshanren-misc/init-highlight-global ()
  (use-package highlight-global
    :init
    (progn
      (defface hl-global-face-1
        '((t (:background "dodger blue" :foreground "black")))
        "my highlight-global face default candidate 1"
        :group 'highlight-global)

      (defface hl-global-face-2
        '((t (:background "hot pink" :foreground "black")))
        "my highlight-global face default candidate 2"
        :group 'highlight-global)

      (defface hl-global-face-3
        '((t (:background "yellow" :foreground "black")))
        "my highlight-global face default candidate 3"
        :group 'highlight-global)

      (defface hl-global-face-4
        '((t (:background "orchid" :foreground "black")))
        "my highlight-global face default candidate 4"
        :group 'highlight-global)

      (defface hl-global-face-5
        '((t (:background "red" :foreground "black")))
        "my highlight-global face default candidate 5"
        :group 'highlight-global)

      (defface hl-global-face-6
        '((t (:background "salmon" :foreground "black")))
        "my highlight-global face default candidate 6"
        :group 'highlight-global)

      (defface hl-global-face-7
        '((t (:background "spring green" :foreground "black")))
        "my highlight-global face default candidate 7"
        :group 'highlight-global)

      (defface hl-global-face-8
        '((t (:background "turquoise" :foreground "black")))
        "my highlight-global face default candidate 8"
        :group 'highlight-global)

      (setq-default highlight-faces
                    '(('hl-global-face-1 . 0)
                      ('hl-global-face-2 . 0)
                      ('hl-global-face-3 . 0)
                      ('hl-global-face-4 . 0)
                      ('hl-global-face-5 . 0)
                      ('hl-global-face-6 . 0)
                      ('hl-global-face-7 . 0)
                      ('hl-global-face-8 . 0)))
      ;; set keys
      (spacemacs/set-leader-keys "hh" 'highlight-frame-toggle)
      (spacemacs/set-leader-keys "hc" 'clear-highlight-frame))))

(defun zilongshanren-misc/post-init-golden-ratio ()
  (with-eval-after-load 'golden-ratio
    (dolist (mode '("dired-mode" "occur-mode"))
      (add-to-list 'golden-ratio-exclude-modes mode))
    (dolist (n '("COMMIT_EDITMSG"))
      (add-to-list 'golden-ratio-exclude-buffer-names n))))

;; (defun zilongshanren-misc/post-init-ranger ()
;;   ;; https://emacs-china.org/t/ranger-golden-ratio/964/2
;;   (defun my-ranger ()
;;     (interactive)
;;     (if golden-ratio-mode
;;         (progn
;;           (golden-ratio-mode -1)
;;           (ranger)
;;           (setq golden-ratio-previous-enable t))
;;       (progn
;;         (ranger)
;;         (setq golden-ratio-previous-enable nil))))

;;   (defun my-quit-ranger ()
;;     (interactive)
;;     (if golden-ratio-previous-enable
;;         (progn
;;           (ranger-close)
;;           (golden-ratio-mode 1))
;;       (ranger-close)))

  ;; (with-eval-after-load 'ranger
  ;;   (progn
  ;;     (define-key ranger-normal-mode-map (kbd "q") 'my-quit-ranger)))

  ;; (spacemacs/set-leader-keys "ar" 'my-ranger))

(defun zilongshanren-misc/post-init-hydra ()
  (progn
    (defhydra hydra-hotspots (:color blue)
      "Hotspots"
      ("g" helm-github-stars "helm github stars")
      ("r" zilongshanren/run-current-file "run current file"))

    (defhydra multiple-cursors-hydra (:hint nil)
      "
       ^Up^            ^Down^        ^Other^
             ----------------------------------------------
         [_p_]   Next    [_n_]   Next    [_l_] Edit lines
         [_P_]   Skip    [_N_]   Skip    [_a_] Mark all
         [_M-p_] Unmark  [_M-n_] Unmark [_r_] Mark by regexp
         ^ ^             ^ ^ [_q_] Quit
       "
      ("l" mc/edit-lines :exit t)
      ("a" mc/mark-all-like-this :exit t)
      ("n" mc/mark-next-like-this)
      ("N" mc/skip-to-next-like-this)
      ("M-n" mc/unmark-next-like-this)
      ("p" mc/mark-previous-like-this)
      ("P" mc/skip-to-previous-like-this)
      ("M-p" mc/unmark-previous-like-this)
      ("r" mc/mark-all-in-region-regexp :exit t)
      ("q"
       nil))

    (defhydra
      hydra-apropos (:color blue)
      "Apropos"
      ("a" apropos "apropos")
      ("c" apropos-command "cmd")
      ("d" apropos-documentation "doc")
      ("e" apropos-value "val")
      ("l" apropos-library "lib")
      ("o" apropos-user-option "option")
      ("u" apropos-user-option "option")
      ("v" apropos-variable "var")
      ("i" info-apropos "info")
      ("t" tags-apropos "tags")
      ("z" hydra-customize-apropos/body "customize"))

    (defhydra
      hydra-customize-apropos (:color blue)
      "Apropos (customize)"
      ("a" customize-apropos "apropos")
      ("f" customize-apropos-faces "faces")
      ("g" customize-apropos-groups "groups")
      ("o" customize-apropos-options "options"))

    (define-key global-map (kbd "<f1>") 'hydra-hotspots/body)
    (spacemacs/set-leader-keys "oo" 'hydra-hotspots/body)
    ;; (bind-key*  "<f4>" 'hydra-apropos/body)
    (spacemacs/set-leader-keys "oh" 'hydra-apropos/body)

    ))

(defun zilongshanren-misc/post-init-gist ()
  (use-package gist
    :defer t
    :init
    (setq gist-list-format
          '((files "File" 30 nil "%s")
            (id "Id" 10 nil identity)
            (created "Created" 20 nil "%D %R")
            (visibility "Visibility" 10 nil
                        (lambda
                          (public)
                          (or
                           (and public "public")
                           "private")))
            (description "Description" 0 nil identity)))
    :config
    (progn
      (spacemacs|define-transient-state gist-list-mode
        :title "Gist-mode Transient State"
        :bindings
        ("k" gist-kill-current "delete gist")
        ("e" gist-edit-current-description "edit gist title")
        ("+" gist-add-buffer "add a file")
        ("-" gist-remove-file "delete a file")
        ("y" gist-print-current-url "print url")
        ("b" gist-browse-current-url "browse gist in browser")
        ("*" gist-star "star gist")
        ("^" gist-unstar "unstar gist")
        ("f" gist-fork "fork gist")
        ("q" nil "quit" :exit t)
        ("<escape>" nil nil :exit t))
      (spacemacs/set-leader-keys-for-major-mode 'gist-list-mode
        "." 'spacemacs/gist-list-mode-transient-state/body))
    ))

;; (defun zilongshanren-misc/init-peep-dired ()
;;   ;;preview files in dired
;;   (use-package peep-dired
;;     :defer t
;;     :commands (peep-dired-next-file
;;                peep-dired-prev-file)
;;     :bind (:map dired-mode-map
;;                 ("P" . peep-dired))))

(defun zilongshanren-misc/post-init-flyspell-correct ()
  (progn
    (with-eval-after-load 'flyspell
      (define-key flyspell-mode-map (kbd "C-;") 'flyspell-correct-previous-word-generic))
    (setq flyspell-correct-interface 'flyspell-correct-ivy)))

(defun zilongshanren-misc/post-init-smartparens ()
  (use-package smartparens
    :defer t
    :init
    (progn
      (global-set-key (kbd "C-(") 'wrap-sexp-with-new-round-parens))
    :config
    (progn
      (setq sp-highlight-pair-overlay nil)

      (evil-define-key 'normal sp-keymap
        (kbd ")>") 'sp-forward-slurp-sexp
        (kbd ")<") 'sp-forward-barf-sexp
        (kbd "(>") 'sp-backward-barf-sexp
        (kbd "(<") 'sp-backward-slurp-sexp))))

(defun zilongshanren-misc/init-tiny ()
  (use-package tiny
    :defer t
    :init
    (spacemacs/set-leader-keys "oe" 'tiny-expand)))

(defun zilongshanren-misc/init-helm ()
  (use-package helm
    :init
    (progn
      (setq helm-move-to-line-cycle-in-source t)

      (setq helm-no-header t)
      (setq helm-use-fuzzy 'always)
      (setq helm-position 'bottom)
      ;; limit max number of matches displayed for speed
      (setq helm-candidate-number-limit 100)

      ;; ignore boring files like .o and .a
      (setq helm-ff-skip-boring-files t))
    :config
    (with-eval-after-load 'helm
      (progn
        (setq helm-autoresize-max-height 0)
        (setq helm-autoresize-min-height 30)
        (helm-autoresize-mode 1)

        (when (spacemacs/system-is-mac)
          ;; replace locate with spotlight on Mac
          (setq helm-locate-command "mdfind -name %s %s"))

        (define-key helm-map (kbd "C-w") 'evil-delete-backward-word)
        (spacemacs/set-leader-keys "rh" 'helm-resume)
        ;; (define-key helm-find-files-map (kbd "DEL") 'helm-find-files-up-one-level)

        (push "\\.emlx$" helm-boring-file-regexp-list)))))

(defun zilongshanren-misc/init-helm-github-stars ()
  (use-package helm-github-stars
    :commands (helm-github-stars)
    :init
    (setq helm-github-stars-username "darcylee")))

(defun zilongshanren-misc/post-init-command-log ()
  (with-eval-after-load 'global-command-log-mode
    (setq clm/log-command-exceptions* (append clm/log-command-exceptions*
                                              '(evil-next-visual-line
                                                evil-previous-visual-line)))))

(defun zilongshanren-misc/init-litable ()
  (use-package litable
    :init
    :defer t))

(defun zilongshanren-misc/init-osx-dictionary ()
  (use-package osx-dictionary
    :init
    (progn
      (evilified-state-evilify osx-dictionary-mode osx-dictionary-mode-map)
      (setq osx-dictionary-use-chinese-text-segmentation t)
      (global-set-key (kbd "C-c d") 'osx-dictionary-search-pointer)
      )))

;; (defun zilongshanren-misc/init-4clojure ()
;;   (use-package 4clojure
;;     :init
;;     (progn
;;       (spacemacs/declare-prefix "o4" "4clojure")
;;       (spacemacs/set-leader-keys "o4q" '4clojure-open-question)
;;       (spacemacs/set-leader-keys "o4n" '4clojure-next-question)
;;       (spacemacs/set-leader-keys "o4p" '4clojure-previous-question)
;;       (spacemacs/set-leader-keys "o4c" '4clojure-check-answers)
;;       )))

(defun zilongshanren-misc/post-init-avy ()
  (progn
    (global-set-key (kbd "C-s-'") 'avy-goto-char-2)
    (global-set-key (kbd "M-'") 'avy-goto-char-2)))

(defun zilongshanren-misc/post-init-ace-window ()
  (global-set-key (kbd "C-x C-o") #'ace-window))

(defun zilongshanren-misc/init-discover-my-major ()
  (use-package discover-my-major
    :defer t
    :init
    (progn
      (spacemacs/set-leader-keys (kbd "mhm") 'discover-my-major)
      (evilified-state-evilify makey-key-mode makey-key-mode-get-key-map))))

(defun zilongshanren-misc/post-init-elfeed ()
  (use-package elfeed
    :init
    (global-set-key (kbd "C-x w") 'elfeed)
    :defer t
    :config
    (progn
      (setq elfeed-feeds
            '("http://nullprogram.com/feed/"
              "http://z.caudate.me/rss/"
              "http://irreal.org/blog/?feed=rss2"
              "http://feeds.feedburner.com/LostInTheTriangles"
              "http://tonybai.com/feed/"
              "http://planet.emacsen.org/atom.xml"
              "http://feeds.feedburner.com/emacsblog"
              "http://blog.binchen.org/rss.xml"
              "http://oremacs.com/atom.xml"
              "http://blog.gemserk.com/feed/"
              "http://www.masteringemacs.org/feed/"
              "http://t-machine.org/index.php/feed/"
              "http://gameenginebook.blogspot.com/feeds/posts/default"
              "http://feeds.feedburner.com/ruanyifeng"
              "http://coolshell.cn/feed"
              "http://blog.devtang.com/atom.xml"
              "http://emacsist.com/rss"
              "http://puntoblogspot.blogspot.com/feeds/2507074905876002529/comments/default"
              "http://angelic-sedition.github.io/atom.xml"))

      ;; (evilify elfeed-search-mode elfeed-search-mode-map)
      (evilified-state-evilify-map elfeed-search-mode-map
        :mode elfeed-search-mode
        :bindings
        "G" 'elfeed-update
        "g" 'elfeed-search-update--force)

      (defun zilong/elfeed-mark-all-as-read ()
        (interactive)
        (mark-whole-buffer)
        (elfeed-search-untag-all-unread))

      (define-key elfeed-search-mode-map (kbd "R") 'zilong/elfeed-mark-all-as-read)

      (defadvice elfeed-show-yank (after elfeed-show-yank-to-kill-ring activate compile)
        "Insert the yanked text from x-selection to kill ring"
        (kill-new (x-get-selection)))

      (ad-activate 'elfeed-show-yank))))

(defun zilongshanren-misc/post-init-evil ()
  (progn
    (setcdr evil-insert-state-map nil)
    (define-key evil-insert-state-map [escape] 'evil-normal-state)

    (push "TAGS" spacemacs-useless-buffers-regexp)

    (adjust-major-mode-keymap-with-evil "git-timemachine")
    (adjust-major-mode-keymap-with-evil "edebug")
    (adjust-major-mode-keymap-with-evil "tabulated-list")

    ;; ;; change evil initial mode state
    (loop for (mode . state) in
          '((shell-mode . normal)
            (org-journal-search-mode . emacs))
          do (evil-set-initial-state mode state))

    ;;mimic "nzz" behaviou in vim
    (defadvice evil-search-next (after advice-for-evil-search-next activate)
      (evil-scroll-line-to-center (line-number-at-pos)))

    (defadvice evil-search-previous (after advice-for-evil-search-previous activate)
      (evil-scroll-line-to-center (line-number-at-pos)))

    (define-key evil-normal-state-map (kbd ",/") 'evilnc-comment-or-uncomment-lines)

    ;; (defun my-evil-yank ()
    ;;   (interactive)
    ;;   (save-excursion
    ;;     (call-interactively 'evil-yank))
    ;;   (backward-char))

    ;; (define-key evil-visual-state-map (kbd "y") 'my-evil-yank)
    ;; (define-key evil-visual-state-map "p" 'evil-paste-after-from-0)
    (define-key evil-insert-state-map (kbd "C-r") 'evil-paste-from-register)

    (define-key evil-normal-state-map (kbd "Y") 'darcylee/yank-to-end-of-line)

    (define-key evil-normal-state-map (kbd "[ SPC") (lambda () (interactive) (evil-insert-newline-above) (forward-line)))
    (define-key evil-normal-state-map (kbd "] SPC") (lambda () (interactive) (evil-insert-newline-below) (forward-line -1)))

    (define-key evil-normal-state-map (kbd "[ b") 'previous-buffer)
    (define-key evil-normal-state-map (kbd "] b") 'next-buffer)
    (define-key evil-normal-state-map (kbd "M-y") 'counsel-yank-pop)
    (define-key evil-normal-state-map (kbd "K") 'move-text-up)
    (define-key evil-normal-state-map (kbd "J") 'move-text-down)

    ;; (define-key evil-insert-state-map "\C-e" 'end-of-line)
    ;; (define-key evil-insert-state-map "\C-n" 'next-line)
    ;; (define-key evil-insert-state-map "\C-k" 'kill-line)
    ;; (define-key evil-normal-state-map "\C-p" 'previous-line)
    (define-key evil-emacs-state-map (kbd "s-f") 'forward-word)
    (define-key evil-insert-state-map (kbd "s-f") 'forward-word)
    (define-key evil-emacs-state-map (kbd "s-b") 'backward-word)
    (define-key evil-insert-state-map (kbd "s-b") 'backward-word)

    (define-key evil-ex-completion-map "\C-a" 'move-beginning-of-line)
    (define-key evil-ex-completion-map "\C-b" 'backward-char)
    (define-key evil-ex-completion-map "\C-f" 'forward-char)
    (define-key evil-ex-completion-map "\C-k" 'kill-line)
    (define-key minibuffer-local-map (kbd "C-w") 'evil-delete-backward-word)

    (define-key evil-visual-state-map (kbd ",/") 'evilnc-comment-or-uncomment-lines)
    (define-key evil-visual-state-map (kbd "x") 'er/expand-region)
    (define-key evil-visual-state-map (kbd "X") 'er/contract-region)
    (define-key evil-visual-state-map (kbd "C-r") 'zilongshanren/evil-quick-replace)
    (define-key evil-visual-state-map (kbd "mn") 'mc/mark-next-like-this)
    (define-key evil-visual-state-map (kbd "mp") 'mc/mark-previous-like-this)
    (define-key evil-visual-state-map (kbd "ma") 'mc/mark-all-like-this)
    (define-key evil-visual-state-map (kbd "mf") 'mc/mark-all-like-this-in-defun)

    (define-key evil-visual-state-map "J" (concat ":m '>+1" (kbd "RET") "gv=gv"))
    (define-key evil-visual-state-map "K" (concat ":m '<-2" (kbd "RET") "gv=gv"))

    ;; in spacemacs, we always use evilify miscro state
    ;; (evil-add-hjkl-bindings package-menu-mode-map 'emacs)
    ;; Don't move back the cursor one position when exiting insert mode
    (setq evil-move-cursor-back nil)

    (define-key evil-emacs-state-map (kbd "C-w") 'evil-delete-backward-word)

    ;; for emacs shell mode
    ;; (define-key evil-emacs-state-map (kbd "s-b") 'ido-switch-buffer)
    ;; (define-key evil-emacs-state-map (kbd "s-f") 'ido-find-file)
    (evil-define-key 'emacs term-raw-map (kbd "C-w") 'evil-delete-backward-word)

    ;; (setq evil-insert-state-cursor '("chartreuse3" box))
    (define-key evil-insert-state-map (kbd "C-z") 'evil-emacs-state)

    ;; diff-mode
    (evil-define-key 'normal diff-mode-map
      (kbd "a") 'diff-apply-hunk
      (kbd "d") 'diff-hunk-kill
      (kbd "D") 'diff-file-kill
      (kbd "t") 'diff-test-hunk
      (kbd "n") 'diff-hunk-next
      (kbd "N") 'diff-file-next
      (kbd "p") 'diff-hunk-prev
      (kbd "P") 'diff-file-prev
      (kbd "o") 'diff-goto-source
      (kbd "s") 'diff-split-hunk
      (kbd "r") 'zilongshanren/diff-mode-revert-hunk
      (kbd "e") 'diff-ediff-patch
      (kbd "u") 'diff-undo
      (kbd "q") 'quit-window
      (kbd "Q") 'kill-this-buffer)))

(defun zilongshanren-misc/init-visual-regexp ()
  (use-package visual-regexp
    :commands (vr/replace vr/query-replace)))

(defun zilongshanren-misc/init-visual-regexp-steroids ()
  (use-package visual-regexp-steroids
    :commands (vr/select-replace vr/select-query-replace)
    :init
    (progn
      (define-key global-map (kbd "C-c r") 'vr/replace)
      (define-key global-map (kbd "C-c q") 'vr/query-replace))))

(defun zilongshanren-misc/init-multiple-cursors ()
  (use-package multiple-cursors
    :init
    (progn
      (bind-key* "C-s-l" 'mc/edit-lines)
      (bind-key* "C-s-f" 'mc/mark-all-dwim)
      (bind-key* "C-s-." 'mc/mark-next-like-this)
      (bind-key* "C-s-," 'mc/mark-previous-like-this)
      (bind-key* "s->" 'mc/unmark-next-like-this)
      (bind-key* "s-<" 'mc/unmark-previous-like-this)
      (bind-key* "C-c C-s-." 'mc/mark-all-like-this)

      ;; http://endlessparentheses.com/multiple-cursors-keybinds.html?source=rss
      (define-prefix-command 'endless/mc-map)
      ;; C-x m is usually `compose-mail'. Bind it to something
      ;; else if you use this command.
      (define-key ctl-x-map "m" 'endless/mc-map)

      ;; Really really nice!
      (define-key endless/mc-map "i" #'mc/insert-numbers)
      (define-key endless/mc-map "h" #'mc-hide-unmatched-lines-mode)
      (define-key endless/mc-map "a" #'mc/mark-all-like-this)

      ;; Occasionally useful
      (define-key endless/mc-map "d" #'mc/mark-all-symbols-like-this-in-defun)
      (define-key endless/mc-map "r" #'mc/reverse-regions)
      (define-key endless/mc-map "s" #'mc/sort-regions)
      (define-key endless/mc-map "l" #'mc/edit-lines)
      (define-key endless/mc-map "\C-a" #'mc/edit-beginnings-of-lines)
      (define-key endless/mc-map "\C-e" #'mc/edit-ends-of-lines))
    :config
    (setq mc/cmds-to-run-once
          '(
            counsel-M-x
            zilongshanren/my-mc-mark-next-like-this))
    (setq mc/cmds-to-run-for-all
          '(
            electric-newline-and-maybe-indent
            hungry-delete-backward
            spacemacs/backward-kill-word-or-region
            spacemacs/smart-move-beginning-of-line
            evil-substitute
            lispy-move-beginning-of-line
            lispy-move-end-of-line
            lispy-space
            lispy-delete-backward
            evil-exit-visual-state
            evil-backward-char
            evil-delete-char
            evil-escape-emacs-state
            evil-escape-insert-state
            mwim-beginning-of-code-or-line
            mwim-end-of-line-or-code
            evil-exit-emacs-state
            evil-previous-visual-line
            evil-next-visual-line
            evil-forward-char
            evil-insert
            evil-next-line
            evil-normal-state
            evil-previous-line
            evil-append
            evil-append-line
            forward-sentence
            kill-sentence
            org-self-insert-command
            sp-backward-delete-char
            sp-delete-char
            sp-remove-active-pair-overlay
            orgtbl-hijacker-command-109))))

(defun zilongshanren-misc/post-init-persp-mode ()
  (setq persp-autokill-buffer-on-remove 'kill-weak)
  (setq persp-kill-foreign-buffer-behaviour 'kill)
  (setq persp-lighter nil)
  (when (fboundp 'spacemacs|define-custom-layout)
    (spacemacs|define-custom-layout "@Work"
      :binding "w"
      :body
      (find-file "/work"))))

(defun zilongshanren-misc/post-init-evil-escape ()
  (setq evil-escape-delay 0.2))

(defun zilongshanren-misc/init-find-file-in-project ()
  (use-package find-file-in-project
    :defer t
    :config
    (progn
      ;; If you use other VCS (subversion, for example), enable the following option
      (dolist (item '("GTAGS"))
        (push item ffip-project-file))
      ;; Use 'fd' instead of 'find'. fd project https://github.com/sharkdp/fd
      (if (executable-find "fd")
          (progn
            (setq ffip-use-rust-fd t)
            (setq ffip-rust-fd-extra-opts
                  (concat "--ignore-file="
                          (expand-file-name ".gitignore_global" user-home-directory)))))
      ;; in MacOS X, the search file command is CMD+p
      ;; for this project, I'm only interested certain types of files
      ;; (setq-default ffip-patterns '("*.html" "*.js" "*.css" "*.java" "*.xml" "*.cpp" "*.h" "*.c" "*.mm" "*.m" "*.el"))
      ;; if the full path of current file is under SUBPROJECT1 or SUBPROJECT2
      ;; OR if I'm reading my personal issue track document,

      ;; this is unused for me.
      ;; (defadvice find-file-in-project (before my-find-file-in-project activate compile)
      ;;   (when (ffip-current-full-filename-match-pattern-p "\\(/fireball\\)")
      ;;     ;; set the root directory into "~/projs/PROJECT_DIR"
      ;;     (setq-local ffip-project-root "~/Github/fireball")
      ;;     ;; well, I'm not interested in concatenated BIG js file or file in dist/
      ;;     (setq-local ffip-find-options "-not -size +64k -not -iwholename '*/bin/*'")
      ;;     ;; do NOT search files in below directories, the default value is better.
      ;;     (dolist (item '("*/docs/html/*" "*.meta" "*/cocos2d-x/*" "*.asset" "*/visual-tests/res/*"))
      ;;       (push item  ffip-prune-patterns)))
      ;;   (when (ffip-current-full-filename-match-pattern-p "\\(/cocos2d-x\\)")
      ;;     ;; set the root directory into "~/projs/PROJECT_DIR"
      ;;     (setq-local ffip-project-root "~/cocos2d-x")
      ;;     ;; well, I'm not interested in concatenated BIG js file or file in dist/
      ;;     (setq-local ffip-find-options "-not -size +64k -not -iwholename '*/bin/*'")
      ;;     ;; do NOT search files in below directories, the default value is better.
      ;;     ;; (setq-default ffip-prune-patterns '(".git" ".hg" "*.svn" "node_modules" "bower_components" "obj"))
      ;;     ))
      ;; (ad-activate 'find-file-in-project)
      )))

(defun zilongshanren-misc/post-init-projectile ()
  (progn
    (with-eval-after-load 'projectile
      (progn
        (dolist (item '("GTAGS"))
          (push item projectile-project-root-files-bottom-up))

        (setq projectile-completion-system 'ivy)
        (add-to-list 'projectile-other-file-alist '("html" "js"))
        (add-to-list 'projectile-other-file-alist '("js" "html"))))

    (defvar my-simple-todo-regex "\\<\\(FIXME\\|TODO\\|BUG\\):")

    (defun my-simple-todo ()
      "When in a project, create a `multi-occur' buffer matching the
  regex in `my-simple-todo-regex' across all buffers in the
  current project. Otherwise do `occur' in the current file."
      (interactive)
      (if (projectile-project-p)
          (multi-occur (projectile-project-buffers) my-simple-todo-regex)
        (occur my-simple-todo-regex)))
    (spacemacs/set-leader-keys "pt" 'my-simple-todo)))


(defun zilongshanren-misc/post-init-prodigy ()
  (progn
    (prodigy-define-tag
      :name 'jekyll
      :env '(("LANG" "en_US.UTF-8")
             ("LC_ALL" "en_US.UTF-8")))
    ;; define service
    (prodigy-define-service
      :name "Preview cocos2d-x web"
      :command "python"
      :args '("-m" "SimpleHTTPServer" "6001")
      :cwd "~/cocos2d-x/web"
      :tags '(work)
      :kill-signal 'sigkill
      :kill-process-buffer-on-stop t)

    (prodigy-define-service
      :name "Preview creator engine"
      :command "python"
      :args '("-m" "SimpleHTTPServer" "6004")
      :cwd "~/Github/fireball/engine"
      :tags '(work)
      :kill-signal 'sigkill
      :kill-process-buffer-on-stop t)

    (prodigy-define-service
      :name "Hexo Server"
      :command "hexo"
      :args '("server")
      :cwd "~/4gamers.cn"
      :tags '(hexo server)
      :kill-signal 'sigkill
      :kill-process-buffer-on-stop t)

    (prodigy-define-service
      :name "Hexo Deploy"
      :command "hexo"
      :args '("deploy" "--generate")
      :cwd "~/4gamers.cn"
      :tags '(hexo deploy)
      :kill-signal 'sigkill
      :kill-process-buffer-on-stop t)

    (prodigy-define-service
      :name "Debug Fireball"
      :command "npm"
      :args '("start" "--" "--nologin" "/Users/guanghui/Github/example-cases")
      :cwd "~/Github/fireball/"
      :tags '(work)
      :kill-signal 'sigkill
      :kill-process-buffer-on-stop t)

    (prodigy-define-service
      :name "Org wiki preview"
      :command "python"
      :args '("-m" "SimpleHTTPServer" "8088")
      :cwd "~/org-notes/public_html"
      :tags '(org-mode)
      :init (lambda () (browse-url "http://localhost:8088"))
      :kill-signal 'sigkill
      :kill-process-buffer-on-stop t)))

(defun zilongshanren-misc/init-moz-controller ()
  (use-package moz-controller
    :init
    (moz-controller-global-mode t)
    :diminish moz-controller-mode))


(defun zilongshanren-misc/init-ag ()
  (use-package ag
    :init))

(defun zilongshanren-misc/post-init-erc ()
  (progn
    (add-hook 'erc-text-matched-hook 'my-erc-hook)
    (spaceline-toggle-erc-track-off)))

(defun zilongshanren-misc/init-wrap-region ()
  (use-package wrap-region
    :init
    (progn
      (wrap-region-global-mode t)
      (wrap-region-add-wrappers
       '(("$" "$")
         ("{-" "-}" "#")
         ("/" "/" nil ruby-mode)
         ("/* " " */" "#" (java-mode javascript-mode css-mode js2-mode))
         ("`" "`" nil (markdown-mode ruby-mode))))
      (add-to-list 'wrap-region-except-modes 'dired-mode)
      (add-to-list 'wrap-region-except-modes 'web-mode))
    :defer t
    :config
    (spacemacs|hide-lighter wrap-region-mode)))

(defun zilongshanren-misc/init-keyfreq ()
  (use-package keyfreq
    :init
    (progn
      (keyfreq-mode t)
      (keyfreq-autosave-mode 1))))

(defun zilongshanren-misc/post-init-swiper ()
  (progn
    (define-key global-map (kbd "C-s") 'my-swiper-search)))

(defun zilongshanren-misc/post-init-counsel ()
  (progn
    (define-key counsel-ag-map (kbd "TAB") 'ivy-call-and-recenter)
    (define-key counsel-ag-map (kbd "C-n") 'ivy-next-line-and-call)
    (define-key counsel-ag-map (kbd "C-p") 'ivy-previous-line-and-call)
    (define-key counsel-imenu-map (kbd "TAB") 'ivy-call-and-recenter)
    (define-key counsel-imenu-map (kbd "C-n") 'ivy-next-line-and-call)
    (define-key counsel-imenu-map (kbd "C-p") 'ivy-previous-line-and-call)
    (define-key counsel-find-file-map (kbd "TAB") 'ivy-alt-done)))

(defun zilongshanren-misc/post-init-ivy ()
  (progn
    (spacemacs|hide-lighter ivy-mode)

    (evilified-state-evilify ivy-occur-mode ivy-occur-mode-map)

    (ivy-set-actions
     t
     '(("f" my-find-file-in-git-repo "find files")
       ("!" my-open-file-in-external-app "Open file in external app")
       ("I" ivy-insert-action "insert")))

    (spacemacs/set-leader-keys "fad" 'counsel-goto-recent-directory)
    (spacemacs/set-leader-keys "faf" 'counsel-find-file-recent-directory)

    (setq ivy-wrap t)
    (setq ivy-initial-inputs-alist nil)
    ;; (setq ivy-count-format "[%d/%d] ")
    (setq ivy-use-virtual-buffers t)
    (setq ivy-display-style 'fancy)
    (setq confirm-nonexistent-file-or-buffer t)
    ;; (setq ivy-use-selectable-prompt nil)
    (setq ivy-extra-directories '("./"))

    (add-to-list 'ivy-sort-functions-alist
                 '(read-file-name-internal . darcylee/eh-ivy-sort-file-function))

    (with-eval-after-load 'ivy
      (add-to-list 'ivy-ignore-buffers #'darcylee/ignore-dired-buffers))

    (defun ivy-occur-previous-error (&optional n)
      (interactive "p")
      (ivy-occur-next-error (- (or n 1))))

    (define-key ivy-occur-grep-mode-map (kbd "g") nil)
    (define-key ivy-occur-grep-mode-map (kbd "r") 'ivy-occur-revert-buffer)
    (define-key ivy-occur-grep-mode-map (kbd "n") 'ivy-occur-next-error)
    (define-key ivy-occur-grep-mode-map (kbd "p") 'ivy-occur-previous-error)
    (define-key ivy-minibuffer-map (kbd "C-c o") 'ivy-occur)
    (define-key ivy-minibuffer-map (kbd "C-s-m") 'ivy-partial-or-done)
    (define-key ivy-minibuffer-map (kbd "C-c s") 'ivy-ff-checksum)
    (define-key ivy-minibuffer-map (kbd "s-o") 'ivy-dispatching-done)
    (define-key ivy-minibuffer-map (kbd "C-c C-e") 'spacemacs//counsel-edit)
    (define-key ivy-minibuffer-map (kbd "<f3>") 'ivy-occur)
    (define-key ivy-minibuffer-map (kbd "TAB") 'ivy-alt-done)
    (define-key ivy-minibuffer-map (kbd "C-s-j") 'ivy-immediate-done)
    ;; (define-key ivy-minibuffer-map (kbd "C-j") 'ivy-next-line)
    ;; (define-key ivy-minibuffer-map (kbd "C-k") 'ivy-previous-line)
    (define-key ivy-minibuffer-map (kbd "<C-h") 'counsel-up-directory)))

(defun zilongshanren-misc/post-init-ivy-rich ()
  (use-package ivy-rich
    :init
    (progn
      (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line)
      (setq ivy-rich-path-style 'abbrev))
    :config
    (progn
      (setq ivy-rich-display-transformers-list
            '(ivy-switch-buffer
              (:columns
               ((ivy-read-file-transformer)
                (ivy-rich-counsel-find-file-truename (:face font-lock-doc-face))))
              counsel-M-x
              (:columns
               ((counsel-M-x-transformer (:width 60))
                (ivy-rich-counsel-function-docstring (:face font-lock-doc-face))))
              counsel-recentf
              (:columns
               ((ivy-rich-candidate (:width 0.8))
                (ivy-rich-file-last-modified-time (:face font-lock-comment-face))))
              ))
      (ivy-rich-reload))))

(defun zilongshanren-misc/post-init-magit ()
  (progn
    (with-eval-after-load 'magit
      (progn
        (add-to-list 'magit-no-confirm 'stage-all-changes)
        (define-key magit-log-mode-map (kbd "W") 'magit-copy-section-value)
        (define-key magit-status-mode-map (kbd "s-1") 'magit-jump-to-unstaged)
        (define-key magit-status-mode-map (kbd "s-2") 'magit-jump-to-untracked)
        (define-key magit-status-mode-map (kbd "s-3") 'magit-jump-to-staged)
        (define-key magit-status-mode-map (kbd "s-4") 'magit-jump-to-stashes)
        (setq magit-completing-read-function 'magit-builtin-completing-read)

        (magit-define-popup-switch 'magit-push-popup ?u
                                   "Set upstream" "--set-upstream")
        ))

    (evil-define-key 'normal magit-mode-map
      (kbd "n") 'magit-section-forward
      (kbd "p") 'magit-section-backward)

    (evil-define-key 'normal magit-diff-mode-map
      (kbd "o") 'magit-diff-visit-file-other-window
      (kbd "C-o") 'magit-diff-visit-worktree-file-other-window)

    ;; prefer two way ediff
    (setq magit-ediff-dwim-show-on-hunks t)

    (setq magit-repository-directories '("~/cocos2d-x/"))

    (eval-after-load 'magit
      '(define-key magit-mode-map (kbd "C-c g")
         #'zilongshanren/magit-visit-pull-request))

    (put 'magit-log-mode 'magit-log-default-arguments
         '("--graph" "-n256" "--decorate" "--color"))

    (setq magit-process-popup-time 10)))

(defun zilongshanren-misc/post-init-markdown-mode ()
  (progn
    (add-to-list 'auto-mode-alist '("\\.mdown\\'" . markdown-mode))

    (with-eval-after-load 'markdown-mode
      (progn
        ;; (when (configuration-layer/package-usedp 'company)
        ;;   (spacemacs|add-company-hook markdown-mode))

        (add-hook 'markdown-mode-hook 'bug-reference-mode)

        (spacemacs/set-leader-keys-for-major-mode 'gfm-mode-map
          "p" 'zilongshanren/markdown-to-html)
        (spacemacs/set-leader-keys-for-major-mode 'markdown-mode
          "p" 'zilongshanren/markdown-to-html)

        (evil-define-key 'normal markdown-mode-map (kbd "TAB") 'markdown-cycle)))))

(defun zilongshanren-misc/post-init-mu4e-alert ()
  (use-package mu4e-alert
    :init
    (progn
      ;; set notify style
      ;; more style see https://github.com/jwiegley/alert#builtin-alert-styles
      (cond
       ;; apt install libnotify-bin
       ((spacemacs/system-is-linux)
        (if (executable-find "notify-send")
            (mu4e-alert-set-default-style 'libnotify)
          (mu4e-alert-set-default-style 'notifications)))
       ((spacemacs/system-is-mac) (mu4e-alert-set-default-style 'notifier)))

      ;; only notify subjects
      (setq mu4e-alert-email-notification-types '(subjects))

      ;; Enable Desktop notification
      (mu4e-alert-enable-notifications)

      ;; Enable mode-line notifcation
      (mu4e-alert-enable-mode-line-display))))

(defun zilongshanren-misc/post-init-mu4e ()
  (use-package mu4e
    :init
    (progn
      (require 'smtpmail-async)
      ;; convert org content in mu4e to html and send
      (require 'org-mime)
      (require 'org-mu4e)

      ;; Set up some common mu4e variables
      (setq mu4e-maildir "~/.mail"
            mu4e-drafts-folder "/Drafts"
            mu4e-sent-folder   "/Sent Messages"
            mu4e-refile-folder "/Archive"
            mu4e-trash-folder "/Deleted Messages"
            mu4e-attachment-dir "~/Downloads"
            ;; mu4e-compose-signature-auto-include nil
            )
      (setq org-mu4e-convert-to-html t)
      (add-hook 'org-ctrl-c-ctrl-c-hook 'zilongshanren/htmlize-and-send t)

      ;; (when (and (spacemacs/system-is-mac) window-system)
      ;;   (setq mu4e-html2text-command
      ;;         "textutil -stdin -format html -convert txt -stdout"))

      ;; Mail directory shortcuts
      (setq mu4e-maildir-shortcuts
            '(("/INBOX" . ?i)
              ("/Sent Messages" . ?s)
              ("/Junk" . ?j)
              ("/Deleted Messages" . ?d)
              ))

      ;; hide index messsage
      (setq mu4e-hide-index-messages t)
      ;; indicate update status in modeline
      (setq mu4e-display-update-status-in-modeline t)

      ;; sync email from imap server
      (setq mu4e-get-mail-command "offlineimap -q"
            mu4e-update-interval 300)

      ;;send mail
      ;; (setq message-send-mail-function 'async-smtpmail-send-it)
      (setq message-send-mail-function 'message-send-mail-with-sendmail)
      ;; (setq message-send-mail-function (quote smtpmail-send-it))

      (setq mu4e-view-show-images t
            mu4e-view-show-addresses t)
      (setq mu4e-index-cleanup nil   ;; don't do a full cleanup check
            mu4e-index-lazy-check t) ;; don't consider up-to-date dirs

      ;; (setq smtpmail-stream-type 'starttls
      ;;       smtpmail-default-smtp-server "smtp.qq.com"
      ;;       smtpmail-smtp-server "smtp.qq.com"
      ;;       smtpmail-smtp-service 587
      ;;       smtpmail-local-domain "gitlee.com"
      ;;       smtpmail-sendto-domain "gitlee.com"
      ;;       smtpmail-queue-dir "~/.mail/queued-mail"
      ;;       )

      (define-key mu4e-main-mode-map (kbd "c") 'zilongshanren/mu4e-compose-org-mail)
      (define-key mu4e-main-mode-map (kbd "q") 'kill-this-buffer)
      (define-key mu4e-main-mode-map (kbd "Q") 'mu4e-quit)

      ;; org-mime-setting
      ;; (add-hook 'org-mime-html-hook
      ;;           (lambda ()
      ;;             (while (re-search-forward "@\\([^@]*\\)@" nil t)
      ;;               (replace-match "<span style=\"background-color:red;color:yellow\">\\1</span>"))
      ;;             (org-mime-change-element-style
      ;;              "blockquote" "border-left: 2px solid gray; padding-left: 4px;")
      ;;             (insert (concat "<head>\n<style>\n"
      ;;                             (with-temp-buffer
      ;;                               (insert-file-contents
      ;;                                (expand-file-name "extra/css/org-wide.css" dotspacemacs-directory))
      ;;                               (buffer-string)) "</style>\n</head>\n"))))
      )))
