;;; packages.el --- darcylee-better-default layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: dl <darcylee1986@gmail.com>
;; URL: https://github.com/darcylee/spacemacs-config.git
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defconst darcylee-better-defaults-packages
  '(
    (dired-mode :location built-in)
    (profiler :location built-in)
    (recentf :location built-in)
    pyim
    ;; highlight-symbol
    )
)

(defun darcylee-better-defaults/post-init-recentf ()
  (progn
    (setq recentf-exclude
          '("COMMIT_MSG"
            "COMMIT_EDITMSG"
            "github.*txt$"
            "/tmp/"
            "/ssh:"
            "/sudo:"
            "/TAGS$"
            "/GTAGS$"
            "/GRAGS$"
            "/GPATH$"
            "\\.mkv$"
            "\\.mp[34]$"
            "\\.avi$"
            "\\.pdf$"
            "\\.sub$"
            "\\.srt$"
            "\\.ass$"
            ".*png$"))
    (setq recentf-max-saved-items 2048)))

(defun darcylee-better-defaults/init-dired-mode ()
  (use-package dired-mode
    :defer t
    :init
    (progn
      (require 'dired-x)
      (require 'dired-aux)
      (setq dired-listing-switches "-alh")
      (setq dired-guess-shell-alist-user
            '(("\\.pdf\\'" "open")
              ("\\.docx\\'" "open")
              ("\\.\\(?:djvu\\|eps\\)\\'" "open")
              ("\\.\\(?:jpg\\|jpeg\\|png\\|gif\\|xpm\\)\\'" "open")
              ("\\.\\(?:xcf\\)\\'" "open")
              ("\\.csv\\'" "open")
              ("\\.tex\\'" "open")
              ("\\.\\(?:mp4\\|mkv\\|avi\\|flv\\|ogv\\)\\(?:\\.part\\)?\\'"
               "open")
              ("\\.\\(?:mp3\\|flac\\)\\'" "open")
              ("\\.html?\\'" "open")
              ("\\.md\\'" "open")))

      (setq dired-omit-files
            (concat dired-omit-files "\\|^.DS_STORE$\\|^.projectile$"))

      ;; always delete and copy recursively
      (setq dired-recursive-deletes 'always)
      (setq dired-recursive-copies 'always)

      (defvar dired-filelist-cmd
        '(("vlc" "-L")))

      ;; FIXME: evilify dired mode will lead to startup warnings
      ;; (evilified-state-evilify-map dired-mode-map
      ;;   :mode dired-mode
      ;;   :bindings
      ;;   (kbd "C-k") 'darcylee/dired-up-directory
      ;;   "<RET>" 'dired-find-alternate-file
      ;;   "E" 'dired-toggle-read-only
      ;;   "C" 'dired-do-copy
      ;;   "<mouse-2>" 'my-dired-find-file
      ;;   "`" 'dired-open-term
      ;;   "p" 'peep-dired-prev-file
      ;;   "n" 'peep-dired-next-file
      ;;   "z" 'dired-get-size
      ;;   "c" 'dired-copy-file-here
      ;;   ")" 'dired-omit-mode)
      )
    ))

(defun darcylee-better-defaults/init-profiler ()
  (use-package profiler
    :init
    (evilified-state-evilify profiler-report-mode profiler-report-mode-map)))

(defun darcylee-better-defaults/post-init-pyim ()
  ;; 自定义候选框风格
  (defun pyim-page-style:my-clear-type (page-info)
    (format "%s [%s/%s]\n%s"
            (pyim-page-preview-create)
            (gethash :current-page page-info)
            (gethash :total-page page-info)
            (pyim-page-menu-create
             (gethash :candidates page-info)
             (gethash :position page-info))))
  (progn
    ;; FIXME 这个在初始化的时候还是显示“灵通”
    (setq-default pyim-title "拼音")

    ;; (setq pyim-dicts
    ;;       '((:name "计算机词汇大全" :file "~/.spacemacs.d/extra/pyim-dict/jsjchdq.pyim")))

    (setq-default pyim-punctuation-half-width-functions
                  '(pyim-probe-punctuation-line-beginning
                    pyim-probe-punctuation-after-punctuation))

    (setq-default pyim-english-input-switch-functions
                  '(pyim-probe-program-mode
                    pyim-probe-auto-english))

    (setq pyim-fuzzy-pinyin-alist
          '(("z" "zh")
            ("c" "ch")
            ("s" "sh")
            ("an" "ang")
            ("en" "eng")
            ("in" "ing")
            ("ian" "iang")
            ("uan" "uang")))

    (setq pyim-punctuation-dict
          '(("'" "‘" "’")
            ("\"" "“" "”")
            ("\\" "、")
            ("_" "_")
            ("^" "…")
            ("]" "】")
            ("[" "【")
            ("@" "@")
            ("?" "？")
            (">" "》")
            ("=" "=")
            ("<" "《")
            (";" "；")
            (":" "：")
            ("/" "/")
            ("." "。")
            ("-" "-")
            ("," "，")
            ("+" "+")
            ("*" "*")
            (")" "）")
            ("(" "（")
            ("&" "&")
            ("%" "%")
            ("$" "￥")
            ("#" "#")
            ("!" "！")
            ("`" "・")
            ("~" "～")
            ("}" "』")
            ("|" "÷")
            ("{" "『")))

    (setq pyim-page-style 'my-clear-type)
    (setq pyim-dcache-prefer-emacs-thread t)
    (setq pyim-page-tooltip 'popup)

    ;; 使能基础字库
    (pyim-basedict-enable)
    ))

;;; packages.el ends here
