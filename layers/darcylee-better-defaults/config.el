;;; config.el --- darcylee-better-default layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: dl <darcylee1986@gmail.com>
;; URL: https://github.com/darcylee/spacemacs-config.git
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq auto-coding-regexp-alist
      (delete (rassoc 'utf-16be-with-signature auto-coding-regexp-alist)
              (delete (rassoc 'utf-16le-with-signature auto-coding-regexp-alist)
                      (delete (rassoc 'utf-8-with-signature auto-coding-regexp-alist)
                              auto-coding-regexp-alist))))

(defun ffap-hexl-mode ()
  (interactive)
  (let ((ffap-file-finder 'hexl-find-file))
    (call-interactively 'ffap)))

(when (spacemacs/window-system-is-mac)
  (setq ns-pop-up-frames nil))

(global-prettify-symbols-mode 1)
(setq-default fill-column 80)

(setq recenter-positions '(top middle bottom))
;; delete the selection with a key press
(delete-selection-mode t)

;;add auto format paste code
(dolist (command '(yank yank-pop))
  (eval
   `(defadvice ,command (after indent-region activate)
      (and (not current-prefix-arg)
           (member major-mode
                   '(emacs-lisp-mode
                     lisp-mode
                     clojure-mode
                     scheme-mode
                     haskell-mode
                     ruby-mode
                     rspec-mode
                     python-mode
                     c-mode
                     c++-mode
                     objc-mode
                     latex-mode
                     js-mode
                     plain-tex-mode))
           (let ((mark-even-if-inactive transient-mark-mode))
             (indent-region (region-beginning) (region-end) nil))))))

;; tramp, for sudo access
;; very slow!!!!
;; for profiling emacs --debug-init --timed-requires --profile
;; (require 'tramp)
;; keep in mind known issues with zsh - see emacs wiki
;; (setq tramp-default-method "ssh")

;; This line has very bad performance lose!!!!!!!!!!!!!!!!!!!
;; (set-default 'imenu-auto-rescan t)

;; https://www.reddit.com/r/emacs/comments/4c0mi3/the_biggest_performance_improvement_to_emacs_ive/
(remove-hook 'find-file-hooks 'vc-find-file-hook)


(setq large-file-warning-threshold 100000000)
;;http://batsov.com/emacsredux/blog/2015/05/09/emacs-on-os-x/

(setq save-abbrevs nil)

;; turn on abbrev mode globally
(setq-default abbrev-mode t)

(setq url-show-status nil)

;;Don't ask me when close emacs with process is running
(defadvice save-buffers-kill-emacs (around no-query-kill-emacs activate)
  "Prevent annoying \"Active processes exist\" query when you quit Emacs."
  (flet ((process-list ())) ad-do-it))

;;Don't ask me when kill process buffer
(setq kill-buffer-query-functions
      (remq 'process-kill-buffer-query-function
            kill-buffer-query-functions))

;; cleanup recent files
(defun darcylee/cleanup-recentf ()
  (progn
    (and (fboundp 'recentf-cleanup)
         (recentf-cleanup))))

(add-hook 'kill-emacs-hook #'darcylee/cleanup-recentf)

;; change evil initial mode state
;; (menu-bar-mode t)

(add-hook 'before-save-hook
          (lambda ()
            (when buffer-file-name
              (let ((dir (file-name-directory buffer-file-name)))
                (when (and (not (file-exists-p dir))
                           (y-or-n-p (format "Directory %s does not exist. Create it?" dir)))
                  (make-directory dir t))))))

;; http://emacs.stackexchange.com/questions/13970/fixing-double-capitals-as-i-type
(defun dcaps-to-scaps ()
  "Convert word in DOuble CApitals to Single Capitals."
  (interactive)
  (and (= ?w (char-syntax (char-before)))
       (save-excursion
         (and (if (called-interactively-p)
                  (skip-syntax-backward "w")
                (= -3 (skip-syntax-backward "w")))
              (let (case-fold-search)
                (looking-at "\\b[[:upper:]]\\{2\\}[[:lower:]]"))
              (capitalize-word 1)))))

(define-minor-mode dubcaps-mode
  "Toggle `dubcaps-mode'.  Converts words in DOuble CApitals to
Single Capitals as you type."
  :init-value nil
  :lighter (" DC")
  (if dubcaps-mode
      (add-hook 'post-self-insert-hook #'dcaps-to-scaps nil 'local)
    (remove-hook 'post-self-insert-hook #'dcaps-to-scaps 'local)))

(defun spacemacs/check-large-file ()
  (when (> (buffer-size) 500000)
    (progn (fundamental-mode)
           (hl-line-mode -1)))
  (if (and (executable-find "wc")
           (> (string-to-number (shell-command-to-string (format "wc -l %s" (buffer-file-name))))
              5000))
      (linum-mode -1)))

(add-hook 'find-file-hook 'spacemacs/check-large-file)

(defadvice find-file (before make-directory-maybe
                             (filename &optional wildcards) activate)
  "Create parent directory if not exists while visiting file."
  (unless (file-exists-p filename)
    (let ((dir (file-name-directory filename)))
      (when dir
        (unless (file-exists-p dir)
          (make-directory dir t))))))

(add-hook 'minibuffer-inactive-mode-hook
          #'(lambda() (set (make-local-variable 'semantic-mode) nil)
              (set (make-local-variable 'electric-pair-mode) nil)))

;; http://trey-jackson.blogspot.com/2010/04/emacs-tip-36-abort-minibuffer-when.html
(defun darcylee/stop-using-minibuffer ()
  "kill the minibuffer"
  (when (and (>= (recursion-depth) 1) (active-minibuffer-window))
    (abort-recursive-edit)))

(add-hook 'mouse-leave-buffer-hook 'darcylee/stop-using-minibuffer)

(setq tags-add-tables nil)

(electric-pair-mode t)
;; https://www.reddit.com/r/emacs/comments/4xhxfw/how_to_tune_the_behavior_of_eletricpairmode/
(setq electric-pair-inhibit-predicate 'electric-pair-conservative-inhibit)
(show-paren-mode t)

;; http://oremacs.com/2015/01/17/setting-up-ediff/
;; (defmacro csetq (variable value)
;;   `(funcall (or (get ',variable 'custom-set)
;;                 'set-default)
;;             ',variable ,value))
;; (csetq ediff-diff-options "-w")

;; set mouse scrolling slower and smoother
;; https://stackoverflow.com/questions/445873/how-can-i-make-emacs-mouse-scrolling-slower-and-smoother
(setq mouse-wheel-scroll-amount '(3 ((shift) . 1) ((control) . nil)))
(setq mouse-wheel-progressive-speed nil)
(setq ring-bell-function 'ignore)

(setq backup-by-copying t
      make-backup-files nil
      create-lockfiles nil)

;; specify config
(setq-default indent-tabs-mode nil
              tab-width 4)

;; set coding config, last is highest priority.
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Recognize-Coding.html#Recognize-Coding
(prefer-coding-system 'cp950)
(prefer-coding-system 'cp936)
(prefer-coding-system 'utf-16)
(prefer-coding-system 'utf-8-dos)
(prefer-coding-system 'gb18030)
(prefer-coding-system 'gb2312)
(prefer-coding-system 'utf-8-unix)
;; (set-language-environment "Chinese-GBK")

;; show function info
(which-function-mode)
(setq which-func-modes '(emacs-lisp-mode
                         lisp-mode
                         clojure-mode
                         scheme-mode
                         haskell-mode
                         ruby-mode
                         rspec-mode
                         python-mode
                         c-mode
                         c++-mode
                         objc-mode
                         latex-mode
                         js-mode))

(setq which-func-unknown "n/a")
;; when editing js file, this feature is very useful
;; (setq-default header-line-format
;;               '((which-func-mode ("" which-func-format " "))))

;; more useful frame title, that show either a file or a
;; buffer name (if the buffer isn't visiting a file)
(setq frame-title-format
      '("GNU/Emacs - " user-login-name "@" system-name ":"
        (:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name)) "%b"))))

(setq-default spaceline-helm-help-p nil)

(add-hook 'ivy-occur-grep-mode-hook (lambda () (setq truncate-lines t)))
(add-hook 'ivy-occur-mode-hook (lambda () (setq truncate-lines t)))
