;;; config.el --- darcylee Layer packages File for Spacemacs
;;
;; Copyright (c) 2017-2017 darcylee
;;
;; Author: darcylee <darcylee1986@gmail.com>
;; URL: https://github.com/darcylee/spacemacs-config
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(spacemacs|add-toggle iimage
  :status iimage-mode
  :on (iimage-mode)
  :off (iimage-mode -1)
  :documentation "Enable iimage mode"
  :evil-leader "oti")

;; reformat your json file, it requires python
(defun beautify-json ()
  (interactive)
  (let ((b (if mark-active (min (point) (mark)) (point-min)))
        (e (if mark-active (max (point) (mark)) (point-max))))
    (shell-command-on-region b e
                             "python -mjson.tool" (current-buffer) t)))




(add-to-list 'auto-mode-alist (cons (concat "\\." (regexp-opt
                                                   '("xml"
                                                     "xsd"
                                                     "rng"
                                                     "xslt"
                                                     "xsl")
                                                   t) "\\'") 'nxml-mode))
(setq nxml-slash-auto-complete-flag t)



(add-to-list 'auto-mode-alist '("\\.puml\\'" . plantuml-mode))

(add-to-list 'auto-mode-alist '("\\.mm\\'" . objc-mode))
(add-to-list 'auto-mode-alist '("\\.c\\'" . c++-mode))

;; return nil to write content to file
(defun darcylee/untabify-buffer ()
  (interactive)
  (save-excursion
    (untabify (point-min) (point-max)) nil))

(defun darcylee/tabify-buffer ()
  (interactive)
  (save-excursion
    (tabify (point-min) (point-max)) nil))

;; (add-hook 'c++-mode-hook
;;           #'(lambda ()
;;              (add-hook 'write-contents-hooks
;;                        'darcylee/untabify-buffer nil t)))

(defun my-c-mode-common-hook ()
  ;; my customizations for all of c-mode, c++-mode, objc-mode, java-mode
  (c-set-offset 'substatement-open 0)
  (c-set-offset 'case-label 0)
  (c-set-offset 'statement-case-open 0)

  ;; other customizations can go here
  (setq c-default-style "linux") ;; set style to "linux"
  (setq c++-tab-always-indent t)
  (setq c-basic-offset 4) ;; Default is 2
  (setq c-indent-level 4) ;; Default is 2
  (setq tab-stop-list '(4 8 12 16 20 24 28 32 36 40 44 48 52 56 60))
  (setq tab-width 4)
  (setq indent-tabs-mode nil)           ; use spaces only if nil
  )

(defun my-c-mode-common-hook-tab-is-8 ()
  ;; my customizations for all of c-mode, c++-mode, objc-mode, java-mode
  (c-set-offset 'substatement-open 0)
  (c-set-offset 'case-label 0)
  (c-set-offset 'statement-case-open 0)

  ;; other customizations can go here
  (setq c-default-style "linux") ;; set style to "linux"
  (setq c++-tab-always-indent t)
  (setq c-basic-offset 8) ;; Default is 2
  (setq c-indent-level 8) ;; Default is 2
  (setq tab-stop-list '(8 16 24 32 40 48 56 64 72 80 88 96 104 112 120))
  (setq tab-width 8)
  (setq indent-tabs-mode t)           ; use spaces only if nil
  )

(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)

(add-hook 'web-mode-hook 'darcylee/impatient-mode-hook)

(setq auto-mode-alist
      (append
       '(("\\.mak\\'" . makefile-bsdmake-mode))
       auto-mode-alist))

(setq gofmt-command "goimports")

(defmacro darcylee|toggle-company-backends (backend)
  "Push or delete the backend to company-backends"
  (let ((funsymbol (intern (format "zilong/company-toggle-%S" backend))))
    `(defun ,funsymbol ()
       (interactive)
       (if (eq (car company-backends) ',backend)
           (setq-local company-backends (delete ',backend company-backends))
         (push ',backend company-backends)))))


;; user template of doxymacs
(defconst doxymacs-file-all-comment-template
  '("/*" > n
    " * " "Copyright (c) " (format-time-string "%Y" (current-time)) " Ruijie Network Inc. All rights reserved." > n
    " */" > n
    "/**" > n
    " *   " (doxymacs-doxygen-command-char) "file   "
    (if (buffer-file-name)
        (file-name-nondirectory (buffer-file-name))
      "") > n
      " *   " (doxymacs-doxygen-command-char) "author " (user-full-name) (doxymacs-user-mail-address) > n
      " *   " (doxymacs-doxygen-command-char) "date   " (current-time-string) > n
      " *"> n
      " *   " (doxymacs-doxygen-command-char) "brief  " (p "Brief description of this file: ") > n
      " *" p > n
      " *   " (doxymacs-doxygen-command-char) "version 1.0" > n
      " *" > n
      " */"> n)
  "Default JavaDoc-style template for file documentation.")
(defconst doxymacs-file-comment-template
  '("/**" > n
    " *   " (doxymacs-doxygen-command-char) "file   "
    (if (buffer-file-name)
        (file-name-nondirectory (buffer-file-name))
      "") > n
      " *   " (doxymacs-doxygen-command-char) "author " (user-full-name) (doxymacs-user-mail-address) > n
      " *   " (doxymacs-doxygen-command-char) "date   " (current-time-string) > n
      " *"> n
      " *   " (doxymacs-doxygen-command-char) "brief  " (p "Brief description of this file: ") > n
      " *" p > n
      " *   " (doxymacs-doxygen-command-char) "version 1.0" > n
      " *" > n
      " */"> n)
  "Default JavaDoc-style template for file documentation.")

(defconst doxymacs-function-comment-template
  '((let ((next-func (doxymacs-find-next-func)))
      (if next-func
          (list
           'l
           "/**" '> 'n
           " * " (doxymacs-doxygen-command-char) "brief " 'p '> 'n
           " *" '> 'n
           (doxymacs-parm-tempo-element (cdr (assoc 'args next-func)))
           (unless (string-match
                    (regexp-quote (cdr (assoc 'return next-func)))
                    doxymacs-void-types)
             '(l " *" > n " * " (doxymacs-doxygen-command-char)
                 "return " (p "Returns: ") > n))
           " */" '>)
        (progn
          (error "Can't find next function declaration.")
          nil))))
  "Default JavaDoc-style template for function documentation.")

(defconst doxymacs-blank-singleline-comment-template
  '("/* " > p " */")
  "Default JavaDoc-style template for a blank single line doxygen comment.")

;; set doxymacs default style
(setq doxymacs-doxygen-style "JavaDoc")


(defvar custom-gtags-lib-path nil
  "my extra gtags lib path")
