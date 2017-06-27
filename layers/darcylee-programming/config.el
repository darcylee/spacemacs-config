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

(add-hook 'term-mode-hook 'darcylee/ash-term-hooks)


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

(defun my-c++-mode-common-hook ()
  ;; my customizations for all of c-mode, c++-mode, objc-mode, java-mode
  (c-set-offset 'substatement-open 8)
  ;; other customizations can go here
  (setq c-default-style "linux")           ;; set style to "linux"
  (setq c++-tab-always-indent t)
  (setq c-basic-offset 8)                  ;; Default is 2
  (setq c-indent-level 8)                  ;; Default is 2

  ;; (setq tab-stop-list '(4 8 12 16 20 24 28 32 36 40 44 48 52 56 60))
  (setq tab-stop-list '(8 16 24 32 40 48 56 64 72 80 88 96 104 112 120))
  (setq tab-width 8)
  (setq indent-tabs-mode t)  ; use spaces only if nil
  )

(add-hook 'c++-mode-hook 'my-c++-mode-common-hook)

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
