;;; keybindings.el --- darcylee Layer packages File for Spacemacs
;;
;; Copyright (c) 2016 darcylee
;;
;; Author: darcylee <darcylee1986@gmail.com>
;; URL:
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(define-key global-map (kbd "C-c y") 'darcylee/youdao-dictionary-search)
(global-set-key (kbd "s-l") 'goto-line)
(global-set-key (kbd "s-/") 'evilnc-comment-or-uncomment-lines)
(global-set-key (kbd "C-s") 'swiper)
(global-set-key (kbd "C-s-h") 'mark-defun)
;; Utility functions
(defun bb/define-key (keymap &rest bindings)
  (declare (indent 1))
  (while bindings
    (define-key keymap (pop bindings) (pop bindings))))

(define-key evil-normal-state-map (kbd "-") nil)

(bb/define-key evil-normal-state-map
               "+" 'evil-numbers/inc-at-pt
               "-" 'evil-numbers/dec-at-pt
               "\\" 'evil-repeat-find-char-reverse
               "[s" (lambda (n) (interactive "p") (dotimes (c n nil) (insert " ")))
               "]s" (lambda (n) (interactive "p")
                      (forward-char) (dotimes (c n nil) (insert " ")) (backward-char (1+ n))))

(with-eval-after-load 'company
  (progn
    (bb/define-key company-active-map (kbd "C-w") 'evil-delete-backward-word)
    (bb/define-key company-active-map (kbd "C-f") nil)

    (bb/define-key company-active-map (kbd "C-n") 'company-select-next)
    (bb/define-key company-active-map (kbd "C-p") 'company-select-previous-or-abort)
    (bb/define-key company-active-map (kbd "C-u") 'company-previous-page)
    (bb/define-key company-active-map (kbd "C-d") 'company-next-page)
    (bb/define-key company-active-map (kbd "s-w") 'company-show-location)))

(spacemacs/declare-prefix "ot" "Toggle")


(if (configuration-layer/layer-usedp 'helm)
    (progn (global-set-key (kbd "<f1>") 'zilongshanren/helm-hotspots)
           (spacemacs/set-leader-keys "oo" 'zilongshanren/helm-hotspots)))

(spacemacs/set-leader-keys "fF" 'find-file-in-project)
(spacemacs/set-leader-keys "fR" 'spacemacs/rename-current-buffer-file)

(spacemacs|spacebind
 :global
 (("b" "Buffers"
   ("D" spacemacs/kill-other-buffers "Kill other buffers")
   ("k" darcylee/kill-useless-buffers "Kill useless buffers")
   ("C-d" spacemacs/kill-matching-buffers-rudely "Kill matching buffers")
   ("M" "Bookmark"
    ("s" bookmark-set "Set Bookmark")
    ("r" bookmark-rename "Rename Bookmark")
    ("d" bookmark-delete "Delete Bookmark")
    ("j" bookmark-jump "Jump Bookmark"))
   )
  ("o" "User Bindings"
   ("C" my-auto-update-tags-when-save "Auto Update tags when save")
   ("c" "capture screen"
    ("s" zilongshanren/capture-selection "Selection")
    ("w" zilongshanren/capture-window "Window"))
   ("g" helm-github-stars "Helm github start")
   ("r" zilongshanren/browser-refresh--chrome-applescript "refresh chrome")
   ("x" org-open-at-point "org open at point")
   )))

;; helm specific keybindings
(if (configuration-layer/layer-usedp 'helm)
    (progn
      (spacemacs/set-leader-keys "rh" 'helm-resume)
      (spacemacs/set-leader-keys "sj" 'counsel-imenu)))

(spacemacs/set-leader-keys "en" 'flycheck-next-error)
(spacemacs/set-leader-keys "ep" 'flycheck-previous-error)
(spacemacs/set-leader-keys "o(" 'ielm)

(spacemacs/set-leader-keys "gL" 'magit-log-buffer-file)
(spacemacs/set-leader-keys "og" 'my-git-timemachine)

;; (spacemacs/set-leader-keys "sj" 'helm-imenu)

(spacemacs/set-leader-keys "if" 'spacemacs/insert-file)
(spacemacs/set-leader-keys "ib" 'insert-buffer)

(when (or (spacemacs/system-is-linux)
          (spacemacs/system-is-mswindows))
  (global-set-key (kbd "C-w") 'evil-delete-backward-word)
  (global-set-key (kbd "s-=") 'spacemacs/scale-up-font)
  (global-set-key (kbd "s--") 'spacemacs/scale-down-font)
  (global-set-key (kbd "s-0") 'spacemacs/reset-font-size)
  (global-set-key (kbd "s-q") 'save-buffers-kill-terminal)
  (global-set-key (kbd "s-v") 'yank)
  (global-set-key (kbd "s-c") 'evil-yank)
  (global-set-key (kbd "s-a") 'mark-whole-buffer)
  (global-set-key (kbd "s-x") 'kill-region)
  (global-set-key (kbd "s-w") 'delete-window)
  (global-set-key (kbd "s-W") 'delete-frame)
  (global-set-key (kbd "s-d") 'kill-line)
  (global-set-key (kbd "s-z") 'undo-tree-undo)
  (global-set-key (kbd "s-Z") 'undo-tree-redo)
  (global-set-key (kbd "s-s") 'save-buffer)
  (global-set-key (kbd "C-s-n") 'move-text-down)
  (global-set-key (kbd "C-s-p") 'move-text-up))

;; ido specific keybindings
(define-key ido-common-completion-map (kbd "C-p") 'ido-prev-match)
(define-key ido-common-completion-map (kbd "C-n") 'ido-next-match)
(define-key ido-common-completion-map (kbd "C-k") 'ido-prev-match)
(define-key ido-common-completion-map (kbd "C-j") 'ido-next-match)
