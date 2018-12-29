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

(define-key global-map (kbd "C-c y") 'youdao-dictionary-search-at-point+)
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
    ;; (bb/define-key company-active-map (kbd "C-f") nil)

    (bb/define-key company-active-map (kbd "C-n") 'company-select-next)
    (bb/define-key company-active-map (kbd "C-p") 'company-select-previous-or-abort)
    (bb/define-key company-active-map (kbd "C-b") 'company-previous-page)
    (bb/define-key company-active-map (kbd "C-f") 'company-next-page)
    (bb/define-key company-active-map (kbd "s-w") 'company-show-location)))

(spacemacs/declare-prefix "ot" "Toggle")


(if (configuration-layer/layer-usedp 'helm)
    (progn (global-set-key (kbd "<f1>") 'zilongshanren/helm-hotspots)
           (spacemacs/set-leader-keys "oo" 'zilongshanren/helm-hotspots)))

(spacemacs/set-leader-keys "fF" 'find-file-in-project)
(spacemacs/set-leader-keys "oc" 'my-auto-update-tags-when-save)
(spacemacs/set-leader-keys "op" 'zilongshanren/org-save-and-export)
(spacemacs/set-leader-keys "fR" 'darcylee/rename-file-and-buffer)

;;Must set key to nil to prevent error: Key sequence b m s starts with non-prefix key b m
(spacemacs/set-leader-keys "bm" nil)
(spacemacs/set-leader-keys "bD" 'spacemacs/kill-other-buffers)
(spacemacs/declare-prefix "bm" "Bookmark")
(spacemacs/set-leader-keys "bms" 'bookmark-set)
(spacemacs/set-leader-keys "bmr" 'bookmark-rename)
(spacemacs/set-leader-keys "bmd" 'bookmark-delete)
(spacemacs/set-leader-keys "bmj" 'bookmark-jump)
;; temp fix for ivy-switch-buffer
;; (spacemacs/set-leader-keys "bb" 'ivy-switch-buffer)

(spacemacs/set-leader-keys "od" 'occur-dwim)
(spacemacs/set-leader-keys "ox" 'org-open-at-point)
(spacemacs/set-leader-keys "oac" 'zilongshanren/browser-refresh--chrome-applescript)

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

(spacemacs/set-leader-keys "sj" 'helm-imenu)

(spacemacs/set-leader-keys "if" 'spacemacs/insert-file)
(spacemacs/set-leader-keys "ib" 'insert-buffer)

(when (or (spacemacs/system-is-linux)
          (spacemacs/system-is-mswindows))
  (global-set-key(kbd "C-w") 'evil-delete-backward-word)
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
