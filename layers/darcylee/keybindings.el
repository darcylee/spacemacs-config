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

(when (spacemacs/system-is-mswindows)
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
  (global-set-key (kbd "s-n") 'make-frame)
  (global-set-key (kbd "s-z") 'undo-tree-undo)
  (global-set-key (kbd "s-Z") 'undo-tree-redo)
  (global-set-key (kbd "s-s") 'save-buffer))
