(defvar org-agenda-dir ""
  "gtd org files location")

(defvar deft-dir ""
  "deft org files locaiton")

(defvar blog-admin-dir ""
  "blog-admin files location")

(defvar user-home-page
  "http://www.gitlee.com")

(setq-default
 user-full-name "darcylee"
 user-mail-address "darcylee1986@gmail.com")

(setq-default
 org-agenda-dir "~/Documents/org-notes"
 deft-dir "~/Documents/org-notes"
 blog-admin-dir "~/4gamers.cn")

;;load sensitive data
;; or (like spacemacs init.el)put the above variable into it ,then the own value separated from public config
;; .emacs.secrets.el for example:
;; (setq-default
;;  org-agenda-dir "~/Dropbox/Apps/emacs/gtd"
;;  deft-dir "~/Dropbox/Apps/emacs/notes"
;;  blog-admin-dir "~/Documents/hexo"
;;  )
;; (slack-register-team
;;   :name "emacs-slack"
;;   :default t
;;   :client-id "xxxxxxxxx"
;;   :client-secret "xxxxxxxxx"
;;   :token "xxxxxxxxx"
;;   :subscribed-channels '(xxxxxxxxx))
;; (setq paradox-github-token "")
;; (load "~/Dropbox/Apps/emacs/.emacs.secrets.el" t)

;;load sensitive data
(setq config-secrets (expand-file-name ".secrets.el" dotspacemacs-directory))
(load config-secrets 'no-error 'no-messge t)

;;load local configs
(setq config-local (expand-file-name ".config-local.el" dotspacemacs-directory))
(load config-local 'no-error 'no-messge t)
