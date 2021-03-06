;;; packages.el --- darcylee Layer packages File for Spacemacs
;;
;; Copyright (c) 2014-2016 darcylee
;;
;; Author: darcylee <darcylee1986@gmail.com>
;; URL: https://github.com/darcylee/spacemacs-config
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;; List of all packages to install and/or initialize. Built-in packages
;; which require an initialization must be listed explicitly in the list.

(setq darcylee-programming-packages
      '(
        css-mode
        paredit
        lispy
        cmake-font-lock
        cmake-mode
        flycheck
        (nodejs-repl-eval :location local)
        (doxymacs :location local)
        js2-mode
        js2-refactor
        json-mode
        racket-mode
        yasnippet
        web-mode
        js-doc
        lua-mode
        ;; (cc-mode :location built-in)
        cc-mode
        (python :location built-in)
        (emacs-lisp :location built-in)
        company
        (eldoc :location built-in)
        dumb-jump
        graphviz-dot-mode
        cider
        ;; editorconfig
        robe
        (kconfig :location local)
        plantuml-mode
        (ediff :location built-in)
        (magit-gerrit :location (recipe :fetcher github :repo "darcylee/magit-gerrit"))
        ;; (magit-gerrit :location "/private/work/extra/magit-gerrit/")
        (smerge-mode :location built-in)
        (robot-mode :location (recipe :fetcher github :repo "jstvz/robot-mode"))
        ))

(defun darcylee-programming/post-init-robe ()
  (progn
    (add-hook 'inf-ruby-mode-hook 'spacemacs/toggle-auto-completion-on)
    (defun darcylee/ruby-send-current-line (&optional print)
      "Send the current line to the inferior Ruby process."
      (interactive "P")
      (ruby-send-region
       (line-beginning-position)
       (line-end-position))
      (when print (ruby-print-result)))

    (defun darcylee/ruby-send-current-line-and-go ()
      (interactive)
      (darcylee/ruby-send-current-line)
      (ruby-switch-to-inf t))

    (defun darcylee/start-inf-ruby-and-robe ()
      (interactive)
      (when (not (get-buffer "*ruby*"))
        (inf-ruby))
      (robe-start))

    (dolist (mode '(ruby-mode enh-ruby-mode))
      (spacemacs/set-leader-keys-for-major-mode mode
        "sb" 'ruby-send-block
        "sB" 'ruby-send-buffer
        "sl" 'darcylee/ruby-send-current-line
        "sL" 'darcylee/ruby-send-current-line-and-go
        "sI" 'darcylee/start-inf-ruby-and-robe))))

(defun darcylee-programming/init-editorconfig ()
  (use-package editorconfig
    :init
    (progn
      (defun conditional-enable-editorconfig ()
        (if (and (darcylee/vcs-project-root)
                 (locate-dominating-file default-directory ".editorconfig"))
            (editorconfig-apply)))
      (add-hook 'prog-mode-hook 'conditional-enable-editorconfig))))

(defun darcylee-programming/post-init-cider ()
  (setq cider-cljs-lein-repl
        "(do (require 'figwheel-sidecar.repl-api)
           (figwheel-sidecar.repl-api/start-figwheel!)
           (figwheel-sidecar.repl-api/cljs-repl))")

  (defun darcylee/cider-figwheel-repl ()
    (interactive)
    (save-some-buffers)
    (with-current-buffer (cider-current-repl-buffer)
      (goto-char (point-max))
      (insert "(require 'figwheel-sidecar.repl-api)
             (figwheel-sidecar.repl-api/start-figwheel!) ; idempotent
             (figwheel-sidecar.repl-api/cljs-repl)")
      (cider-repl-return)))

  (global-set-key (kbd "C-c C-f") #'darcylee/cider-figwheel-repl))

(defun darcylee-programming/post-init-graphviz-dot-mode ()
  (with-eval-after-load 'graphviz-dot-mode
      (require 'company-keywords)
      (push '(graphviz-dot-mode  "digraph" "node" "shape" "subgraph" "label" "edge" "bgcolor" "style" "record") company-keywords-alist)))

(defun darcylee-programming/post-init-dumb-jump ()
  (setq dumb-jump-selector 'ivy)
  (defun my-dumb-jump ()
    (interactive)
    (evil-set-jump)
    (dumb-jump-go))
  (global-set-key (kbd "C-s-g") 'my-dumb-jump))

(defun darcylee-programming/post-init-emacs-lisp ()
    (remove-hook 'emacs-lisp-mode-hook 'auto-compile-mode))

(defun darcylee-programming/post-init-python ()
  (add-hook 'python-mode-hook #'(lambda () (modify-syntax-entry ?_ "w")))
  ;; if you use pyton3, then you could comment the following line
  (setq python-shell-interpreter "python"))

(defun darcylee-programming/post-init-js-doc ()
  (setq js-doc-mail-address 'user-mail-address
        js-doc-author (format "Darcy Lee <%s>" js-doc-mail-address)
        js-doc-url 'user-home-page
        js-doc-license "MIT")

 (defun my-js-doc-insert-function-doc-snippet ()
    "Insert JsDoc style comment of the function with yasnippet."
    (interactive)

    (with-eval-after-load 'yasnippet
      (js-doc--beginning-of-defun)

      (let ((metadata (js-doc--function-doc-metadata))
            (field-count 1))
        (yas-expand-snippet
         (concat
          js-doc-top-line
          " * ${1:Function description.}\n"
          (format "* @method %s\n" (nth-value 1 (split-string (which-function) "\\.")))
          (mapconcat (lambda (param)
                       (format
                        " * @param {${%d:Type of %s}} %s - ${%d:Parameter description.}\n"
                        (incf field-count)
                        param
                        param
                        (incf field-count)))
                     (cdr (assoc 'params metadata))
                     "")
          (when (assoc 'returns metadata)
            (format
             " * @returns {${%d:Return Type}} ${%d:Return description.}\n"
             (incf field-count)
             (incf field-count)))
          (when (assoc 'throws metadata)
            (format
             " * @throws {${%d:Exception Type}} ${%d:Exception description.}\n"
             (incf field-count)
             (incf field-count)))
          js-doc-bottom-line))))))


(defun darcylee-programming/init-ctags-update ()
  (use-package ctags-update
    :init
    :defer t
    :config
    (spacemacs|hide-lighter ctags-auto-update-mode)))

(defun darcylee-programming/post-init-web-mode ()
  (with-eval-after-load "web-mode"
    (web-mode-toggle-current-element-highlight)
    (web-mode-dom-errors-show))
  (setq company-backends-web-mode '((company-dabbrev-code
                                     company-keywords
                                     company-etags)
                                    company-files company-dabbrev)))

(defun darcylee-programming/post-init-yasnippet ()
  (progn
    (set-face-background 'secondary-selection "gray")
    (setq-default yas-prompt-functions '(yas-ido-prompt yas-dropdown-prompt))
    (mapc #'(lambda (hook) (remove-hook hook 'spacemacs/load-yasnippet)) '(prog-mode-hook
                                                                      org-mode-hook
                                                                      markdown-mode-hook))

    (spacemacs/add-to-hooks 'darcylee/load-yasnippet '(prog-mode-hook
                                                       markdown-mode-hook
                                                       org-mode-hook))
    ))

(defun darcylee-programming/post-init-racket-mode ()
  (progn
    (eval-after-load 'racket-repl-mode
      '(progn
         (define-key racket-repl-mode-map (kbd "]") nil)
         (define-key racket-repl-mode-map (kbd "[") nil)))

    (add-hook 'racket-mode-hook #'(lambda () (lispy-mode 1)))
    (add-hook 'racket-repl-mode-hook #'(lambda () (lispy-mode t)))
    ;; (add-hook 'racket-repl-mode-hook #'(lambda () (smartparens-mode t)))
    ))

(defun darcylee-programming/post-init-json-mode ()
  (add-to-list 'auto-mode-alist '("\\.tern-project\\'" . json-mode))
  (add-to-list 'auto-mode-alist '("\\.fire\\'" . json-mode))
  (add-to-list 'auto-mode-alist '("\\.fire.meta\\'" . json-mode))
  (spacemacs/set-leader-keys-for-major-mode 'json-mode
    "ti" 'my-toggle-web-indent))

(defun darcylee-programming/init-lispy ()
  (use-package lispy
    :defer t
    :diminish (lispy-mode)
    :init
    (progn

      (add-hook 'emacs-lisp-mode-hook (lambda () (lispy-mode 1)))
      (add-hook 'ielm-mode-hook (lambda () (lispy-mode 1)))
      (add-hook 'inferior-emacs-lisp-mode-hook (lambda () (lispy-mode 1)))
      ;; (add-hook 'spacemacs-mode-hook (lambda () (lispy-mode 1)))
      (add-hook 'scheme-mode-hook (lambda () (lispy-mode 1)))
      (add-hook 'cider-repl-mode-hook (lambda () (lispy-mode 1)))
      )
    :config
    (progn
      (push '(cider-repl-mode . ("[`'~@]+" "#" "#\\?@?")) lispy-parens-preceding-syntax-alist)

      (define-key lispy-mode-map (kbd "s-j") 'lispy-splice)
      (define-key lispy-mode-map (kbd "s-k") 'paredit-splice-sexp-killing-backward)

      (with-eval-after-load 'cider-repl
        (define-key cider-repl-mode-map (kbd "C-s-j") 'cider-repl-newline-and-indent))

      (add-hook
       'minibuffer-setup-hook
       'conditionally-enable-lispy)
      (define-key lispy-mode-map (kbd "s-m") 'lispy-mark-symbol)
      (define-key lispy-mode-map (kbd "s-1") 'lispy-describe-inline)
      (define-key lispy-mode-map (kbd "s-2") 'lispy-arglist-inline))))


(defun darcylee-programming/init-cmake-font-lock ()
  (use-package cmake-font-lock
    :defer t))

;; (defun darcylee-programming/init-google-c-style ()
;;   (use-package google-c-style
;;     :init (add-hook 'c-mode-common-hook 'google-set-c-style)))

(defun darcylee-programming/post-init-cmake-mode ()
  (progn
    (spacemacs/declare-prefix-for-mode 'cmake-mode
                                       "mh" "docs")
    (spacemacs/set-leader-keys-for-major-mode 'cmake-mode
      "hd" 'cmake-help)
    (add-hook 'cmake-mode-hook (function cmake-rename-buffer))))


(defun darcylee-programming/post-init-flycheck ()
  (with-eval-after-load 'flycheck
    (progn
      (setq flycheck-display-errors-delay 0.9)
      (setq flycheck-idle-change-delay 2.0)
      )))

(defun darcylee-programming/post-init-eldoc ()
  (setq eldoc-idle-delay 0.4))


;; (defun darcylee-programming/init-impatient-mode ()
;;   "Initialize impatient mode"
;;   (use-package impatient-mode
;;     :init
;;     (progn
;;       (add-hook 'web-mode-hook 'darcylee/impatient-mode-hook)
;;       (spacemacs/set-leader-keys-for-major-mode 'web-mode
;;         "p" 'imp-visit-buffer)
;;       )))




(defun darcylee-programming/post-init-js2-refactor ()
  (progn
    (spacemacs/set-leader-keys-for-major-mode 'js2-mode
      "r>" 'js2r-forward-slurp
      "r<" 'js2r-forward-barf)))

(defun darcylee-programming/post-init-js2-mode ()
  (progn
    (add-hook 'js2-mode-hook 'my-setup-develop-environment)
    (add-hook 'web-mode-hook 'my-setup-develop-environment)

    (spacemacs|define-jump-handlers js2-mode)

    (setq company-backends-js2-mode '((company-dabbrev-code :with company-keywords company-etags)
                                      company-files company-dabbrev))

    (darcylee|toggle-company-backends company-tern)

    (spacemacs/set-leader-keys-for-major-mode 'js2-mode
      "tb" 'zilong/company-toggle-company-tern)

    (add-hook 'js2-mode-hook 'my-js2-mode-hook)

    ;; add your own keywords highlight here
    (font-lock-add-keywords 'js2-mode
                            '(("\\<\\(cc\\)\\>" 1 font-lock-type-face)))

    (spacemacs/declare-prefix-for-mode 'js2-mode "ms" "repl")

    (with-eval-after-load 'js2-mode
      (progn
        ;; these mode related variables must be in eval-after-load
        ;; https://github.com/magnars/.emacs.d/blob/master/settings/setup-js2-mode.el
        (setq-default js2-allow-rhino-new-expr-initializer nil)
        (setq-default js2-auto-indent-p nil)
        (setq-default js2-enter-indents-newline nil)
        (setq-default js2-global-externs '("module" "ccui" "require" "buster" "sinon" "assert" "refute" "setTimeout" "clearTimeout" "setInterval" "clearInterval" "location" "__dirname" "console" "JSON"))
        (setq-default js2-idle-timer-delay 0.2)
        (setq-default js2-mirror-mode nil)
        (setq-default js2-strict-inconsistent-return-warning nil)
        (setq-default js2-include-rhino-externs nil)
        (setq-default js2-include-gears-externs nil)
        (setq-default js2-concat-multiline-strings 'eol)
        (setq-default js2-rebind-eol-bol-keys nil)
        (setq-default js2-auto-indent-p t)

        (setq-default js2-bounce-indent nil)
        (setq-default js-indent-level 4)
        (setq-default js2-basic-offset 4)
        (setq-default js-switch-indent-offset 4)
        ;; Let flycheck handle parse errors
        (setq-default js2-mode-show-parse-errors nil)
        (setq-default js2-mode-show-strict-warnings nil)
        (setq-default js2-highlight-external-variables t)
        (setq-default js2-strict-trailing-comma-warning nil)

        (add-hook 'web-mode-hook 'my-web-mode-indent-setup)

        (spacemacs/set-leader-keys-for-major-mode 'js2-mode
          "ti" 'my-toggle-web-indent)
        (spacemacs/set-leader-keys-for-major-mode 'js-mode
          "ti" 'my-toggle-web-indent)
        (spacemacs/set-leader-keys-for-major-mode 'web-mode
          "ti" 'my-toggle-web-indent)
        (spacemacs/set-leader-keys-for-major-mode 'css-mode
          "ti" 'my-toggle-web-indent)

        (spacemacs/declare-prefix-for-mode 'js2-mode "mt" "toggle")
        (spacemacs/declare-prefix-for-mode 'js-mode "mt" "toggle")
        (spacemacs/declare-prefix-for-mode 'web-mode "mt" "toggle")
        (spacemacs/declare-prefix-for-mode 'css-mode "mt" "toggle")


        (eval-after-load 'tern-mode
          '(spacemacs|hide-lighter tern-mode))
        ))

    (evilified-state-evilify js2-error-buffer-mode js2-error-buffer-mode-map)

    ))

(defun darcylee-programming/post-init-css-mode ()
  (progn
    (dolist (hook '(css-mode-hook sass-mode-hook less-mode-hook))
      (add-hook hook 'rainbow-mode))

    (defun css-imenu-make-index ()
      (save-excursion
        (imenu--generic-function '((nil "^ *\\([^ ]+\\) *{ *$" 1)))))

    (add-hook 'css-mode-hook
              (lambda ()
                (setq imenu-create-index-function 'css-imenu-make-index)))))

(defun darcylee-programming/post-init-tagedit ()
  (add-hook 'web-mode-hook (lambda () (tagedit-mode 1))))

;; For each extension, define a function darcylee/init-<extension-name>
;;
(defun darcylee-programming/init-doxymacs ()
  "Initialize doxymacs"
  (use-package doxymacs
    :demand t
    :init
    :config
    (progn
      (add-hook 'font-lock-mode-hook 'my-doxymacs-font-lock-hook)
      (add-hook 'c-mode-common-hook 'doxymacs-mode)
      (define-key doxymacs-mode-map "\C-cdF"
        'doxymacs-insert-file-all-comment)
      (spacemacs|hide-lighter doxymacs-mode))))

;; https://atlanis.net/blog/posts/nodejs-repl-eval.html
(defun darcylee-programming/init-nodejs-repl-eval ()
  (use-package nodejs-repl-eval
    :commands (nodejs-repl-eval-buffer nodejs-repl-eval-dwim nodejs-repl-eval-function)
    :init
    (progn
      (spacemacs/declare-prefix-for-mode 'js2-mode
                                         "ms" "REPL")
      (spacemacs/set-leader-keys-for-major-mode 'js2-mode
        "sb" 'nodejs-repl-eval-buffer
        "sf" 'nodejs-repl-eval-function
        "sd" 'nodejs-repl-eval-dwim))
    :defer t
    ))

(defun darcylee-programming/post-init-lua-mode ()
  (progn
    (add-hook 'lua-mode-hook 'evil-matchit-mode)
    ;; (add-hook 'lua-mode-hook 'smartparens-mode)
    ;; (setq lua-indent-level 2)

;;; add lua language, basic, string and table keywords.
    (with-eval-after-load 'lua-mode
      (require 'company-keywords)
      (push '(lua-mode  "setmetatable" "local" "function" "and" "break" "do" "else" "elseif" "self" "resume" "yield"
                        "end" "false" "for" "function" "goto" "if" "nil" "not" "or" "repeat" "return" "then" "true"
                        "until" "while" "__index" "dofile" "getmetatable" "ipairs" "pairs" "print" "rawget" "status"
                        "rawset" "select" "_G" "assert" "collectgarbage" "error" "pcall" "coroutine"
                        "rawequal" "require" "load" "tostring" "tonumber" "xpcall" "gmatch" "gsub"
                        "rep" "reverse" "sub" "upper" "concat" "pack" "insert" "remove" "unpack" "sort"
                        "lower") company-keywords-alist))

    ))

(defun darcylee-programming/post-init-plantuml-mode ()
  (progn
    (with-eval-after-load 'plantuml-mode
      (require 'company-keywords)
      (push '(plantuml-mode "participant" "actor" "boundary" "control" "entity" "database" "collections"
                            "autonumber" "resume" "newpage" "alt" "else" "opt" "loop" "par" "break" "critical"
                            "group" "title" "start" "end" "note" "hnote" "rnote" "ref" "activate" "deactivate"
                            "destroy" "create" "skinparam" "guillenmet" "box" "hide" "footbox" "handwritten"
                            "sequence" "BoxPadding" "ParticipantPadding" "usecase" "static" "abstract" "show"
                            "empty" "fields" "attributes" "class" "enum" "interface" "packageStyle" "rectangle"
                            "package" "node" "folder" "frame" "cloud" "repeat" "startuml" "salt" "startsalt"
                            "legend" "sprite" "scale" "monochrome" "define" "component" "caption" "state"
                            ) company-keywords-alist))
    ))

(defun darcylee-programming/post-init-cc-mode ()
  (progn
    (setq company-backends-c-mode-common '((company-dabbrev-code :with company-keywords company-gtags company-etags)
                                           company-files company-dabbrev))

    (add-hook 'c++-mode-hook 'my-setup-develop-environment)
    (add-hook 'c-mode-hook 'my-setup-develop-environment)


    ;; http://stackoverflow.com/questions/23553881/emacs-indenting-of-c11-lambda-functions-cc-mode
    ;; (defadvice c-lineup-arglist (around my activate)
    ;;   "Improve indentation of continued C++11 lambda function opened as argument."
    ;;   (setq ad-return-value
    ;;         (if (and (equal major-mode 'c++-mode)
    ;;                  (ignore-errors
    ;;                    (save-excursion
    ;;                      (goto-char (c-langelem-pos langelem))
    ;;                      ;; Detect "[...](" or "[...]{". preceded by "," or "(",
    ;;                      ;;   and with unclosed brace.
    ;;                      (looking-at ".*[(,][ \t]*\\[[^]]*\\][ \t]*[({][^}]*$"))))
    ;;             0                       ; no additional indent
    ;;           ad-do-it)))               ; default behavior
    )

  )

(defun darcylee-programming/init-gulpjs ()
  (use-package gulpjs
    :init
    (progn
      (defun zilong/build-engine ()
        (interactive)
        (gulpjs-start-task-with-file-name "~/Github/fireball/app.js"))

      (spacemacs/set-leader-keys "ags" 'gulpjs-start-task)
      (spacemacs/set-leader-keys "agS" 'zilong/build-engine)
      (spacemacs/set-leader-keys "agr" 'gulpjs-restart-task))))


(defun darcylee-programming/init-paredit ()
  (use-package paredit
    :commands (paredit-wrap-round
               paredit-wrap-square
               paredit-wrap-curly
               paredit-splice-sexp-killing-backward)
    :init
    (progn

      (bind-key* "s-(" #'paredit-wrap-round)
      (bind-key* "s-[" #'paredit-wrap-square)
      (bind-key* "s-{" #'paredit-wrap-curly))))

(defun darcylee-programming/post-init-company ()
  (progn
    (setq company-minimum-prefix-length 2
          company-idle-delay 0.08)

    (when (configuration-layer/package-usedp 'company)
      (spacemacs|add-company-backends :modes shell-script-mode makefile-bsdmake-mode sh-mode lua-mode nxml-mode
                                      conf-unix-mode json-mode graphviz-dot-mode go-mode makefile-gmake-mode
                                      plantuml-mode))))

(defun darcylee-programming/post-init-company-c-headers ()
  (progn
    (setq company-c-headers-path-system
          (quote
           ("/usr/include/" "/usr/local/include/" "/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/include/c++/v1")))
    (setq company-c-headers-path-user
          (quote
           ("." "/private/work/tmp/include")))))

(defun darcylee-programming/init-kconfig ()
  (use-package kconfig))

(defun darcylee-programming/post-init-ediff ()
  (progn
    (add-hook 'ediff-keymap-setup-hook
              (lambda ()
                ;; modify help message
                (dolist (msg '(ediff-long-help-message-compare2
                               ediff-long-help-message-compare3
                               ediff-long-help-message-narrow2
                               ediff-long-help-message-word-mode
                               ediff-long-help-message-merge
                               ediff-long-help-message-head
                               ediff-long-help-message-tail))
                  (dolist (chng '(;;("^" . "  ")
                                  ("p,DEL -previous diff " . "  k,p -previous diff ")
                                  ("n,SPC -next diff     " . "  j,n -next diff     ")
                                  ("    j -jump to diff  " . "    d -jump to diff  ")
                                  ))
                    (setf (symbol-value msg)
                          (replace-regexp-in-string (car chng) (cdr chng) (symbol-value msg)))))

                ;; modify some kbd.
                (define-key ediff-mode-map "d" 'ediff-jump-to-difference)
                (define-key ediff-mode-map "j" 'ediff-next-difference)
                (define-key ediff-mode-map "k" 'ediff-previous-difference)))
    (setq ediff-diff-options "-ra")
    (setq ediff-custom-diff-options "-ra")))

(defun darcylee-programming/init-magit-gerrit ()
  (use-package magit-gerrit
    :init
    :config
    (progn
      (setq-default magit-gerrit-ssh-creds "lijiaquan@gerrit.ruijie.work")
      (add-to-list 'magit-gerrit-review-labels (list "PSTL-Check" "PC") t)
      ;; (add-to-list 'magit-gerrit-review-labels (list "All-Comments-Resolved" "ACR") t)

      (setq-default magit-gerrit-show-review-labels t)
      )))

(defun darcylee-programming/init-smerge-mode ()
  (use-package smerge-mode
    :defer t
    :diminish smerge-mode
    :commands spacemacs/smerge-transient-state/body
    :init
    (spacemacs/set-leader-keys
      "gr" 'spacemacs/smerge-transient-state/body)
    :config
    (progn
      (spacemacs|define-transient-state smerge
        :title "Smerge Transient State"
        :doc "
 Movement^^^^         Merge Action^^      Diff^^            Other
 ---------------^^^^  ----------------^^  --------------^^  ---------------------------^^
 [_n_]^^   next hunk  [_b_] keep base     [_<_] base/mine   [_C_] combine curr/next hunks
 [_N_/_p_] prev hunk  [_m_] keep mine     [_=_] mine/other  [_u_] undo
 [_j_]^^   next line  [_a_] keep all      [_>_] base/other  [_q_] quit
 [_k_]^^   prev line  [_o_] keep other    [_r_] refine
 ^^^^                 [_c_] keep current  [_e_] ediff
 ^^^^                 [_K_] kill current"
        :bindings
        ;; move
        ("n" smerge-next)
        ("N" smerge-prev)
        ("p" smerge-prev)
        ("j" evil-next-line)
        ("k" evil-previous-line)
        ;; merge action
        ("b" smerge-keep-base)
        ("m" smerge-keep-mine)
        ("a" smerge-keep-all)
        ("o" smerge-keep-other)
        ("c" smerge-keep-current)
        ;; diff
        ("<" smerge-diff-base-mine)
        ("=" smerge-diff-mine-other)
        (">" smerge-diff-base-other)
        ("r" smerge-refine)
        ("e" smerge-ediff :exit t)
        ;; other
        ("C" smerge-combine-with-next)
        ("K" smerge-kill-current)
        ("u" undo-tree-undo)
        ("q" nil :exit t)))))

(defun darcylee-programming/init-robot-mode ()
  "robot framework mode init"
  (use-package robot-mode
    :config
    (progn
      (define-key robot-mode-map [remap evil-indent] 'robot-mode-indent-region))))
