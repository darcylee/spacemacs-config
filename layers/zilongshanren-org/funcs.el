;;; funcs.el --- zilongshanren Layer packages File for Spacemacs
;;
;; Copyright (c) 2015-2016 zilongshanren
;;
;; Author: zilongshanren <guanghui8827@gmail.com>
;; URL: https://github.com/zilongshanren/spacemacs-private
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
(require 'cl)

(setq octopress-workdir (expand-file-name "~/4gamers.cn/"))

(defun zilongshanren/octopress-rake (command)
  "run rake commands"
  (let ((command-str (format "/bin/bash -l -c 'source $HOME/.rvm/scripts/rvm && rvm use ruby 2.0.0  && cd %s && rake %s'" octopress-workdir command)))
    (shell-command-to-string command-str)))

(defun zilongshanren/octopress-qrsync (command)
  (let ((command-str (format "/usr/local/bin/qrsync %s" command )))
    (shell-command-to-string command-str)))

(defun zilongshanren/octopress-generate ()
  "generate jekyll site"
  (interactive)
  (zilongshanren/octopress-rake "generate")
  (message "Generate site OK"))

(defun zilongshanren/octopress-deploy ()
  "default deploy task"
  (interactive)
  (zilongshanren/octopress-rake "deploy")
  (zilongshanren/octopress-qrsync "/Users/guanghui/4gamers.cn/guanghui.json")
  (message "Deploy site OK"))

(defun zilongshanren/octopress-gen-deploy ()
  "generate website and deploy"
  (interactive)
  (zilongshanren/octopress-rake "gen_deploy")
  (zilongshanren/octopress-qrsync "/Users/guanghui/4gamers.cn/guanghui.json")
  (message "Generate and Deploy OK"))

(defun zilongshanren/octopress-upimg ()
  (interactive)
  (zilongshanren/octopress-qrsync "/Users/guanghui/4gamers.cn/guanghui.json")
  (message "Up Img to Qiniu"))

(defun zilongshanren/directory-parent (directory)
  (let ((parent (file-name-directory (directory-file-name directory))))
    (if (not (equal directory parent))
        parent)))

(defun zilongshanren/jekyll-serve ()
  (interactive)
  (let* ((default-directory
           (if (string-match "_posts/$" default-directory)
               (zilongshanren/directory-parent (zilongshanren/directory-parent default-directory))
             (zilongshanren/directory-parent default-directory)))
         (buffer (if (get-buffer "*jekyll*")
                     (switch-to-buffer "*jekyll*")
                   (ansi-term "/bin/zsh" "jekyll")))
         (proc (get-buffer-process buffer)))
    (term-send-string proc "rake generate && rake preview\n")
    (sit-for 4)
    (browse-url "http://localhost:4000")))


;; Screenshot
(defun zilongshanren//insert-org-or-md-img-link (prefix imagename)
  (if (equal (file-name-extension (buffer-file-name)) "org")
      (insert (format "[[%s%s]]\n" prefix imagename))
    (insert (format "![%s](%s%s)\n" imagename prefix imagename))))

(defun zilongshanren//take-screenshot (image window)
  "Grab a window instead of the entire screen"
  (shell-command (format "mkdir -p %s" (file-name-directory image)))
  (when (spacemacs/system-is-linux)
    (lower-frame)
    (cond
     ((executable-find "gnome-screenshot")
      (if window (setq opt "-w -b --border-effect=shadow --delay=3 -f") (setq opt "-a -f"))
      (shell-command (format "gnome-screenshot %s %s" opt image)))

     ((executable-find "scrot")
      (if window (setq opt "-s -b") (setq opt "-s"))
      (shell-command (format "scrot %s %s" opt image)))

     (t
      (message "no screenshot program")))
    (raise-frame))

  (when (spacemacs/system-is-mac)
    (if window (setq opt "-w") (setq opt "-s"))
    (call-process "screencapture" nil nil nil opt image))

  (when (spacemacs/system-is-mswindows)
    (message "unsupport yet!!")))

(defun zilongshanren//insert-capture-screenshot (basename window)
  "Take a screenshot into a time stamped unique-named file in the same directory
as the org-buffer/markdown-buffer and insert a link to this file."
  (if (equal basename "")
      (setq basename (format-time-string "%Y%m%d_%H%M%S")))
  (setq blog_img_dir
        (concat (file-name-directory (buffer-file-name))
                "../img/post/"))
  (setq imagename (concat basename ".png"))
  (if (file-exists-p blog_img_dir)
      (progn
        (setq relative_dir
              (concat "/img/post/"
                      (file-name-base (buffer-file-name))
                      "/"))
        (setq absolute_path (concat blog_img_dir
                                    (file-name-base (buffer-file-name))
                                    "/"
                                    imagename))
        (zilongshanren//take-screenshot absolute_path window)
        (if (file-exists-p absolute_path)
            (progn
              ;; (if (executable-find "convert")
              ;;     (shell-command-to-string (format "convert %s -resize 800x600 %s" absolute_path absolute_path)))
              (zilongshanren//insert-org-or-md-img-link relative_dir imagename))
          (message "File: %s is not exists." absolute_path)))
    (progn
      (setq relativepathname (concat "img/" imagename))
      (zilongshanren//take-screenshot relativepathname window)
      (if (file-exists-p relativepathname)
          (zilongshanren//insert-org-or-md-img-link "./" relativepathname)
        (message "File: %s is not exists." relativepathname))))
  )

(defun zilongshanren/capture-selection (basename)
  (interactive "sScreenshot Name: ")
  (zilongshanren//insert-capture-screenshot basename nil))

(defun zilongshanren/capture-window (basename)
  (interactive "sScreenshot Name: ")
  (zilongshanren//insert-capture-screenshot basename t))

(defun zilongshanren/org-archive-done-tasks ()
  (interactive)
  (org-map-entries
   (lambda ()
     (org-archive-subtree)
     (setq org-map-continue-from (outline-previous-heading)))
   "/DONE" 'file))

(defun zilongshanren/org-archive-cancel-tasks ()
  (interactive)
  (org-map-entries
   (lambda ()
     (org-archive-subtree)
     (setq org-map-continue-from (outline-previous-heading)))
   "/CANCELLED" 'file))

;; "https://github.com/vhallac/.emacs.d/blob/master/config/customize-org-agenda.el"
(defun zilongshanren/skip-non-stuck-projects ()
  "Skip trees that are not stuck projects"
  (bh/list-sublevels-for-projects-indented)
  (save-restriction
    (widen)
    (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
      ;; VH: I changed this line from
      ;; (if (bh/is-project-p)
      (if (and (eq (point) (bh/find-project-task))
               (bh/is-project-p))
          (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
                 (has-next ))
            (save-excursion
              (forward-line 1)
              (while (and (not has-next) (< (point) subtree-end) (re-search-forward "^\\*+ NEXT " subtree-end t))
                (unless (member "WAITING" (org-get-tags-at))
                  (setq has-next t))))
            (if has-next
                next-headline
              nil)) ; a stuck project, has subtasks but no next task
        next-headline))))

(defun zilongshanren/org-insert-src-block (src-code-type)
  "Insert a `SRC-CODE-TYPE' type source code block in org-mode."
  (interactive
   (let ((src-code-types
          '("emacs-lisp" "python" "C" "sh" "java" "js" "clojure" "C++" "css"
            "calc" "asymptote" "dot" "gnuplot" "ledger" "lilypond" "mscgen"
            "octave" "oz" "plantuml" "R" "sass" "screen" "sql" "awk" "ditaa"
            "haskell" "latex" "lisp" "matlab" "ocaml" "org" "perl" "ruby"
            "scheme" "sqlite")))
     (list (ido-completing-read "Source code type: " src-code-types))))
  (progn
    (newline-and-indent)
    (insert (format "#+BEGIN_SRC %s\n" src-code-type))
    (newline-and-indent)
    (insert "#+END_SRC\n")
    (previous-line 2)
    (org-edit-src-code)))

(defun darcylee/org-insert-admonition-block (admonitions-type)
  "Insert a `ADMONITIONS' type block in org-mode."
  (interactive
   (let ((admonitions-types
          ;; "note" "warning" "tip" "caution" "important" "attention" "hit" "error"
          ;; "danger" "info" "notice" "question" "summary" "success"
          ;; we use common admonitions
          '("note" "warning" "tip" "caution" "important" "attention" "error" "danger" "abstract")))
     (list (ido-completing-read "Source code type: " admonitions-types))))
  (progn
    (newline-and-indent)
    (insert (format "#+begin_%s\n" admonitions-type))
    (newline-and-indent)
    (insert (format "#+end_%s\n" admonitions-type))
    (previous-line 2)
    (evil-insert-state)))

(defun darcylee/org-format-region-as-code-block (beg end)
  "Formate as code block"
  (interactive "*r")
  (let ((lang (read-string "Language: "))
        (ind (save-excursion
               (goto-char beg)
               (back-to-indentation)
               (buffer-substring (line-beginning-position) (point))))
        (code (delete-and-extract-region beg end)))
    (insert ind "#+BEGIN_SRC " lang "\n"
            code (if (string-suffix-p "\n" code) "" "\n")
            ind "#+END_SRC\n")))

(defun darcylee/org-format-region-as-admonition-block (beg end)
  "Formate as code block"
  (interactive "*r")
  (let ((type (read-string "which admonition type(\"note\" \"warning\" \"tip\" \"caution\" \"important\" \"attention\" \"error\" \"danger\"): "))
        (code (delete-and-extract-region beg end)))
    (progn
      (newline)
      (insert (format "#+begin_%s\n" type)
              code (if (string-suffix-p "\n" code) "" "\n")
              (format "#+end_%s\n" type))
      (newline)
      )))

(defun org-reset-subtask-state-subtree ()
  "Reset all subtasks in an entry subtree."
  (interactive "*")
  (if (org-before-first-heading-p)
      (error "Not inside a tree")
    (save-excursion
      (save-restriction
        (org-narrow-to-subtree)
        (org-show-subtree)
        (goto-char (point-min))
        (beginning-of-line 2)
        (narrow-to-region (point) (point-max))
        (org-map-entries
         '(when (member (org-get-todo-state) org-done-keywords)
            (org-todo (car org-todo-keywords))))))))

(defun org-reset-subtask-state-maybe ()
  "Reset all subtasks in an entry if the `RESET_SUBTASKS' property is set"
  (interactive "*")
  (if (org-entry-get (point) "RESET_SUBTASKS")
      (org-reset-subtask-state-subtree)))

(defun org-subtask-reset ()
  (when (member org-state org-done-keywords) ;; org-state dynamically bound in org.el/org-todo
    (org-reset-subtask-state-maybe)
    (org-update-statistics-cookies t)))

(defun zilong/org-summary-todo (n-done n-not-done)
  "Switch entry to DONE when all subentries are done, to TODO otherwise."
  (let (org-log-done org-log-states)    ; turn off logging
    (org-todo (if (= n-not-done 0) "DONE" "TODO"))))

(defun zilong/filter-by-tags ()
  (let ((head-tags (org-get-tags-at)))
    (member current-tag head-tags)))

(defun zilong/org-clock-sum-today-by-tags (timerange &optional tstart tend noinsert)
  (interactive "P")
  (let* ((timerange-numeric-value (prefix-numeric-value timerange))
         (files (org-add-archive-files (org-agenda-files)))
         (include-tags '("WORK" "EMACS" "DREAM" "WRITING" "MEETING"
                         "LIFE" "PROJECT" "OTHER"))
         (tags-time-alist (mapcar (lambda (tag) `(,tag . 0)) include-tags))
         (output-string "")
         (tstart (or tstart
                     (and timerange (equal timerange-numeric-value 4) (- (org-time-today) 86400))
                     (and timerange (equal timerange-numeric-value 16) (org-read-date nil nil nil "Start Date/Time:"))
                     (org-time-today)))
         (tend (or tend
                   (and timerange (equal timerange-numeric-value 16) (org-read-date nil nil nil "End Date/Time:"))
                   (+ tstart 86400)))
         h m file item prompt donesomething)
    (while (setq file (pop files))
      (setq org-agenda-buffer (if (file-exists-p file)
                                  (org-get-agenda-file-buffer file)
                                (error "No such file %s" file)))
      (with-current-buffer org-agenda-buffer
        (dolist (current-tag include-tags)
          (org-clock-sum tstart tend 'zilong/filter-by-tags)
          (setcdr (assoc current-tag tags-time-alist)
                  (+ org-clock-file-total-minutes (cdr (assoc current-tag tags-time-alist)))))))
    (while (setq item (pop tags-time-alist))
      (unless (equal (cdr item) 0)
        (setq donesomething t)
        (setq h (/ (cdr item) 60)
              m (- (cdr item) (* 60 h)))
        (setq output-string (concat output-string (format "[-%s-] %.2d:%.2d\n" (car item) h m)))))
    (unless donesomething
      (setq output-string (concat output-string "[-Nothing-] Done nothing!!!\n")))
    (unless noinsert
      (insert output-string))
    output-string))

(defun darchlee/org-journal-search-fun (str)
  "Search for a string within all entries."
  (interactive
   (list
    (ivy-completing-read
     "Enter a string to search for: "
     org-journal-search-history nil nil nil 'org-journal-search-history)))
  (org-journal-search-forever str))

;;; funcs.el ends here
