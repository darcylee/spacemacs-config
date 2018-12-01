(set-language-environment "Chinese-GB18030")

(setq custom-gtags-lib-path (concat "/path1:/path2"))

(setq bug-reference-url-format "http://url=%s")

(setq-default
 user-mail-address "xxx@xxx.xx"
 user-full-name "your name")

(spacemacs|define-custom-layout "@xx"
  :binding "x"
  :body
  (find-file "~/main.c"))
