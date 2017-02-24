;;; packages
(prelude-require-packages
 '(restclient monokai-theme terraform-mode))

;;; theme
(load-theme 'monokai t)

;;; fonts
(add-to-list 'default-frame-alist '(font . "Droid Sans Mono-11"))

;;; org-mode
;; don't use diff font in headers
(custom-set-variables
 '(org-level-color-stars-only t))

;; add time below a TODO when it's marked as DONE
(setq org-log-done 'time)
