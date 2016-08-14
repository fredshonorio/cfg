;;; packages
(prelude-require-packages
 '(restclient monokai-theme))

;;; theme
(load-theme 'monokai t)

;;; fonts
(add-to-list 'default-frame-alist '(font . "DejaVu Sans Mono-10"))
