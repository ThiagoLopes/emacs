;;; package --- Main init file
;;; Commentary:
;;; This is my init file

;;; Code:

;; Monoaki Alt
(require-package 'monokai-alt-theme)

;; Monokai
(require-package 'monokai-theme)

;; Sanityinc theme
(require-package 'color-theme-sanityinc-tomorrow)

;; Doom theme
(require-package 'doom-themes)
(setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
      doom-themes-enable-italic t) ; if nil, italics is universally disabled
(doom-themes-visual-bell-config)

;; Iniciando powerline
(require-package 'powerline)
(powerline-default-theme)

;; Load theme, change your theme
(load-theme 'doom-molokai t)

(provide 'themes)
;;; themes.el ends here
