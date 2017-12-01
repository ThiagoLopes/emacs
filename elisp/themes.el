;;; package --- Main init file
;;; Commentary:
;;; This is my init file

;;; Code:

;; Monoaki Alt
(require-package 'monokai-alt-theme)

;; Aftermoon
(require-package 'afternoon-theme)

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
(load-theme 'afternoon t)

;; Change font
(set-face-attribute 'default nil :family "Hack" :height 100)

(provide 'themes)
;;; themes.el ends here
