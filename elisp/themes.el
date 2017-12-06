;;; package --- Main init file
;;; Commentary:
;;; This is my init file

;;; Code:

;; Monoaki Alt
(use-package monokai-alt-theme
  :disabled)

;; Aftermoon
(use-package afternoon-theme)

;; Monokai
(use-package monokai-theme
  :disabled)

;; Sanityinc theme
(use-package color-theme-sanityinc-tomorrow
  :disabled)

;; Doom theme
(use-package doom-themes
  :config
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (doom-themes-visual-bell-config)
  :disabled
  )

;; Iniciando powerline
(use-package powerline
  :init
  (powerline-default-theme))

;; Change font
(set-face-attribute 'default nil :family "Hack" :height 90)

(provide 'themes)
;;; themes.el ends here
