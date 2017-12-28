;;; package --- Main init file
;;; Commentary:
;;; This is my init file

;;; Code:

;; Monoaki Alt
(use-package monokai-alt-theme
  :disabled)

;; Aftermoon
(use-package afternoon-theme
  :disabled)

;; Monokai
(use-package monokai-theme
  :disabled)

;; Sanityinc
(use-package color-theme-sanityinc-tomorrow
  :disabled)

;; Spacemacs
(use-package spacemacs-theme
  :defer t
  :init
  (load-theme 'spacemacs-light t))

;; Change font
(set-face-attribute 'default nil :family "Hack" :height 90)

(provide 'base-themes)
;;; themes.el ends here
