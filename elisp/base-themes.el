;;; package --- Main init file
;;; Commentary:
;;; This is my init file

;;; Code:

;; Monoaki Alt
(use-package monokai-alt-theme
  :defer t
  :disabled)

;; Aftermoon
(use-package afternoon-theme
  :defer t
  :disabled)

;; Monokai
(use-package monokai-theme
  :defer t
  :disabled)

;; Sanityinc
(use-package color-theme-sanityinc-tomorrow
  :config
  :disabled)

;; Spacemacs
(use-package spacemacs-theme
  :defer t
  :init
  :disabled)

(use-package dracula-theme
  :ensure t
  :config
  :disabled)

(use-package zenburn-theme
  :disabled)

(use-package atom-one-dark-theme
  :ensure t
  :disabled)

(use-package zenburn-theme
  :ensure t)

(use-package powerline
  :config
  (powerline-center-theme)
  (setq powerline-default-separator
        'slant))

(set-frame-font "Hack:pixelsize=12")

(provide 'base-themes)
;;; themes.el ends here
