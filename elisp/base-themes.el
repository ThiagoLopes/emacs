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
  (load-theme 'sanityinc-tomorrow-night t))

;; Spacemacs
(use-package spacemacs-theme
  :defer t
  :init
  :disabled
  (load-theme 'spacemacs-dark t))

(use-package dracula-theme
  :ensure t
  :config
  (load-theme 'dracula t)
  :disabled)

;; Change font
;; (set-face-attribute 'default nil :family "Hack" :height 90)

;; (use-package spacemacs-fonts-support
;;   :config
;;   (spacemacs/set-default-font
;;    '("Hack"
;;      :size 10
;;      :weight normal
;;      :width normal)))

(use-package zenburn-theme
  :disabled)

(use-package powerline
  :config
  (powerline-center-theme)
  (setq powerline-default-separator
        'slant))

(set-frame-font "Menlo:pixelsize=14")

(provide 'base-themes)
;;; themes.el ends here
