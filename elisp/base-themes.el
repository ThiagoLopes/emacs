;;; package --- Main init file
;;; Commentary:
;;; This is my init file

;;; Code:

;;------------------------------------------------------------------------------
;; Toggle between light and dark
;;------------------------------------------------------------------------------
;; Ensure that themes will be applied even if they have not been customized
(defun reapply-themes ()
  "Forcibly load the themes listed in `custom-enabled-themes'."
  (dolist (theme custom-enabled-themes)
    (unless (custom-theme-p theme)
      (load-theme theme)))
  (custom-set-variables `(custom-enabled-themes (quote ,custom-enabled-themes))))

(defun light ()
  "Activate a light color theme."
  (interactive)
  (setq custom-enabled-themes '(sanityinc-tomorrow-day))
  (reapply-themes))

(defun night ()
  "Activate a dark color theme."
  (interactive)
  (setq custom-enabled-themes '(sanityinc-tomorrow-bright))
  (reapply-themes))


(use-package dimmer
  :config
  (setq-default dimmer-fraction 0.3)
  (add-hook 'after-init-hook 'dimmer-mode))

;; Aftermoon
(use-package afternoon-theme
  :defer t
  :disabled)

;; Spacemacs
(use-package spacemacs-theme
  :defer t
  :init
  :disabled)

(use-package zenburn-theme)

(use-package atom-one-dark-theme
  :ensure t
  :disabled)

(use-package color-theme-sanityinc-solarized
  :defer t)
(use-package color-theme-sanityinc-tomorrow
  :defer 5)

(set-frame-font "Hack:pixelsize=12")

(provide 'base-themes)
;;; themes.el ends here
