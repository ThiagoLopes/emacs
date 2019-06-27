;;; init-themes.el --- Defaults for themes -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require-package 'color-theme-sanityinc-solarized)
(require-package 'color-theme-sanityinc-tomorrow)
(require-package 'zenburn-theme)
(require-package 'white-theme)
(require-package 'alect-themes)
(require-package 'doom-themes)
(require-package 'monokai-theme)

;; If you don't customize it, this is the theme you get.
(setq-default custom-enabled-themes '(monokai))

;; Ensure that themes will be applied even if they have not been customized
(defun reapply-themes ()
  "Forcibly load the themes listed in `custom-enabled-themes'."
  (dolist (theme custom-enabled-themes)
    (unless (custom-theme-p theme)
      (load-theme theme t)))
  (custom-set-variables `(custom-enabled-themes (quote ,custom-enabled-themes))))

(add-hook 'after-init-hook 'reapply-themes)

;;------------------------------------------------------------------------------
;; Toggle between light and dark
;;------------------------------------------------------------------------------
(defun light ()
  "Activate a light color theme."
  (interactive)
  (setq custom-enabled-themes '(sanityinc-tomorrow-day))
  (reapply-themes))

(defun dark ()
  "Activate a dark color theme."
  (interactive)
  (setq custom-enabled-themes '(sanityinc-tomorrow-bright))
  (reapply-themes))

(when (maybe-require-package 'dimmer)
  (setq-default dimmer-fraction 0.4)
  (add-hook 'after-init-hook 'dimmer-mode)
  ;; TODO: file upstream as a PR
  (after-load 'dimmer
    (advice-add 'frame-set-background-mode :after (lambda (&rest args) (dimmer-process-all)))))

;;------------------------------------------------------------------------------
;; Mode line
;;------------------------------------------------------------------------------
(when (maybe-require-package 'telephone-line)
  (telephone-line-mode 1))

;;------------------------------------------------------------------------------
;; Themes
;;------------------------------------------------------------------------------
(when (maybe-require-package 'night-owl-theme))
(when (maybe-require-package 'dracula-theme))
(when (maybe-require-package 'apropospriate-theme))

(provide 'base-themes)
;;; init-themes.el ends here
