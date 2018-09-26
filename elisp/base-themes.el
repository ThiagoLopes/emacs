;;; package --- Main init file
;;; Commentary:
;;; This is my init file

;;; Code:

(set-face-attribute 'default nil :font "Hack 10")
(setq-default line-spacing 4)
(setq-default frame-title-format "%b (%f)")

;; Toggle between light and dark
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
  (setq custom-enabled-themes '(tsdh-light))
  (reapply-themes))

(defun night ()
  "Activate a dark color theme."
  (interactive)
  (setq custom-enabled-themes '(tsdh-dark))
  (reapply-themes))

(use-package zenburn-theme)

(provide 'base-themes)
;;; themes.el ends here
