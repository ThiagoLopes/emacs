;;; package --- Main init file
;;; Commentary:
;;; File with themes

;;; Code:

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

(use-package zenburn-theme
  :disabled)
(use-package sexy-monochrome-theme)

(provide 'base-themes)
;;; themes.el ends here
