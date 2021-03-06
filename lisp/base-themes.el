;;; base-themes.el --- Defaults for themes -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require-package 'color-theme-sanityinc-solarized)
(require-package 'color-theme-sanityinc-tomorrow)
(require-package 'zenburn-theme)
(require-package 'white-theme)
(require-package 'alect-themes)
(require-package 'doom-themes)
(require-package 'monokai-theme)
(require-package 'color-theme-modern)

;; If you don't customize it, this is the theme you get.
(defvar dark-theme '(doom-solarized-dark))
(defvar light-theme '(doom-solarized-light))
(setq-default custom-enabled-themes dark-theme)

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
  (setq custom-enabled-themes light-theme)
  (reapply-themes))

(defun dark ()
  "Activate a dark color theme."
  (interactive)
  (setq custom-enabled-themes dark-theme)
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
(when (maybe-require-package 'doom-modeline)
  (doom-modeline-mode 1))

;;------------------------------------------------------------------------------
;; Themes
;;------------------------------------------------------------------------------
(when (maybe-require-package 'highlight-numbers)
  (highlight-numbers-mode 1))

;;------------------------------------------------------------------------------
;; Set font
;; Good fonts to code - Hack, Iosevka, Terminus, PragmataPro
;;------------------------------------------------------------------------------
(condition-case nil
    (set-face-attribute 'default nil :font "Fantasque Sans Mono 11")
  (error nil))

;;------------------------------------------------------------------------------
;; Theme magic
;;------------------------------------------------------------------------------
(when (maybe-require-package 'theme-magic))


(provide 'base-themes)
;;; base-themes.el ends here
