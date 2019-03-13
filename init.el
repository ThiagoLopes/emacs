;;; package --- Main init file
;;; Commentary:
;;; This is my init file

;; This file bootstraps the configuration, which is divided into
;; a number of other files.

;;; Code:
;; Produce backtraces when errors occur
(setq debug-on-error nil)

;; Check emacs version
(let ((minver "24.4"))
  (when (version< emacs-version minver)
    (error "Your Emacs is too old -- this config requires v%s or higher" minver)))
(when (version< emacs-version "25.1")
  (message "Your Emacs is old, and some functionality in this config will be disabled. Please upgrade if possible."))

;; add list folders
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(require 'base-benchmarking) ;; Measure startup time

(defconst *spell-check-support-enabled* nil) ;; Enable with t if you prefer :D

;;----------------------------------------------------------------------------
;; Adjust garbage collection thresholds during startup, and thereafter
;;----------------------------------------------------------------------------
(let ((normal-gc-cons-threshold (* 20 1024 1024))
      (init-gc-cons-threshold (* 128 1024 1024)))
  (setq gc-cons-threshold init-gc-cons-threshold)
  (add-hook 'emacs-startup-hook
            (lambda () (setq gc-cons-threshold normal-gc-cons-threshold))))

;;----------------------------------------------------------------------------
;; Bootstrap config init
;;----------------------------------------------------------------------------
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(require 'new-base-utils)
(require 'base-elpa)
(require 'base-exec-path) ;; configure $PATH

;;----------------------------------------------------------------------------
;; Allow users to provide an optional "init-preload-local.el"
;;----------------------------------------------------------------------------
(require 'init-preload-local nil t)

;;----------------------------------------------------------------------------
;; Load configs for specific features and modes
;;----------------------------------------------------------------------------

(require-package 'diminish)
(require-package 'scratch)
(require-package 'command-log-mode)

(require 'base-frame-hooks)
(require 'base-themes)

(require 'base)

(require 'base-dired)
(require 'base-isearch)
(require 'base-grep)
(require 'base-uniquify)
(require 'base-ibuffer)
(require 'base-flycheck)

(require 'base-recentf)
(require 'base-smex)
(require 'base-ivy)
;; (require 'base-helm)
(require 'base-hippie-expand)
(require 'base-company)
(require 'base-windows)
(require 'base-sessions)
(require 'base-mmm)

(require 'base-editing-utils)
(require 'base-whitespace)
(require 'base-vc)
(require 'base-git)
(require 'base-projectile)
(require 'base-compile)

(require 'base-markdown)
(require 'base-csv)
(require 'base-erlang)
(require 'base-javascript)
(require 'base-org)
(require 'base-html)
(require 'base-css)
(require 'base-http)
(require 'base-python)
(require 'base-haskell)
(require 'base-elm)
(require 'base-sql)
(require 'base-rust)
(require 'base-yaml)
(require 'base-go)

(require 'base-paredit)
(require 'base-lisp)
(require 'base-misc)

(require-package 'htmlize)


(when (maybe-require-package 'uptimes)
  (setq-default uptimes-keep-count 200)
  (add-hook 'after-init-hook (lambda () (require 'uptimes))))


;;----------------------------------------------------------------------------
;; Allow access from emacsclient
;;----------------------------------------------------------------------------
(add-hook 'after-init-hook
          (lambda ()
            (require 'server)
            (unless (server-running-p)
              (server-start))))

;;----------------------------------------------------------------------------
;; Variables configured via the interactive 'customize' interface
;;----------------------------------------------------------------------------
(when (file-exists-p custom-file)
  (load custom-file))


;;----------------------------------------------------------------------------
;; Locales (setting them earlier in this file doesn't work in X)
;;----------------------------------------------------------------------------
(require 'base-locales)


;;----------------------------------------------------------------------------
;; Allow users to provide an optional "base-local" containing personal settings
;;----------------------------------------------------------------------------
(require 'base-local nil t)

;; MY CONFIGS

(require 'base-packages)
(require 'base-utils)
(require 'base-keybindings)

;; (require 'langs)

;;; init.el ends here
