;;; init.el --- Load the full configuration -*- lexical-binding: t -*-
;;; Commentary:

;; This file bootstraps the configuration, which is divided into
;; a number of other files.
;; Forkend from purcell https://github.com/purcell/emacs.d/
;; Highly modified

;;; Code:

;; Produce backtraces when errors occur
(setq debug-on-error nil)

;; Check emacs version
(let ((minver "24.4"))
  (when (version< emacs-version minver)
    (error "Your Emacs is too old -- this config requires v%s or higher" minver)))
(when (version< emacs-version "25.1")
  (message "Your Emacs is old, and some functionality in this config will be disabled. Please upgrade if possible."))
;; https://www.reddit.com/r/emacs/comments/cdei4p/failed_to_download_gnu_archive_bad_request/
(when (version= emacs-version "26.1")
  (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3"))

;; add list folders
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(require 'base-benchmarking) ;; Measure startup time

(defconst *spell-check-support-enabled* nil) ;; Enable with t if you prefer :D

;;----------------------------------------------------------------------------
;; Adjust garbage collection thresholds during startup, and thereafter
;;----------------------------------------------------------------------------
(let ((normal-gc-cons-threshold (* 16 1024 1026))
      (init-gc-cons-threshold (* 512 1024 1024)))
  (setq gc-cons-threshold init-gc-cons-threshold)
  (add-hook 'emacs-startup-hook
            (lambda () (setq gc-cons-threshold normal-gc-cons-threshold))))

;;----------------------------------------------------------------------------
;; Bootstrap config init
;;----------------------------------------------------------------------------
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(require 'base-utils) ;; customized
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
(maybe-require-package 'scratch)
(require-package 'command-log-mode)

(require 'base-frame-hooks)
(require 'base-themes) ;; customized
(require 'base-gui-frames)

(require 'base) ;; customized

(require 'base-dired)
(require 'base-isearch)
(require 'base-grep)
(require 'base-uniquify)
(require 'base-ibuffer)
(require 'base-flycheck)

(require 'base-recentf)
(require 'base-smex)
(require 'base-ivy)
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
(require 'base-scheme)
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

(provide 'init)
;;; init.el ends here
