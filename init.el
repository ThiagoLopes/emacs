;;; init.el -*- lexical-binding: t; -*-
;;; Commentary:

;; This file bootstraps the configuration, which is divided into a
;; number of other files. Forkend from purcell
;; https://github.com/purcell/emacs.d/
;; and from
;; https://github.com/hlissner/doom-emacs
;; Highly modified by me and other folks

;; Author: Thiago Lopes <thiagolopes@pm.me>

;;Temporary project name: Quake

;;      .,o'       `o,.
;;    ,o8'           `8o.
;;   o8'               `8o
;;  o8:                 ;8o
;; .88                   88.
;; :88.                 ,88:
;; `888                 888'
;;  888o   `888 888'   o888
;;  `888o,. `88 88' .,o888'
;;   `8888888888888888888'
;;     `888888888888888'
;;        `::88;88;:'
;;           88 88
;;           88 88
;;           `8 8'
;;            ` '

;;; Code:

;;----------------------------------------------------------------------------
;; Adjust garbage collection thresholds during startup, and thereafter
;; ----------------------------------------------------------------------------
;; A big contributor to startup times is garbage collection. We up the
;; gc threshold to temporarily prevent it from running, then reset it
;; later added in hook `emacs-startup-hook`. Not resetting it will
;; cause stuttering/freezes.
(let ((normal-gc-cons-threshold (* 16 1024 1026)))
  (setq gc-cons-threshold most-positive-fixnum)
  (add-hook 'emacs-startup-hook
            (lambda () (setq gc-cons-threshold normal-gc-cons-threshold))))

;; In noninteractive sessions, prioritize non-byte-compiled source files to
;; prevent the use of stale byte-code. Otherwise, it saves us a little IO time
;; to skip the mtime checks on every *.elc file we load.
(setq load-prefer-newer noninteractive)

(let (file-name-handler-alist)
  ;; Ensure Quake is running out of this file's directory
  (setq user-emacs-directory (file-name-directory load-file-name)))

;;----------------------------------------------------------------------------
;; Produce backtraces when errors occur
;;----------------------------------------------------------------------------
(setq debug-on-error nil)

;;----------------------------------------------------------------------------
;; Check emacs version
;;----------------------------------------------------------------------------
(let ((minver "24.4"))
  (when (version< emacs-version minver)
    (error "Your Emacs is too old -- this config requires v%s or higher" minver)))
(when (version< emacs-version "25.1")
  (message "Your Emacs is old, and some functionality in this config will be disabled. Please upgrade if possible."))
;; https://www.reddit.com/r/emacs/comments/cdei4p/failed_to_download_gnu_archive_bad_request/
(when (version= emacs-version "26.1")
  (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3"))

;;----------------------------------------------------------------------------
;; add list folders
;;----------------------------------------------------------------------------
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(require 'base-benchmarking) ;; Measure startup time

(defconst *spell-check-support-enabled* t) ;; Enable with t if you prefer :D

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

(require 'base-editing-utils)

(require 'base-frame-hooks)
(require 'base-themes) ;; customized
(require 'base-gui-frames)

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
(require 'base-whitespace)
(require 'base-dumb-jump)

;; Version
(require 'base-vc)
(require 'base-git)
(require 'base-github)
(require 'base-projectile)

;; Compile
(require 'base-compile)

;; Langs
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
(require 'base-toml)
(require 'base-yaml)
(require 'base-go)

;; Lisp/Scheme
(require 'base-paredit)
(require 'base-lisp)
(require 'base-slime)
(require 'base-clojure)
(require 'base-clojure-cider)
(require 'base-common-lisp)
(require 'base-scheme)

(when *spell-check-support-enabled*
  (require 'base-spelling))

(require 'base-misc)

(require 'base-folding)

;; Simple packages load
(require-package 'htmlize)
(require-package 'gnuplot)
(require-package 'lua-mode)
(maybe-require-package 'dotenv-mode)

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
