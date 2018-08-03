;;; package --- Main init file
;;; Commentary:
;;; This is my init file

;;; Code:

;;----------------------------------------------------------------------------
;; Adjust garbage collection thresholds during startup, and thereafter
;;----------------------------------------------------------------------------
(let ((normal-gc-cons-threshold (* 20 1024 1024))
      (init-gc-cons-threshold (* 128 1024 1024)))
  (setq gc-cons-threshold init-gc-cons-threshold)
  (add-hook 'after-init-hook
            (lambda () (setq gc-cons-threshold normal-gc-cons-threshold))))

;;-----PACKAGES-----------------------------------------------------------------
;; Package sources
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))

(package-initialize)

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
;;------------------------------------------------------------------------------

;;Record startup timestamp
(defvar super-emacs/invokation-time
  (current-time))

(add-to-list 'load-path (concat user-emacs-directory "elisp"))
;; (add-to-list 'load-path ".orgmode/")

;; Spell
(defconst *spell-check-support-enabled* nil) ;; Enable with t if you prefer

(require 'base)
(require 'base-keybindings)
(require 'base-packages)
(require 'base-themes)
(require 'base-utils)
(require 'base-exec-path)
(require 'lang-javascript)
(require 'lang-python)
(require 'lang-html)
(require 'lang-go)

;;Print welcome message
(princ (cl-concatenate 'string
                       ";; 01000101 01101101 01100001 01100011 01110011\n\n\n"
                       ";; Startup completed in "
                       (number-to-string (cadr (time-subtract (current-time)
                                                              super-emacs/invokation-time)))
                       " seconds\n\n"
                       ";; Welcome to ｅｍａｃｓ  安ェ殴ど依挨虞!\n\n"
                       ";; Today's date: "
                       (format-time-string "%B %d %Y")
                       (concat "\n\n;; Happy hacking, " user-login-name " - Emacs ♥ you!\n\n"))
       (get-buffer-create (current-buffer)))

;;; init.el ends here
