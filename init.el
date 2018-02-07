;;; package --- Main init file
;;; Commentary:
;;; This is my init file

;;; Code:

;;-----PACKAGES-----------------------------------------------------------------
;; Package sources
(package-initialize)
(unless (assoc-default "melpa" package-archives)
  (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t))
(unless (assoc-default "melpa" package-archives)
  (add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t))
(unless (assoc-default "melpa" package-archives)
  (add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/") t))
;;------------------------------------------------------------------------------

;;Record startup timestamp
(defvar super-emacs/invokation-time
(current-time))

(setq user-full-name "Thiago Lopes"
      user-mail-address "thiagolopes@protonmail.com")

(add-to-list 'load-path (concat user-emacs-directory "elisp"))
;; (add-to-list 'load-path ".orgmode/")

(setq gc-cons-threshold 64000000)
(add-hook 'after-init-hook #'(lambda ()
                               ;; restore after startup
                               (setq gc-cons-threshold 800000)))

(require 'base)
(require 'base-keybindings)
(require 'base-packages)
(require 'base-themes)
;; (require 'lang-javascript)
(require 'lang-python)
(require 'lang-web)

;;Print welcome message
(princ (cl-concatenate 'string
                       "01000101 01101101 01100001 01100011 01110011\n\n\n"
                       "Startup completed in "
                       (number-to-string (cadr (time-subtract (current-time)
                                                              super-emacs/invokation-time)))
                       " seconds\n\n"
                       "Welcome to ｅｍａｃｓ  安ェ殴ど依挨虞!\n\n"
                       "Today's date: "
                       (format-time-string "%B %d %Y"))
(get-buffer-create (current-buffer)))

;;; init.el ends here
