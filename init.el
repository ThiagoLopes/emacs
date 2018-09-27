;;; package --- Main init file
;;; Commentary:
;;; This is my init file

;;; Code:

;;-----PACKAGES-----------------------------------------------------------------
;; Package sources
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(setq use-package-always-ensure t)
(setq gc-cons-threshold (* 50 1000 1000))
(setq gc-cons-threshold (* 2 1000 1000))
;;------------------------------------------------------------------------------
;; Add base(s)
(add-to-list 'load-path (concat user-emacs-directory "elisp"))
(require 'base)
(require 'base-packages)
(require 'base-utils)
(require 'base-keybindings)
(require 'base-themes)

(require 'langs)
;;; init.el ends here
