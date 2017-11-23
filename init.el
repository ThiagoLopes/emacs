;;; package --- Main init file
;;; Commentary:
;;; This is my init file

;;; Code:

(setq user-full-name "Thiago Lopes")
(setq user-mail-address "thiagolopes@protonmail.com")

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(add-to-list 'load-path (concat user-emacs-directory "elisp"))

(require 'base)
;; (require 'base-theme)
;; (require 'base-extensions)
;; (require 'base-functions)
;; (require 'base-global-keys)

;; (require 'lang-python)

;; Iniciando tema monokai
(load-theme 'monokai t)

;; Iniciando powerline
(require 'powerline)
(powerline-default-theme)
(require 'autopair)
(autopair-global-mode) ;; enable autopair in all buffers
(require 'highlight-symbol)
(global-set-key [(control f3)] 'highlight-symbol)
(global-set-key [f3] 'highlight-symbol-next)
(global-set-key [(shift f3)] 'highlight-symbol-prev)
(global-set-key [(meta f3)] 'highlight-symbol-query-replace)
(add-hook 'after-init-hook #'global-flycheck-mode)
(require 'ido)
(require 'ido-vertical-mode)
(ido-mode t)
(ido-vertical-mode 1)
(setq ido-vertical-define-keys 'C-n-and-C-p-only)
;;; init.el ends here
