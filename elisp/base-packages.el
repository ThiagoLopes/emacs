;; Emacs configuration file
;; Forked from: Auralcat
;; Author: Thiago Lopes
;; Started in May 2017.

;;; Code:
;;-----PACKAGES-----------------------------------------------------------------
;; Package sources
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
             ("org" . "http://orgmode.org/elpa/")
             ("marmalade" . "http://marmalade-repo.org/packages/")
             ("melpa-stable" . "http://melpa-stable.milkbox.net/packages/")))
(package-initialize)

;; If package isn't installed, fetch it
(defun require-package (package)
  (setq-default highlight-tabs t)
  "Install given PACKAGE."
  (unless (package-installed-p package)
    (unless (assoc package package-archive-contents)
      (package-refresh-contents))
    (package-install package)))

;; Autopair - Automatically pair braces and quotes like in TextMate
(require-package 'autopair)
(autopair-global-mode) ;; enable autopair in all buffers

;; Iniciando tema monokai
(require-package 'monokai-alt-theme)
(require-package 'monokai-theme)
(require-package 'color-theme-sanityinc-tomorrow)
(load-theme 'sanityinc-tomorrow-night t)

;; Iniciando powerline
(require-package 'powerline)
(powerline-default-theme)

;; Highlight symbol
(require-package 'highlight-symbol)
(global-set-key [(control f3)] 'highlight-symbol)
(global-set-key [f3] 'highlight-symbol-next)
(global-set-key [(shift f3)] 'highlight-symbol-prev)
(global-set-key [(meta f3)] 'highlight-symbol-query-replace)

;; Flyckech Mode
(require-package 'flycheck)
(add-hook 'after-init-hook #'global-flycheck-mode)

;; Ido
(require-package 'ido-hacks)
(require-package 'ido-vertical-mode)
(ido-mode t)
(ido-vertical-mode 1)
(setq ido-vertical-define-keys 'C-n-and-C-p-)

;; Company mode - autocomplete
(require-package 'company)
(add-hook 'after-init-hook 'global-company-mode)


;; Magit - Work with Git inside Emacs
(require-package 'magit)

;; Rainbow mode - CSS
(require-package 'rainbow-mode)

;; Switch window
(require-package 'switch-window)
(global-set-key (kbd "C-x o") 'switch-window)
(setq switch-window-shortcut-style 'qwerty)
(setq switch-window-qwerty-shortcuts
      '("a" "s" "d" "f" "j" "k" "l" ";" "w" "e" "i" "o"))

;; Python Elpy
(require-package 'elpy)
(elpy-enable)

;; Dashboard
(require-package 'dashboard)
(dashboard-setup-startup-hook)

;; Emmet
(require-package 'emmet-mode)
(add-hook 'sgml-mode-hook 'emmet-mode) ;; Auto-start on any markup modes

;; Elm
(require-package 'elm-mode)

;; NumberLines
;;(require-package 'nlinum)
;;(global-nlinum-mode)

;; FUN NYAN MODE
(require-package 'nyan-mode)
(nyan-mode)

;; Git gutter mode
(require-package 'git-gutter)
(global-git-gutter-mode +1)

;; Virtualenvwrapper
(require-package 'virtualenvwrapper)
(venv-initialize-eshell) ;; if you want eshell support

(provide 'base-packages)
;;; base-packages ends here
