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
             ("melpa" . "http://melpa.org/packages/")))
(package-initialize)

;; If package isn't installed, fetch it
(defun require-package (package)
  (setq-default highlight-tabs t)
  "Install given PACKAGE."
  (unless (package-installed-p package)
    (unless (assoc package package-archive-contents)
      (package-refresh-contents))
    (package-install package)))

;;------------------------------------------------------------------------------

;; Use package
(require-package 'use-package)

;; Autopair - Automatically pair braces and quotes like in TextMate
(require-package 'autopair)
(autopair-global-mode) ;; enable autopair in all buffers

;; Highlight symbol
(require-package 'highlight-symbol)
(global-set-key [(control f3)] 'highlight-symbol)
(global-set-key [f3] 'highlight-symbol-next)
(global-set-key [(shift f3)] 'highlight-symbol-prev)
(global-set-key [(meta f3)] 'highlight-symbol-query-replace)

;; Flyckech Mode
(require-package 'flycheck)
;; highlight per-line instead
(setq flycheck-highlighting-mode 'lines)
;; run flycheck
(flycheck-mode)
(flycheck-buffer)

(require-package 'flycheck-pos-tip)
(with-eval-after-load 'flycheck
  (flycheck-pos-tip-mode))

;; Ido
(require-package 'ido-hacks)
(require-package 'ido-vertical-mode)
(require-package 'ido-completing-read+)
(ido-mode t)
(ido-ubiquitous-mode 1)
(ido-everywhere 1)
;; disable ido faces to see flx highlights.
(setq ido-enable-flex-matching t)
(setq ido-use-faces nil)
(setq ido-vertical-define-keys 'C-n-and-C-p-)
(ido-vertical-mode 1)


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
(setq elpy-rpc-python-command "python3")
(elpy-use-ipython)

;; Pyenv
(require-package 'pyenv-mode)
(pyenv-mode)

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
(custom-set-variables
 '(git-gutter:hide-gutter t))
(custom-set-variables
 '(git-gutter:update-interval 0))

;; Virtualenvwrapper
(require-package 'virtualenvwrapper)
(setq venv-location "~/.virtualenv/")

;; Zoom
(require-package 'zoom)
(custom-set-variables
 '(zoom-size '(0.618 . 0.618)))
(custom-set-variables
 '(zoom-mode t))

;; Beacon
(require-package 'beacon)
(beacon-mode 1)

;; ;; Helm
;; (require-package 'helm)
;; ;; Helm plugins
;; (require-package 'helm-ls-git)

;; (helm-mode 1)
;; (global-set-key (kbd "M-x")                          'undefined)
;; (global-set-key (kbd "M-x")                          'helm-M-x)
;; (global-set-key (kbd "C-x C-b")                      'helm-buffers-list)
;; (global-set-key (kbd "M-y")                          'helm-show-kill-ring)
;; (global-set-key (kbd "C-x C-f")                      'helm-find-files)
;; (global-set-key (kbd "C-c <SPC>")                    'helm-all-mark-rings)
;; (global-set-key (kbd "C-x r b") 'helm-filtered-bookmarks)

;; Markdown mode
(require-package 'markdown-mode)
(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

(provide 'base-packages)
;;; base-packages ends here
