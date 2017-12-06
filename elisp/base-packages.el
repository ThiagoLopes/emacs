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
;;------------------------------------------------------------------------------

;; Use package
(require-package 'use-package)
(eval-when-compile
  ;; Following line is not needed if use-package.el is in ~/.emacs.d
  (require 'use-package))
(setq use-package-always-ensure t)

;; Org
(use-package org)

;; Autopair - Automatically pair braces and quotes like in TextMate
(use-package autopair
  :config
  (autopair-global-mode)
  )

;; Highlight symbol
(use-package highlight-symbol
  :config
  (global-set-key [(control f3)] 'highlight-symbol)
  (global-set-key [f3] 'highlight-symbol-next)
  (global-set-key [(shift f3)] 'highlight-symbol-prev)
  (global-set-key [(meta f3)] 'highlight-symbol-query-replace)
  )

;; Flyckech Mode
(use-package flycheck
  :ensure t
  :init
  (flycheck-mode)
  (flycheck-buffer)
  :config
  (setq flycheck-highlighting-mode 'lines)
  )

(use-package flycheck-pos-tip
  :after (flycheck)
  :init
  (flycheck-pos-tip-mode)
  )

;; Company mode - autocomplete
(use-package company
  :ensure t
  :init
  (setq company-require-match 'never)
  :config
  (add-hook 'after-init-hook 'global-company-mode)
  :bind(:map company-active-map
             ("TAB" . company-complete-common-or-cycle)
             ("<tab>" . company-complete-common-or-cycle ))
  )

;; Magit - Work with Git inside Emacs
(use-package magit)

;; Rainbow mode - CSS
(use-package rainbow-mode)

;; Switch window
(use-package switch-window
  :no-require t
  :config
  (bind-key "C-x o" 'switch-window)
  (setq switch-window-shortcut-style 'qwerty)
  (setq switch-window-qwerty-shortcuts
        '("a" "s" "d" "f" "j" "k" "l" ";" "w" "e" "i" "o"))
  )

;; Python Elpy
(use-package elpy
  :ensure t
  :init
  (elpy-enable)
  :config
  (elpy-use-ipython)
  (setq python-shell-interpreter "ipython"
        python-shell-interpreter-args "--simple-prompt --pprint")

  (when (require 'flycheck nil t)
    (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
    (add-hook 'elpy-mode-hook 'flycheck-mode))

  (add-hook 'python-mode-hook
            (lambda ()
              (add-to-list (make-local-variable 'company-backends)
                           '(elpy-company-backend))))
  (add-hook 'inferior-python-mode-hook
            (lambda ()
              (push
               'comint-watch-for-password-prompt comint-output-filter-functions)))
  )

;; PEP8
(use-package py-autopep8
  :init
  (add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save)
  )

;; Pyenv
;;(require-package 'pyenv-mode)
;; (pyenv-mode)

;; Dashboard
(use-package dashboard
  :init
  (dashboard-setup-startup-hook))

;; Emmet
(use-package emmet-mode
  :config
  (add-hook 'sgml-mode-hook 'emmet-mode)
  )

;; Elm
(use-package elm-mode)

;; NumberLines
;;(require-package 'nlinum)
;;(global-nlinum-mode)

;; FUN NYAN MODE
(use-package nyan-mode
  :config
  (nyan-mode))

;; Changer inner - clone ci vim
(use-package change-inner
  :init
  (bind-key "M-i" 'change-inner)
  )

;; Git gutter mode
(use-package git-gutter
  :init
  (global-git-gutter-mode +1)
  :custom
  (hide-gutter t "disable if not have changes")
  (live-update t "update live")
  )
;; (require-package 'git-gutter)
;; (global-git-gutter-mode +1)
;; (custom-set-variables
;;  '(git-gutter:hide-gutter t))
;; (custom-set-variables
;;  '(git-gutter:update-interval 0))

;; Zoom
;; (use-package zoom
;;   :custom
;;   (zoom-size '(0.618 . 0.618) "golden ration")
;;   (zoom-mode t "enable")
;;   )

;; Golder ration
(use-package golden-ratio
  :config
  (golden-ratio-mode 1))

;; Beacon
(use-package beacon
  :init
  (beacon-mode 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; HELM PACKAGES SETTINGS ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package helm
  :ensure t
  :bind (("M-x" . helm-M-x)
         ("C-x C-f" . helm-find-files)
         ("C-x f" . helm-recentf)
         ("M-y" . helm-show-kill-ring)
         ("C-x b" . helm-buffers-list))
  ;; :bind (:map helm-map
  ;;             ("M-i" . helm-previous-line)
  ;;             ("M-k" . helm-next-line)
  ;;             ("M-I" . helm-previous-page)
  ;;             ("M-K" . helm-next-page)
  ;;             ("M-h" . helm-beginning-of-buffer)
  ;;             ("M-H" . helm-end-of-buffer))
  :config (progn
            (setq helm-buffers-fuzzy-matching t)
            (helm-mode 1)))

(use-package helm-descbinds
  :after (helm)
  :ensure t
  :bind ("C-h b" . helm-descbinds))

(use-package helm-swoop
  :after (helm)
  :ensure t
  :bind (("M-m" . helm-swoop)
         ("M-M" . helm-swoop-back-to-last-point))
  :init
  (bind-key "M-m" 'helm-swoop-from-isearch isearch-mode-map))

(use-package helm-ag
  :after (helm)
  :ensure t
  :bind ("M-p" . helm-do-ag)
  :commands (helm-ag helm-projectile-ag)
  :init (setq helm-ag-insert-at-point 'symbol
              helm-ag-command-option "--path-to-ignore ~/.agignore"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; END CONFIGURATION FOR HERE ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Markdown mode
(require-package 'markdown-mode)
(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

;; Ibuffer
(use-package ibuffer
  :init
  (global-set-key (kbd "C-x C-b") 'ibuffer)
  :config
  (setq ibuffer-saved-filter-groups
        '(("home"
           ("emacs-config" (or (filename . ".emacs.d")
                               (filename . "emacs-config")))
           ("martinowen.net" (filename . "martinowen.net"))
           ("Org" (or (mode . org-mode)
                      (filename . "OrgMode")))
           ("code" (filename . "code"))
           ("Web Dev" (or (mode . html-mode)
                          (mode . css-mode)))
           ("Subversion" (name . "\*svn"))
           ("Magit" (name . "\*magit"))
           ("Pytho" (name . "\*py"))
           ("ERC" (mode . erc-mode))
           ("Help" (or (name . "\*Help\*")
                       (name . "\*Apropos\*")
                       (name . "\*info\*"))))))
  (add-hook 'ibuffer-mode-hook
            '(lambda ()
               (ibuffer-switch-to-saved-filter-groups "home")))
  (setq ibuffer-show-empty-filter-groups nil)
  (add-hook 'ibuffer-mode-hook
            '(lambda ()
               (ibuffer-auto-mode 1)
               (ibuffer-switch-to-saved-filter-groups "home")))

  )

;; Buffer move
(use-package buffer-move
  :init
  (bind-key "<C-S-up>" 'buf-move-up)
  (bind-key "<C-S-down>" 'buf-move-down)
  (bind-key "<C-S-left>" 'buf-move-left)
  (bind-key "<C-S-right>" 'buf-move-right))

;; Ace jump
(use-package ace-jump-mode
  :config
  (bind-key "C-c SPC" 'ace-jump-mode)
  )

;;

(provide 'base-packages)
;;; base-packages ends here
