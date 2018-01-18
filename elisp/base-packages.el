;; Emacs configuration file
;; Forked from: Auralcat
;; Author: Thiago Lopes
;; Started in May 2017.

;;; Code:

;;-----PACKAGES-----------------------------------------------------------------
;; Package sources
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("melpa" . "http://melpa.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")))

(package-initialize)
;;------------------------------------------------------------------------------

;; Use package
(require-package 'use-package)
(eval-when-compile
  (require 'use-package))
(setq use-package-always-ensure t)

;; Avy
(use-package avy
  :bind
  ("C-c SPC" . avy-goto-char))

;; Counsel
(use-package counsel
  :bind
  ("M-x" . counsel-M-x)
  ("C-x C-m" . counsel-M-x)
  ("C-x C-f" . counsel-find-file)
  ("C-x c k" . counsel-yank-pop)
  ("M-y" . counsel-yank-pop))

(use-package counsel-projectile
  :bind
  ("C-x v" . counsel-projectile)
  ("C-x c p" . counsel-projectile-ag)
  :config
  (counsel-projectile-on))

(use-package swiper-helm
  :bind
  ("C-s" . swiper)
)

;; Ivy
(use-package ivy
  :bind
  ("C-x C-r" . ivy-resume)
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers nil)
  (define-key read-expression-map (kbd "C-r") 'counsel-expression-history))

;; Number Lines
(use-package nlinum
  :init
  (nlinum-mode))

;; Company
(use-package company
  :init
  (add-hook 'after-init-hook 'global-company-mode)
  :config
  (setq company-tooltip-limit 10)
  (setq company-idle-delay 0.2)
  (setq company-echo-delay 0)
  (setq company-minimum-prefix-length 3)
  (setq company-require-match nil)
  (setq company-selection-wrap-around t)
  (setq company-tooltip-align-annotations t)
  (setq company-tooltip-flip-when-above t)
  (setq company-transformers '(company-sort-by-occurrence)) ; weight by frequency
  :diminish
  company-mode
  )

;; Org
(use-package org)

;; Highlight symbol
(use-package highlight-symbol
  :config
  (global-set-key [(control f3)] 'highlight-symbol)
  (global-set-key [f3] 'highlight-symbol-next)
  (global-set-key [(shift f3)] 'highlight-symbol-prev)
  (global-set-key [(meta f3)] 'highlight-symbol-query-replace)
  )

;; Ediff
(use-package ediff
  :config
  (setq ediff-window-setup-function 'ediff-setup-windows-plain)
  (setq-default ediff-highlight-all-diffs 'nil)
  (setq ediff-diff-options "-w"))

;; Flyckech Mode
(use-package flycheck
  :config
  (setq flycheck-highlighting-mode 'lines)
  )

(use-package flycheck-pos-tip
  :after (flycheck)
  :init
  (flycheck-pos-tip-mode)
  )

;; Magit - Work with Git inside Emacs
(use-package magit)
(use-package magit-popup)
(use-package magit-gitflow
  :config
  (add-hook 'magit-mode-hook 'turn-on-magit-gitflow))


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

;; Dashboard
(use-package dashboard
  :config
  (add-hook 'prog-mode-hook (lambda () (set (make-local-variable 'mouse-1-click-follows-link) nil)))
  :init
  (dashboard-setup-startup-hook))

;; Emmet
(use-package emmet-mode
  :config
  (add-hook 'sgml-mode-hook 'emmet-mode)
  )

;; Elm
(use-package elm-mode)

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
(use-package git-gutter+
  :init
  (global-git-gutter+-mode +1)
  :config
  (setq git-gutter+-hide-gutter t)
  )

(use-package git-gutter-fringe+
  :init
  (setq git-gutter+-toggle-fringe t)
)

;; Zoom
(use-package zoom
  :custom
  (zoom-size '(0.618 . 0.618) "golden ration")
  (zoom-mode t "enable")
  )

;; Beacon
(use-package beacon
  :init
  (beacon-mode 1))

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
  (add-hook 'ibuffer-hook
                  (lambda ()
                    (ibuffer-vc-set-filter-groups-by-vc-root)
                    (unless (eq ibuffer-sorting-mode 'alphabetic)
                      (ibuffer-do-sort-by-alphabetic)))))

;; Buffer move
(use-package buffer-move
  :init
  (bind-key "<C-S-up>" 'buf-move-up)
  (bind-key "<C-S-down>" 'buf-move-down)
  (bind-key "<C-S-left>" 'buf-move-left)
  (bind-key "<C-S-right>" 'buf-move-right))

;; Persistent undo
(use-package undohist)
(undohist-initialize)
;;; 永続化を無視するファイル名の正規表現
(setq undohist-ignored-files
      '("~/.emacs.tmp/")
      )

;; hl todo
(use-package hl-todo
  :init
  (global-hl-todo-mode))

;; Recentf
(use-package recentf
  :config
  (setq recentf-save-file (recentf-expand-file-name "~/.emacs.d/private/cache/recentf"))
  (recentf-mode 1))

;; Smartparens
(use-package smartparens
  :init
  (setq smartparens-global-mode t)
  :config
  (setq electric-pair-mode t)
  (setq electric-quote-mode t)
)

;; Smex
;; (use-package smex)

;; Yasnippet
(use-package yasnippet
  :config
  (yas-global-mode 1))

(use-package yasnippet-snippets)

;; Undo-tree
(use-package undo-tree
  :config
  ;; Remember undo history
  (setq
   undo-tree-auto-save-history nil
   undo-tree-history-directory-alist `(("." . "~/.emacs-backup/undo")))
  (global-undo-tree-mode 1))

(use-package dired-sidebar
  :bind (("C-x C-n" . dired-sidebar-toggle-sidebar))
  :ensure t
  :commands (dired-sidebar-toggle-sidebar)
  :config
  (use-package all-the-icons-dired
    ;; M-x all-the-icons-install-fonts
    :ensure t
    :commands (all-the-icons-dired-mode)))

;; (use-package projectile
;;   :init
;;   (projectile-mode t)

(use-package telephone-line
  :init
  (telephone-line-mode 1))

;; NVM USE
(use-package nvm
  :config
  (nvm-use "9.0.0"))

;; Ido mode :D
(ido-mode t)

(provide 'base-packages)
;;; base-packages ends here
