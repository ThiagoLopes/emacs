;; Emacs configuration file
;; Forked from: Auralcat, Sacha Chua's
;; Author: Thiago Lopes
;; Started in May 2017.

;;; Code:
;; Use package
(require-package 'use-package)
(eval-when-compile
  (require 'use-package)
  (use-package auto-compile
    :config (auto-compile-on-load-mode))
  (setq load-prefer-newer t)
  (setq use-package-always-ensure t)
  )

;; Better default
(use-package better-defaults)

;; Dash
(use-package dash)

;; Winner
(use-package winner)

;; Smart Mode line
(use-package smart-mode-line)

;; MiniBuffer
;; (use-package miniedit
;;   :commands minibuffer-edit
;;   :init (miniedit-install))

;; Which-key
(use-package which-key
  :defer t
  :config
  (which-key-mode)
  (which-key-setup-side-window-bottom)
  )

;; Smartcan
(use-package smartscan
  :defer t
  :config (global-smartscan-mode t))

;; Avy
(use-package avy
  :defer 2
  :bind
  ("C-c SPC" . avy-goto-char))

;; Helm
;; (use-package helm
;;   :ensure t
;;   :diminish helm-mode
;;   :init
;;   (setq helm-autoresize-max-height 30
;;         helm-display-header-line nil
;;         helm-always-two-windows t
;;         helm-split-window-inside-p t
;;         helm-move-to-line-cycle-in-source t
;;         helm-ff-search-library-in-sexp t
;;         helm-ff-file-name-history-use-recentf t
;;         helm-comp-read-mode-line ""
;;         helm-read-file-name-mode-line-string ""
;;         helm-mode-line-string "")
;;   ;; enable fuzzy matching
;;   (setq helm-buffers-fuzzy-matching t
;;         helm-completion-in-region-fuzzy-match t
;;         helm-M-x-fuzzy-match t
;;         helm-apropos-fuzzy-match t
;;         helm-imenu-fuzzy-match t
;;         helm-lisp-fuzzy-completion t
;;         helm-locate-fuzzy-match t
;;         helm-mode-fuzzy-match t
;;         helm-recentf-fuzzy-match t
;;         helm-semantic-fuzzy-match t)
;;   :config
;;   (require 'helm-config)
;;   (helm-mode 1)
;;   (helm-autoresize-mode 1))

;; fuzzier matching for helm
;; (use-package helm-flx
;;   :ensure t
;;   :after helm
;;   :config
;; (helm-flx-mode +1))

;; Counsel
;; (use-package counsel
;;   :config
;;   (setq ivy-use-virtual-buffers t)
;;   (setq enable-recursive-minibuffers t)
;;   :bind
;;   ("M-x" . counsel-M-x)
;;   ("C-x C-m" . counsel-M-x)
;;   ("C-x C-f" . counsel-find-file)
;;   ("M-y" . counsel-yank-pop))

;; (use-package counsel-projectile
;;   :defer t
;;   :bind
;;   ("C-x v" . counsel-projectile)
;;   ("C-x c p" . counsel-projectile-ag))

(use-package swiper-helm
  :bind
  ("C-s" . swiper)
  )

;; Org
(use-package org
  :defer 2)

;; Highlight symbol
(use-package highlight-symbol
  :defer 2
  :config
  (global-set-key [(control f3)] 'highlight-symbol)
  (global-set-key [f3] 'highlight-symbol-next)
  (global-set-key [(shift f3)] 'highlight-symbol-prev)
  (global-set-key [(meta f3)] 'highlight-symbol-query-replace)
  )

;; Ediff
;; (use-package ediff
;;   :config
;;   (setq ediff-window-setup-function 'ediff-setup-windows-plain)
;;   (setq-default ediff-highlight-all-diffs 'nil)
;;   (setq ediff-diff-options "-w"))

;; Flyckech Mode
(use-package flycheck
  :defer 2
  :config
  (global-flycheck-mode)
  (add-hook 'after-init-hook #'global-flycheck-mode)
  (setq flycheck-highlighting-mode 'lines)
  )

(use-package flycheck-pos-tip
  :after (flycheck)
  :config
  (flycheck-pos-tip-mode)
  )

;; Magit - Work with Git inside Emacs
(use-package magit
  :defer 2
  )
(use-package magit-popup
  :after (magit))

(use-package magit-gitflow
  :config
  (add-hook 'magit-mode-hook 'turn-on-magit-gitflow)
  :after
  (magit))

;; Switch window
(use-package switch-window
  :defer t
  :config
  (bind-key "C-x o" 'switch-window)
  (setq switch-window-shortcut-style 'qwerty)
  (setq switch-window-qwerty-shortcuts
        '("a" "s" "d" "f" "j" "k" "l" ";" "w" "e" "i" "o"))
  )

;; ;; Dashboard
;; (use-package dashboard
;;   :config
;;   (add-hook 'prog-mode-hook (lambda () (set (make-local-variable 'mouse-1-click-follows-link) nil)))
;;   :init
;;   (dashboard-setup-startup-hook))

;; ;; Elm
;; (use-package elm-mode)

;; FUN NYAN MODE
(use-package nyan-mode
  :defer t
  :config
  (nyan-mode))

;; Changer inner - clone ci vim
(use-package change-inner
  :defer t
  :init
  (bind-key "M-i" 'change-inner)
  )

;; Git gutter mode
(use-package git-gutter
  :ensure t
  :diminish git-gutter-mode
  :init
  (setq git-gutter:window-width 1
        git-gutter:update-interval 2
        git-gutter:modified-sign "│"
        git-gutter:added-sign "│"
        git-gutter:deleted-sign "│")
  :config
  (setq git-gutter+-hide-gutter t)
  (set-face-foreground 'git-gutter:modified "#ffb86c") ; dracula rainbow-5
  (set-face-foreground 'git-gutter:added "#50fa7b") ; dracula rainbow-6
  (set-face-foreground 'git-gutter:deleted "#ff5555") ; dracula rainbow-9
  (add-to-list 'git-gutter:update-hooks 'focus-in-hook)
  (add-hook 'magit-post-refresh-hook 'git-gutter:update-all-windows)
  (global-git-gutter-mode t))

(use-package git-gutter-fringe+
  :defer 4
  :init
  (setq git-gutter+-toggle-fringe t)
  )

;; Zoom
(use-package zoom
  :defer 1
  :custom
  (zoom-size '(0.618 . 0.618) "golden ration")
  (zoom-mode t "enable")
  )

;; Beacon
(use-package beacon
  :init
  (beacon-mode 1))

;; Clean up obsolete buffers
(use-package midnight
  :defer t
  :init
  (midnight-mode))

;; ;; Ibuffer
;; (use-package ibuffer
;;   :init
;;   (global-set-key (kbd "C-x C-b") 'ibuffer)
;;   (add-hook 'ibuffer-hook
;;             (lambda ()
;;               (ibuffer-vc-set-filter-groups-by-vc-root)
;;               (unless (eq ibuffer-sorting-mode 'alphabetic)
;;                 (ibuffer-do-sort-by-alphabetic)))))

;; Persistent undo
(use-package undohist
  :defer t
  :config
  (undohist-initialize)
;;; 永続化を無視するファイル名の正規表現
  (setq undohist-ignored-files
        '("~/.emacs.tmp/")
        ))

;; hl todo
(use-package hl-todo
  :init
  (global-hl-todo-mode))

;; ;; Recentf
;; (use-package recentf
;;   :config
;;   (setq recentf-save-file (recentf-expand-file-name "~/.emacs.d/private/cache/recentf"))
;;   (recentf-mode 1))

;; Smartparens
(use-package smartparens
  :init
  (setq smartparens-global-mode t)
  :config
  (setq electric-pair-mode t)
  (setq electric-quote-mode t)
  )

;; Smex
(use-package smex)

;; ;; Yasnippet
;; (use-package yasnippet
;;   :diminish yas-minor-mode
;;   :init
;;   (yas-global-mode)
;;   :config
;;   (progn
;;     (yas-global-mode)
;;     (add-hook 'hippie-expand-try-functions-list 'yas-hippie-try-expand)
;;     (setq yas-key-syntaxes '("w_" "w_." "^ "))
;;     (setq yas-installed-snippets-dir "~/elisp/yasnippet-snippets")
;;     (setq yas-expand-only-for-last-commands nil)
;;     (yas-global-mode 1)
;;     (bind-key "\t" 'hippie-expand yas-minor-mode-map)
;;     (add-to-list 'yas-prompt-functions 'shk-yas/helm-prompt)))


;; (use-package yasnippet-snippets)

;; Undo-tree
;; (use-package undo-tree
;;   :defer t
;;   :diminish undo-tree-mode
;;   :config
;;   (progn
;;     (global-undo-tree-mode)
;;     (setq undo-tree-visualizer-timestamps t)
;;     (setq undo-tree-visualizer-diff t)))
;; ;; Remember undo history
;; (setq
;;  undo-tree-auto-save-history nil
;;  undo-tree-history-directory-alist `(("." . "~/.emacs-backup/undo")))
;; (global-undo-tree-mode 1)

;; (use-package dired-sidebar
;;   :bind (("C-x C-n" . dired-sidebar-toggle-sidebar))
;;   :ensure t
;;   :commands (dired-sidebar-toggle-sidebar)
;;   :config
;;   (use-package all-the-icons-dired
;;     ;; M-x all-the-icons-install-fonts
;;     :ensure t
;;     :commands (all-the-icons-dired-mode)))

;; NVM USE
(use-package nvm
  :defer 3
  :config
  (nvm-use "9.0.0"))

;; Ido mode :D
(ido-mode t)

;; Sublimity
(use-package sublimity
  :init
  (sublimity-mode)
  :config
  (require 'sublimity-scroll))

;; enhanced `list-packages'
(use-package paradox
  :ensure t
  :commands paradox-list-packages
  :init
  (setq paradox-github-token t)
  :config
  (evilified-state-evilify paradox-menu-mode paradox-menu-mode-map))

;; show argument list/type information in the modeline
(use-package eldoc
  :diminish eldoc-mode)

;; in-buffer completion
;; (use-package company
;;   :ensure t
;;   :diminish company-mode
;;   :init
;;   (setq company-minimum-prefix-length 2
;;         company-selection-wrap-around t
;;         company-tooltip-align-annotations t)
;;   :config
;; (add-hook 'after-init-hook 'global-company-mode))

;; Autocomplete
(use-package auto-complete
  :defer t
  :config
  (ac-config-default))

(use-package rainbow-delimiters
  :config
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(use-package ztree
  :defer t)

(provide 'base-packages)
;;; base-packages ends here
