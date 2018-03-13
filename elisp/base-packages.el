;; Emacs configuration file
;; Forked from: Auralcat, Sacha Chua's
;; Author: Thiago Lopes
;; Started in May 2017.

;;; Code:
(require-package 'use-package)
(eval-when-compile
  (require 'use-package)
  (use-package auto-compile
    :config (auto-compile-on-load-mode))
  (setq load-prefer-newer t)
  (setq use-package-always-ensure t)
  )

;; Dash
(use-package dash)

;; Winner
(use-package winner)

;; Smart Mode line
(use-package smart-mode-line)

;; Which-key
(use-package which-key
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

(use-package swiper-helm
  :bind
  ("C-s" . swiper)
  )

;; Org
(use-package org
  :defer 2)

;; Highlight symbol
(use-package symbol-overlay
  :defer 2
  :config
  (global-set-key [(control f3)] 'symbol-overlay-put)
  (global-set-key [f3] 'symbol-overlay-jump-next)
  (global-set-key [(shift f3)] 'symbol-overlay-jump-prev)
  ;; (global-set-key [(meta f3)] 'highlight-symbol-query-replace)
  )

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

;; Changer inner - clone ci vim
(use-package change-inner
  :defer t
  :init
  (bind-key "M-i" 'change-inner)
  )

;; Git gutter mode
(use-package git-gutter
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
  (global-git-gutter-mode t)
  (custom-set-variables '(git-gutter:hide-gutter t))
  )

;; Zoom
(use-package zoom
  :defer 1)

(custom-set-variables
 '(zoom-mode t)
 '(zoom-size '(0.618 . 0.618)))

;; Beacon
(use-package beacon
  :init
  (beacon-mode 1))

;; Clean up obsolete buffers
(use-package midnight
  :defer t
  :init
  (midnight-mode))

;; hl todo
(use-package hl-todo
  :init
  (global-hl-todo-mode))

;; Smartparens
(use-package smartparens
  :init
  (setq smartparens-global-mode t)
  :config
  (setq electric-pair-mode t)
  (setq electric-quote-mode t)
  )

;; Smex
(use-package smex
  :init
  (smex-initialize)
  :bind
  ("M-x" . smex))

;; NVM USE
(use-package nvm
  :defer 3
  :config
  (nvm-use "9.6.1"))

;; Ido mode :D
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode t)

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

(use-package rainbow-delimiters
  :config
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(use-package ztree
  :defer t)

(use-package esup)

(use-package undo-tree
  :init
  (global-undo-tree-mode))

;; yasnippet
(use-package yasnippet
  :defer t)
(use-package yasnippet-snippets
  :defer t)

;; Company
(use-package company
  :ensure t
  :init
  (global-company-mode)
  :config
  (progn
    ;; Use Company for completion
    (bind-key [remap completion-at-point] #'company-complete company-mode-map)

    (setq company-tooltip-align-annotations t
          ;; Easy navigation to candidates with M-<n>
          company-show-numbers t)
    (setq company-dabbrev-downcase nil))
  :diminish company-mode)

(use-package company-quickhelp          ; Documentation popups for Company
  :ensure t
  :defer t
  :init (add-hook 'global-company-mode-hook #'company-quickhelp-mode))

;; Counsel
(use-package counsel
  :bind
  (("M-y" . counsel-yank-pop)
   :map ivy-minibuffer-map
   ("M-y" . ivy-next-line)))

;; ag
(use-package ag)

;; fill collumn
(use-package fill-column-indicator)
(defun column-indicator()
  (fci-mode))

(provide 'base-packages)
;;; base-packages ends here
