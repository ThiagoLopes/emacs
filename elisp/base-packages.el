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

;; Better default
(use-package better-defaults)

;; Dash
(use-package dash)

;; Winner
(use-package winner)

;; Smart Mode line
(use-package smart-mode-line)

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
  (global-git-gutter-mode t))

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
  (nvm-use "9.0.0"))

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

;; Autocomplete
(use-package auto-complete
  :config
  (ac-config-default))

(use-package rainbow-delimiters
  :config
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(use-package ztree
  :defer t)

(use-package esup)

(provide 'base-packages)
;;; base-packages ends here
