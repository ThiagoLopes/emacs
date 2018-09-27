;; Emacs configuration file
;; Forked from: Auralcat, Sacha Chua's
;; Author: Thiago Lopes
;; Started in May 2017.

;;; Code:
(use-package which-key
  :config
  (which-key-mode)
  (setq which-key-idle-delay 0.5))

(use-package dimmer
  :config
  (dimmer-mode t))

(use-package dash)

(use-package undo-tree
  :init
  (progn
    (global-undo-tree-mode)
    (setq undo-tree-auto-save-history nil)
    (setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/tmp/undo"))
          undo-tree-auto-save-history t
          undo-tree-visualizer-timestamps t
          undo-tree-visualizer-diff t)))

;;(use-package smart-mode-line)

(use-package smartscan
  :config (global-smartscan-mode t))

(use-package expand-region
  :config
  (global-set-key (kbd "M-S-^ M-S-@") 'er/expand-region))

(use-package move-text
  :config
  (move-text-default-bindings))

(use-package ivy
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "(%d/%d) ")
  (setq enable-recursive-minibuffers t)
  (setq ivy-initial-inputs-alist nil)
  (setq ivy-re-builders-alist
      '((swiper . ivy--regex-plus)
        (t      . ivy--regex-fuzzy))))

(use-package swiper)

(use-package org)

(use-package symbol-overlay
  :defer 2
  :config
  (global-set-key [(control f3)] 'symbol-overlay-put)
  (global-set-key [f3] 'symbol-overlay-jump-next)
  (global-set-key [(shift f3)] 'symbol-overlay-jump-prev)
  (global-set-key [(meta f3)] 'highlight-symbol-query-replace))

(use-package counsel
  :bind (("M-y" . counsel-yank-pop)
         :map ivy-minibuffer-map
         ("M-y" . ivy-next-line))
  :config
  (global-set-key (kbd "M-x") 'counsel-M-x)
  (global-set-key (kbd "C-x C-f") 'counsel-find-file))

(use-package smex)
(use-package flx)
(use-package avy)

(use-package ivy-rich
  :config
  (ivy-rich-mode 1)
  (setq ivy-rich-path-style 'abbrev))

(use-package flycheck
  :config
  (global-flycheck-mode)
  (add-hook 'after-init-hook #'global-flycheck-mode)
  (setq flycheck-highlighting-mode 'lines))

(use-package flycheck-popup-tip
  :after (flycheck)
  :config
  (add-hook 'flycheck-mode-hook 'flycheck-popup-tip-mode)
  (flycheck-popup-tip-mode))

(use-package magit)

(use-package magit-popup
  :after (magit))

(use-package switch-window
  :config
  (bind-key "C-x o" 'switch-window)
  (setq switch-window-shortcut-style 'qwerty)
  (setq switch-window-qwerty-shortcuts
        '("a" "s" "d" "f" "j" "k" "l" ";" "w" "e" "i" "o")))

(use-package change-inner)

(use-package git-gutter
  :diminish git-gutter-mode
  :init
  (setq git-gutter:window-width 1
        git-gutter:update-interval 2
        git-gutter:modified-sign "│"
        ;; git-gutter:added-sign "│"
        git-gutter:deleted-sign "│"
        )
  :config
  (add-to-list 'git-gutter:update-hooks 'focus-in-hook)
  (add-hook 'magit-post-refresh-hook 'git-gutter:update-all-windows)
  (custom-set-variables '(git-gutter:hide-gutter t))
  (global-git-gutter-mode +1))

(use-package zoom
  :config
  (zoom-mode t)
  (custom-set-variables
   '(zoom-size '(0.618 . 0.618))))

(use-package beacon
  :config
  (beacon-mode t))

(use-package exec-path-from-shell)

(use-package midnight
  :config
  (midnight-mode))

(use-package hl-todo
  :config
  (global-hl-todo-mode))

(use-package smartparens
  :init
  (setq smartparens-global-mode t)
  :config
  (setq electric-pair-mode t)
  (setq electric-quote-mode t)
  )

(use-package rainbow-delimiters
  :config
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(use-package yasnippet)

(use-package yasnippet-snippets)

(use-package company
  :config
  (setq company-idle-delay 0.1)
  (setq company-global-modes '(not org-mode markdown-mode))
  (setq company-minimum-prefix-length 1)
  (add-hook 'after-init-hook 'global-company-mode))

(use-package company-quickhelp
  :after
  (company)
  :init
  (add-hook 'global-company-mode-hook #'company-quickhelp-mode))

(use-package ag)

(use-package symon)

(use-package auto-package-update
  :ensure t
  :config
  (setq auto-package-update-delete-old-versions t
        auto-package-update-interval 4)
  (auto-package-update-maybe))

(use-package yaml-mode)
(use-package markdown-mode)
(use-package haml-mode)
(use-package json-mode)

;; (use-package spaceline
;;   :init
;;   (setq powerline-default-separator 'slant)
;;   :config
;;   (spaceline-emacs-theme)
;;   (spaceline-toggle-minor-modes-off)
;;   (spaceline-toggle-buffer-size-off))

(provide 'base-packages)
;;; base-packages ends here
