;; Emacs configuration file
;; Forked from: Auralcat, Sacha Chua's
;; Author: Thiago Lopes
;; Started in May 2017.

;;; Code:

(when (maybe-require-package 'expand-region)
  (global-set-key (kbd "M-@") 'er/expand-region))

(when (maybe-require-package 'move-text)
  (move-text-default-bindings))

(when (maybe-require-package 'symbol-overlay)
  (global-set-key (kbd "<f9>") 'symbol-overlay-put)
  (global-set-key (kbd "C-x <f9>") 'symbol-overlay-jump-next))

(when (maybe-require-package 'ivy-rich)
  (ivy-rich-mode 1)
  (setq ivy-rich-path-style 'abbrev))

(when (maybe-require-package 'flycheck-popup-tip)
  (add-hook 'flycheck-mode-hook 'flycheck-popup-tip-mode)
  (flycheck-popup-tip-mode))

(when (maybe-require-package 'switch-window)
  (global-set-key (kbd "C-x o") 'switch-window)
  (setq switch-window-shortcut-style 'qwerty)
  (setq switch-window-qwerty-shortcuts
        '("a" "s" "d" "f" "j" "k" "l" ";" "w" "e" "i" "o")))

(when (maybe-require-package 'change-inner))

(when (maybe-require-package 'zoom)
  (zoom-mode t)
  (custom-set-variables
   '(zoom-size '(0.618 . 0.618))))

(when (maybe-require-package 'beacon)
  (beacon-mode t))

(when (maybe-require-package 'midnight)
  (midnight-mode))

(when (maybe-require-package 'hl-todo)
  (global-hl-todo-mode))

(when (maybe-require-package 'ag))

(when (maybe-require-package 'spaceline)
  (spaceline-emacs-theme)
  (spaceline-toggle-minor-modes-off)
  (spaceline-toggle-buffer-size-off))

(when (maybe-require-package 'highlight-indent-guides)
  (add-hook 'prog-mode-hook 'highlight-indent-guides-mode)
  (setq highlight-indent-guides-auto-odd-face-perc 15)
  (setq highlight-indent-guides-auto-even-face-perc 15)
  (setq highlight-indent-guides-auto-character-face-perc 30)
  (setq highlight-indent-guides-method (quote character)))

(when (maybe-require-package 'counsel)
  (global-set-key (kbd "M-y") 'counsel-yank-pop ))

(when (maybe-require-package 'ace-jump-mode)
  (global-set-key (kbd "C-c SPC") 'ace-jump-mode))

(when (maybe-require-package 'evil))

(provide 'base-packages)
;;; base-packages ends here
