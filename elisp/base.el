;;; package --- Main Base file
;;; Commentary:
;;; Base com configurações básicas

;;; Code:
;; Change tab width and change tabs to spaces
(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)

;; Set undo tree
(setq undo-tree-history-directory-alist '(("." . "~/.emacs-backup/undo")))

;; Set backups
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))

;; Disk space is cheap. Save lots.
(setq delete-old-versions -1)
(setq version-control t)
(setq vc-make-backup-files t)
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-save-list/" t)))

;; Save history command
(setq savehist-file "~/.emacs.d/savehist")
(savehist-mode 1)
(setq history-length t)
(setq history-delete-duplicates t)
(setq savehist-save-minibuffer-history 1)
(setq savehist-additional-variables
      '(kill-ring
        search-ring
        regexp-search-ring))


;; Configurando UTF-8
;; UTF-8 please
(set-charset-priority 'unicode)
(setq locale-coding-system   'utf-8)   ; pretty
(set-terminal-coding-system  'utf-8)   ; pretty
(set-keyboard-coding-system  'utf-8)   ; pretty
(set-selection-coding-system 'utf-8)   ; please
(prefer-coding-system        'utf-8)   ; with sugar on top
(setq default-process-coding-system '(utf-8-unix . utf-8-unix))

;; ;; Emacs customizations
(setq confirm-nonexistent-file-or-buffer  t
      save-interprogram-paste-before-kill t
      mouse-yank-at-point                 t
      require-final-newline               t
      visible-bell                        nil
      ring-bell-function                  'ignore
      custom-file                         "~/.emacs.d/.custom.el"
      ;; http://ergoemacs.org/emacs/emacs_stop_cursor_enter_prompt.html
      minibuffer-prompt-properties
      '(read-only t point-entered minibuffer-avoid-prompt face minibuffer-prompt)

      ;; Disable non selected window highlight
      cursor-in-non-selected-windows     nil
      highlight-nonselected-windows      nil
      ;; PATH
      exec-path                          (append exec-path '("/usr/local/bin/"))
      inhibit-startup-message            t
      fringes-outside-margins            t
      x-select-enable-clipboard          t
      use-package-always-ensure          t)


(setq history-length                     999
      backup-inhibited                   nil
      make-backup-files                  t
      auto-save-default                  t
      make-backup-files                  t
      create-lockfiles                   nil)

(fset 'yes-or-no-p 'y-or-n-p)
(global-auto-revert-mode t)

;; Disable toolbar & menubar
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(setq use-file-dialog nil)
(setq use-dialog-box nil)
(setq inhibit-startup-screen t)


;; Time in the modeline
(display-time-mode 1)

;; Remove useless whitespace before saving a file
(add-hook 'before-save-hook 'whitespace-cleanup)
(add-hook 'before-save-hook (lambda() (delete-trailing-whitespace)))

;; Show whitesapces
(setq-default show-trailing-whitespace t)

;; Show collumn number
(column-number-mode 1)

;; Setup `hippie-expand' expand functions
(global-set-key (kbd "M-/") 'hippie-expand)
(setq hippie-expand-try-functions-list '(try-expand-dabbrev
                                         try-expand-dabbrev-all-buffers
                                         try-expand-dabbrev-from-kill
                                         try-complete-file-name-partially
                                         try-complete-file-name
                                         try-expand-all-abbrevs
                                         try-expand-list
                                         try-expand-line
                                         try-complete-lisp-symbol-partially
                                         try-complete-lisp-symbol))

;; display the current column in mode-line
(setq column-number-mode t)

;; set default width to 100 columns
(setq-default fill-column 79)

(setq frame-title-format
      "ｅｍａｃｓ  安ェ殴ど依挨虞")

;;Enable show-paren-mode
(show-paren-mode 1)

;;Disable splash message, start *scratch* buffer by default
;; (setq initial-buffer-choice
;; t)
;; (setq initial-scratch-message
;; "")


;; Uniquify
(require 'uniquify)
(setq uniquify-buffer-name-style 'reverse)
(setq uniquify-separator " • ")
(setq uniquify-after-kill-buffer-p t)
(setq uniquify-ignore-buffers-re "^\\*")


;; Spell if enabled
(when *spell-check-support-enabled*
  (require 'init-spelling))

;; PATH
(let ((path (shell-command-to-string ". ~/.zshrc; echo -n $PATH")))
  (setenv "PATH" path)
  (setq exec-path
        (append
         (split-string-and-unquote path ":")
         exec-path)))


;; Some term enhancement
(defadvice term-sentinel (around my-advice-term-sentinel (proc msg))
  (if (memq (process-status proc) '(signal exit))
      (let ((buffer (process-buffer proc)))
        ad-do-it
        (kill-buffer buffer))
    ad-do-it))
(ad-activate 'term-sentinel)

(defadvice ansi-term (before force-bash)
  (interactive (list "/bin/zsh")))
(ad-activate 'ansi-term)


(provide 'base)
;;; base ends here
