;;; package --- Main Base file
;;; Commentary:
;;; Basic configs

;;; Code:
;; Change tab width and change tabs to spaces
(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)

;; Spell
(defconst *spell-check-support-enabled* nil) ;; Enable with t if you prefer

;; Disk space is cheap. Save lots.
(setq delete-old-versions -1)

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

;; UTF-8 please
(set-charset-priority 'unicode)
(setq locale-coding-system   'utf-8)   ; pretty
(set-terminal-coding-system  'utf-8)   ; pretty
(set-keyboard-coding-system  'utf-8)   ; pretty
(set-selection-coding-system 'utf-8)   ; please
(prefer-coding-system        'utf-8)   ; with sugar on top
(setq default-process-coding-system '(utf-8-unix . utf-8-unix))

;; Disable blink cursor
(blink-cursor-mode 0)

;; Backup
(setq make-backup-files nil) ; stop creating backup~ files
(setq auto-save-default nil) ; stop creating #autosave# files
(setq create-lockfiles nil)  ; stop creating .# files

;; Variables
(setq confirm-nonexistent-file-or-buffer  t
      save-interprogram-paste-before-kill t
      mouse-yank-at-point                 nil
      require-final-newline               t
      visible-bell                        nil
      ring-bell-function                  'ignore
      custom-file                         "~/.emacs.d/.custom.el"
      minibuffer-prompt-properties
      '(read-only t point-entered minibuffer-avoid-prompt face minibuffer-prompt)
      ;; highlight-nonselected-windows   t
      ;; fringes-outside-margins          t
      x-select-enable-clipboard          t
      use-package-always-ensure          t)

(setq inhibit-startup-message t         ; Don't show the startup message
      inhibit-startup-screen t          ; or screen
      cursor-in-non-selected-windows t  ; Hide the cursor in inactive windows
      echo-keystrokes 0.1               ; Show keystrokes right away, don't show the message in the scratch buffe
      initial-scratch-message nil       ; Empty scratch buffer
      initial-major-mode 'org-mode      ; org mode by default
      sentence-end-double-space nil     ; Sentences should end in one space, come on!
      confirm-kill-emacs 'y-or-n-p      ; y and n instead of yes and no when quitting
      help-window-select t              ; select help window so it's easy to quit it with 'q'
)


;; UI
(setq use-file-dialog nil)
(setq use-dialog-box nil)(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(fset 'yes-or-no-p 'y-or-n-p)      ; y and n instead of yes and no everywhere else
(global-auto-revert-mode t)
(delete-selection-mode 1)
(global-unset-key (kbd "s-p"))
(global-hl-line-mode nil)

;; Time in the modeline
(display-time-mode 1)

;; Remove useless whitespace before saving a file
(add-hook 'before-save-hook 'whitespace-cleanup)
(add-hook 'before-save-hook (lambda() (delete-trailing-whitespace)))

;; Show whitesapces
(setq-default show-trailing-whitespace t)

;; Show collumn number
(setq column-number-mode t)

;; Save and kill
(global-set-key (kbd "s-q") 'save-buffers-kill-emacs) ;; quit

;; ;; Setup `hippie-expand' expand functions
;; (global-set-key (kbd "M-/") 'hippie-expand)
;; (setq hippie-expand-try-functions-list '(try-expand-dabbrev
;;                                          try-expand-dabbrev-all-buffers
;;                                          try-expand-dabbrev-from-kill
;;                                          try-complete-file-name-partially
;;                                          try-complete-file-name
;;                                          try-expand-all-abbrevs
;;                                          try-expand-list
;;                                          try-expand-line
;;                                          try-complete-lisp-symbol-partially
;;                                          try-complete-lisp-symbol))

;;Enable show-paren-mode
(show-paren-mode 1)

;; Uniquify
(require 'uniquify)
(setq uniquify-buffer-name-style 'reverse)
(setq uniquify-separator " • ")
(setq uniquify-after-kill-buffer-p t)
(setq uniquify-ignore-buffers-re "^\\*")

(provide 'base)
;;; base ends here
