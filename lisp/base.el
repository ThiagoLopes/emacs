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
(savehist-mode 1)
(setq savehist-file "~/.emacs.d/savehist")
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
      ;; fringes-outside-margins         t
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
(menu-bar-mode -1)
;; (fset 'yes-or-no-p 'y-or-n-p)      ; y and n instead of yes and no everywhere else
(global-auto-revert-mode t)
(delete-selection-mode 1)
(global-unset-key (kbd "s-p"))
(global-hl-line-mode nil)

;; Font
;; Good fonts to code - Hack, Iosevka, Terminus, PragmataPro
(condition-case nil
    (set-face-attribute 'default nil :font "Hack 10")
  (error nil))

;; (setq-default line-spacing 4)
(setq-default frame-title-format "%b (%f)")

;; Remove useless whitespace before saving a file
(add-hook 'before-save-hook 'whitespace-cleanup)
(add-hook 'before-save-hook (lambda() (delete-trailing-whitespace)))

;; Show whitesapces
(setq-default show-trailing-whitespace t)

;; Show collumn number
(setq column-number-mode t)

;; Save and kill
(global-set-key (kbd "s-q") 'save-buffers-kill-emacs) ;; quit

;;Enable show-paren-mode
(show-paren-mode 1)

;;----------------------------------------------------------------------------
;; Suppress GUI features
;;----------------------------------------------------------------------------
(setq use-file-dialog nil)
(setq use-dialog-box nil)
(setq inhibit-startup-screen t)

;;----------------------------------------------------------------------------
;; Window size and features
;;----------------------------------------------------------------------------
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'set-scroll-bar-mode)
  (set-scroll-bar-mode nil))

;; I generally prefer to hide the menu bar, but doing this on OS X
;; simply makes it update unreliably in GUI frames, so we make an
;; exception.

(let ((no-border '(internal-border-width . 0)))
  (add-to-list 'default-frame-alist no-border)
  (add-to-list 'initial-frame-alist no-border))

(defun sanityinc/adjust-opacity (frame incr)
  "Adjust the background opacity of FRAME by increment INCR."
  (unless (display-graphic-p frame)
    (error "Cannot adjust opacity of this frame"))
  (let* ((oldalpha (or (frame-parameter frame 'alpha) 100))
         ;; The 'alpha frame param became a pair at some point in
         ;; emacs 24.x, e.g. (100 100)
         (oldalpha (if (listp oldalpha) (car oldalpha) oldalpha))
         (newalpha (+ incr oldalpha)))
    (when (and (<= frame-alpha-lower-limit newalpha) (>= 100 newalpha))
      (modify-frame-parameters frame (list (cons 'alpha newalpha))))))

;; TODO: use seethru package instead?
(global-set-key (kbd "M-C-8") (lambda () (interactive) (sanityinc/adjust-opacity nil -2)))
(global-set-key (kbd "M-C-9") (lambda () (interactive) (sanityinc/adjust-opacity nil 2)))
(global-set-key (kbd "M-C-7") (lambda () (interactive) (modify-frame-parameters nil `((alpha . 100)))))

(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))

;; Non-zero values for `line-spacing' can mess up ansi-term and co,
;; so we zero it explicitly in those cases.
(add-hook 'term-mode-hook
          (lambda ()
            (setq line-spacing 0)))


;; Change global font size easily

(require-package 'default-text-scale)
(add-hook 'after-init-hook 'default-text-scale-mode)



(require-package 'disable-mouse)

(provide 'base)
;;; base ends here
