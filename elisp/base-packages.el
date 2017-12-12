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
  (git-gutter:hide-gutter t "disable if not have changes")
  (git-gutter:live-update t "update live")
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; HELM PACKAGES SETTINGS ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package helm
  :ensure t
  :bind (("M-x" . helm-M-x)
         ("C-x f" . helm-recentf)
         ("C-x C-f" . helm-find-files)
         ("M-y" . helm-show-kill-ring)
         ("C-x b" . helm-buffers-list)
         :map helm-find-files-map
         ("<tab>"         . helm-execute-persistent-action)
         ("C-<backspace>" . helm-find-files-up-one-level)
         )
  ;; :bind (:map helm-map
  ;;             ("M-i" . helm-previous-line)
  ;;             ("M-k" . helm-next-line)
  ;;             ("M-I" . helm-previous-page)
  ;;             ("M-K" . helm-next-page)
  ;;             ("M-h" . helm-beginning-of-buffer)
  ;;             ("M-H" . helm-end-of-buffer))
  :config (progn
            (setq helm-buffers-fuzzy-matching t)
            (helm-mode 1)
            )
  (setq helm-ff-skip-boring-files t)
  (setq helm-ff-file-name-history-use-recentf t)
  (setq helm-boring-file-regexp-list
        '("\\.git$" "\\.hg$" "\\.svn$" "\\.CVS$" "\\._darcs$" "\\.la$" "\\.o$" "~$"
          "\\.so$" "\\.a$" "\\.elc$" "\\.fas$" "\\.fasl$" "\\.pyc$" "\\.pyo$"))

  )

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
           ("dired" (mode . dired-mode))
           ("martinowen.net" (filename . "martinowen.net"))
           ("Org" (or (mode . org-mode)
                      (filename . "OrgMode")))
           ("code" (filename . "code"))
           ("Web Dev" (or (mode . html-mode)
                          (mode . css-mode)))
           ("Subversion" (name . "\*svn"))
           ("Magit" (name . "\*magit"))
           ("Pytho" (name . "\*.py"))
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

;; Ibuffer VC
;; (use-package ibuffer-vc
;;   :after
;;   (ibuffer)
;;   :init
;;   (add-hook 'ibuffer-hook
;;             (lambda ()
;;               (ibuffer-vc-set-filter-groups-by-vc-root)
;;               (ibuffer-do-sort-by-alphabetic)))
;;   )

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

;; Diminish
(use-package diminish
  :config
  (diminish 'git-gutter-mode)
  (diminish 'flycheck-mode)
  (diminish 'elpy-mode)
  (diminish 'golden-ratio-mode)
  (diminish 'autopair-mode)
  (diminish 'auto-revert-mode)
  (diminish 'helm-mode)
  )

;; Web Mode
(use-package web-mode
  :mode (("\\.html\\'" . web-mode)
         ("\\.html\\.erb\\'" . web-mode)
         ("\\.mustache\\'" . web-mode)
         ("\\.jinja\\'" . web-mode)
         ("\\.php\\'" . web-mode))
  :init
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-css-indent-offset 2)

  (setq web-mode-script-padding 1)
  (setq web-mode-style-padding 1)

  (setq web-mode-enable-auto-pairing t)
  (setq web-mode-enable-auto-expanding t)
  (setq web-mode-enable-css-colorization t)

  (setq web-mode-enable-block-face t)
  (setq web-mode-enable-current-element-highlight t)
  (setq web-mode-enable-current-column-highlight t)

  (setq web-mode-extra-auto-pairs
        '(("erb"  . (("beg" "end")))
          ("php"  . (("beg" "end")
                     ("beg" "end")))
          ))
  :config
  (progn
    (setq web-mode-engines-alist
          '(("\\.jinja\\'"  . "django"))))
  (setq web-mode-ac-sources-alist
        '(("css" . (ac-source-css-property))
          ("html" . (ac-source-words-in-buffer ac-source-abbrev))))
  )


(use-package web-beautify
  :commands (web-beautify-css
             web-beautify-css-buffer
             web-beautify-html
             web-beautify-html-buffer
             web-beautify-js
             web-beautify-js-buffer))

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

(provide 'base-packages)
;;; base-packages ends here
