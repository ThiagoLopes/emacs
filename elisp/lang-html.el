;; My functions
(defun my/clean-up-spans-in-region (beg end)
  (interactive "r")
  (save-excursion
    (let ((changed t))
      (while changed
        (setq changed nil)
        (goto-char beg)
        (while (re-search-forward "<span>\\([^<]*\\)</span>" end t)
          (replace-match "\\1")
          (setq changed t)))
      (setq changed t)
      (while changed
        (setq changed nil)
        (goto-char beg)
        (while (re-search-forward "<span>*\\(<a[^<]+>[^<]*</a>\\)</span>" end t)
          (replace-match "\\1")
          (setq changed t))))))

(defun my/clean-up-spans-in-string (string)
  (with-temp-buffer
    (insert string)
    (my/clean-up-spans-in-region (point-min) (point-max))
    (buffer-string)))

(ert-deftest my/clean-up-spans-in-string ()
  (should (string= (my/clean-up-spans-in-string "<span><span>Hello world</span></span>")
                   "Hello world"))
  (should (string= (my/clean-up-spans-in-string "<span><span><a href=\"http://example.com\">Hello another world</a></span></span>")
                   "<a href=\"http://example.com\">Hello another world</a>"))
  (should (string= (my/clean-up-spans-in-string "<span><h1>Leave alone</h1></span>") "<span><h1>Leave alone</h1></span>"))
  (should (string= (my/clean-up-spans-in-string "<span><a href=\"http://example.com\">Leave</a> alone</span>")
                   "<span><a href=\"http://example.com\">Leave</a> alone</span>")))




;; Web Mode
;; from FAQ at http://web-mode.org/ for smartparens
(defun my/web-mode-hook ()
  (setq web-mode-enable-auto-pairing nil))

(defun my/sp-web-mode-is-code-context (id action context)
  (when (and (eq action 'insert)
             (not (or (get-text-property (point) 'part-side)
                      (get-text-property (point) 'block-side))))
    t))

(use-package web-mode
  :defer t
  :mode "\\.html?\\'"
  :config
  (progn
    (setq web-mode-markup-indent-offset 2)
    (setq web-mode-code-indent-offset 2)
    (setq web-mode-enable-current-element-highlight t)
    (setq web-mode-ac-sources-alist
          '(("css" . (ac-source-css-property))
            ("html" . (ac-source-words-in-buffer ac-source-abbrev)))
          )))

;; Emmet
(use-package emmet-mode
  :defer t
  :config
  (add-hook 'sgml-mode-hook 'emmet-mode)
  )

;; Rainbow mode - CSS
(use-package rainbow-mode
  :defer t)

;; `markdown' mode
(use-package markdown-mode
  :ensure t
  :mode ("\\.md\\'" . markdown-mode))


;; Sass suport
(add-hook 'sass-mode-hook
          (lambda () (setq indent-tabs-mode nil)))
(setq-default indent-tabs-mode nil)


(provide 'lang-html)
