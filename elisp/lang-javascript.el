;; js2-mode
;; https://github.com/mooz/js2-mode

(use-package js2-mode
  :commands js2-mode
  :mode "\\.js?\\'"
  :init
  (progn
    (add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
    (setq-default js2-basic-offset 2)
    (add-to-list 'interpreter-mode-alist (cons "node" 'js2-mode)))
  :config
  (progn
    (js2-imenu-extras-setup)
    (bind-key "C-x C-e" 'js-send-last-sexp js2-mode-map)
    (bind-key "C-M-x" 'js-send-last-sexp-and-go js2-mode-map)
    (bind-key "C-c b" 'js-send-buffer js2-mode-map)
    (bind-key "C-c d" 'my/insert-or-flush-debug js2-mode-map)
    (bind-key "C-c C-b" 'js-send-buffer-and-go js2-mode-map)
    (bind-key "C-c w" 'my/copy-javascript-region-or-buffer js2-mode-map)))

(add-to-list 'auto-mode-alist '("\\.js\\'\\|\\.json\\'" . js2-mode))

(provide 'lang-javascript)
