;;; package --- golang configs
;;; Commentary:
;;; Contains my golang configs

;;; Code:

(use-package go-mode
   :ensure t
  :init
  (add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode)))


(provide 'lang-go)
