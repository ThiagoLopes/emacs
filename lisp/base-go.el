;;; base-go.el --- Measure startup and require times -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(when (maybe-require-package 'go-mode)
  (add-auto-mode 'go-mode "\\.[g][o]\\'"))

(provide 'base-go)
;;; base-go.el ends here
