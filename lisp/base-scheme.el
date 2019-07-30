;;; init-python.el --- Scheme editing -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(when (maybe-require-package 'racket-mode)
  (require 'racket-mode))


(when (maybe-require-package 'chicken-scheme))

(provide 'base-scheme)
;;; init-elm.el ends here
