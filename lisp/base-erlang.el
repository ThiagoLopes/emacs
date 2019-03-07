;;; init-erlang.el --- Support for the Erlang language -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(when (maybe-require-package 'erlang)
  (require 'erlang-start))


(provide 'base-erlang)
;;; init-erlang.el ends here
