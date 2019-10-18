;;; base-dumb-jump.el --- Jump definition -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;----------------------------------------------------------------------------
;; Dumb jump
;;----------------------------------------------------------------------------

(when (require-package 'dumb-jump)
  (global-set-key (kbd "M-.") 'dumb-jump-go)
  (global-set-key (kbd "M-,") 'dumb-jump-back)
  (global-set-key (kbd "C-M-.") 'dumb-jump-quick-look))


  (provide 'base-dumb-jump)
;;; base-dumb-jump.el ends here
