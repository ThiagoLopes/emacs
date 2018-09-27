;;; Code:

;;------------------------------------------------------------------------------
(global-set-key (kbd "C-a") (quote back-to-indentation-or-beginning))
(global-set-key (kbd "M-a") 'backward-paragraph)
(global-set-key (kbd "M-e") 'forward-paragraph)

(global-set-key (kbd "C-<return>") 'smart-open-line)
(global-set-key (kbd "C-M-<return>") 'smart-open-line-above)

(global-set-key (kbd "C-<") 'previous-buffer)
(global-set-key (kbd "C->") 'next-buffer)

;; FIXME
(global-set-key (kbd "M-/") 'undo-tree-undo)
(global-set-key (kbd "M--") 'undo-tree-redo)

(global-set-key (kbd "C-x C-b") 'ivy-switch-buffer)

(global-set-key (kbd "C-s") 'swiper)

(global-set-key (kbd "M-i") 'change-inner)

;; Remap C-<return> to smart-open-line
(eval-after-load "elpy"
  '(cl-dolist (key '("C-<return>"))
     (define-key elpy-mode-map (kbd key) nil)))

;; TODO: move this
;; Pop to mark
(bind-key "C-x p" 'pop-to-mark-command)
(setq set-mark-command-repeat-pop t)


(provide 'base-keybindings)
;;; keybindings.el ends here
