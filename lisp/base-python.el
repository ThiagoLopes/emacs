;;; init-python.el --- Python editing -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(setq auto-mode-alist
      (append '(("SConstruct\\'" . python-mode)
                ("SConscript\\'" . python-mode))
              auto-mode-alist))

(require-package 'pip-requirements)

(when (maybe-require-package 'anaconda-mode)
  (after-load 'python
    (add-hook 'python-mode-hook 'anaconda-mode)
    (add-hook 'python-mode-hook 'anaconda-eldoc-mode)
    ;; HACK comment this to treat underscore as part of the word
    ;; (add-hook 'python-mode-hook #'(lambda () (modify-syntax-entry ?_ "w")))
    )
  (when (maybe-require-package 'company-anaconda)
    (after-load 'company
      (after-load 'python
        (push 'company-anaconda company-backends))))
  (when (maybe-require-package 'virtualenvwrapper))
  (when (maybe-require-package 'pytest)))

;; If installed elpy
(eval-after-load "elpy"
  '(cl-dolist (key '("C-<return>"))
     (define-key elpy-mode-map (kbd key) nil)))

(provide 'base-python)
;;; init-python.el ends here
