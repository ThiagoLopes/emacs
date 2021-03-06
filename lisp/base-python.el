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
    ;; Anaconda doesn't work on remote servers without some work, so
    ;; by default we enable it only when working locally.
    (add-hook 'python-mode-hook
              (lambda () (unless (file-remote-p default-directory)
                      (anaconda-mode 1))))
    (add-hook 'anaconda-mode-hook 'anaconda-eldoc-mode)
    ;; HACK comment this to treat underscore as part of the word
    ;; (add-hook 'python-mode-hook #'(lambda () (modify-syntax-entry ?_ "w")))
    )
  (after-load 'anaconda-mode
    (define-key anaconda-mode-map (kbd "M-?") nil))
  (when (maybe-require-package 'company-anaconda)
    (after-load 'company
      (after-load 'python
        (push 'company-anaconda company-backends))))
  (when (maybe-require-package 'virtualenvwrapper))
  (when (maybe-require-package 'pytest))
  (when (maybe-require-package 'blacken)))

;; If installed elpy
(eval-after-load "elpy"
  '(cl-dolist (key '("C-<return>"))
     (define-key elpy-mode-map (kbd key) nil)))

(provide 'base-python)
;;; init-python.el ends here
