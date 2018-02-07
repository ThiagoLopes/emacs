;;; package --- python configs
;;; Commentary:
;;; Contains my python configs

;;; Code:

(use-package python
  :defer t
  :mode ("\\.py" . python-mode)
  :config
  (use-package elpy
    :init
    (add-to-list 'auto-mode-alist '("\\.py$" . python-mode))
    :bind (:map elpy-mode-map
                ("M-." . elpy-goto-definition)
                ("M-," . pop-tag-mark)))
  (elpy-enable)
  )

(use-package py-autopep8
  :defer t)

(use-package py-yapf
  :defer t)

(use-package py-isort
  :defer t)

(use-package virtualenvwrapper
  :defer 3
  :config
  (setq venv-location "~/.virtualenv"))

(provide 'lang-python)
;;; base-python.el ends here
