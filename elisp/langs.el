;; Simple script

(setq-default js2-basic-offset 2
              js-indent-level 2)
(use-package js2-mode
  :mode ("\\.js\\'"))
(use-package typescript-mode
  :mode ("\\.ts\\'"))
(use-package prettier-js
  :mode ("\\.js\\'"))

(setq-default css-indent-offset 2
              web-mode-css-indent-offset 2)
(use-package rainbow-mode)

(setq-default web-mode-markup-indent-offset 2
              web-mode-code-indent-offset 2
              web-mode-attr-indent-offset 2)
(use-package web-mode
  :mode ("\\.html\\'")
  :config
  (setq web-mode-markup-indent-offset 2))
(use-package emmet-mode
  :mode "\\.html?\\'"
  :config
  (add-hook 'sgml-mode-hook 'emmet-mode))

(use-package elm-mode
  :mode ("\\.elm\\'"))

(use-package go-mode
  :mode ("\\.go\\'"))

(setq-default python-indent 4)
(use-package python
  :defer 1
  :mode ("\\.py\\'" . python-mode))
(use-package elpy
  :after python
  :init
  (elpy-enable)
  (add-to-list 'auto-mode-alist '("\\.py$" . python-mode))
  :bind (:map elpy-mode-map
              ("M-." . elpy-goto-definition)
              ("M-," . pop-tag-mark)))
(use-package py-yapf)
(use-package py-isort)
(use-package virtualenvwrapper
  :disabled
  :config
  (setq venv-location "~/.virtualenvs"))

(provide 'langs)
