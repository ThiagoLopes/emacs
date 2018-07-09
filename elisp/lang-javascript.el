(use-package json-mode
  :defer t)
(use-package js2-mode
  :defer t
  :config
  ;; Disable js2 mode's syntax error highlighting by default...
  (setq-default js2-mode-show-parse-errors nil
                js2-mode-show-strict-warnings nil)
  ;; ... but enable it if flycheck can't handle javascript
  (autoload 'flycheck-get-checker-for-buffer "flycheck")
  (defun sanityinc/disable-js2-checks-if-flycheck-active ()
    (unless (flycheck-get-checker-for-buffer)
      (set (make-local-variable 'js2-mode-show-parse-errors) t)
      (set (make-local-variable 'js2-mode-show-strict-warnings) t)))
  (add-hook 'js2-mode-hook 'sanityinc/disable-js2-checks-if-flycheck-active)
  (setq-default js2-basic-offset 2 js2-bounce-indent-p nil)
  (add-hook 'js2-mode-hook (lambda () (setq mode-name "JS2")))
  (js2-imenu-extras-setup)
  )

(use-package coffee-mode
  :defer t)
(use-package typescript-mode
  :defer t)
(use-package prettier-js
  :defer t)

(defcustom preferred-javascript-mode
  :type 'symbol
  :group 'programming
  :options '(js2-mode js-mode))

(defconst preferred-javascript-indent-level 2)

;; Need to first remove from list if present, since elpa adds entries too, which
;; may be in an arbitrary order
(eval-when-compile (require 'cl))
(setq auto-mode-alist (cons `("\\.\\(js\\|es6\\)\\(\\.erb\\)?\\'" . ,preferred-javascript-mode)
                            (loop for entry in auto-mode-alist
                                  unless (eq preferred-javascript-mode (cdr entry))
                                  collect entry)))

;; js-mode
(setq-default js-indent-level preferred-javascript-indent-level)

(add-to-list 'interpreter-mode-alist (cons "node" preferred-javascript-mode))

(provide 'lang-javascript)
