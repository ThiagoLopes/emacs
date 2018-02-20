;;; Code:

;;------------------------------------------------------------------------------
;; newline-without-break-of-line
(defun newline-without-break-of-line ()
  "1. move to end of the line.
  2. insert newline with index"
  (interactive)
  (let ((oldpos (point)))
    (end-of-line)
    (newline-and-indent)))

(global-set-key (kbd "<C-return>") 'newline-without-break-of-line)

;;------------------------------------------------------------------------------
;; Move to beginning of line or indentation
(defun back-to-indentation-or-beginning () (interactive)
       (if (= (point) (progn (back-to-indentation) (point)))
           (beginning-of-line)))

(global-set-key (kbd "C-a") (quote back-to-indentation-or-beginning))

;;------------------------------------------------------------------------------
(defun kill-other-buffers ()
  "Kill all other buffers."
  (interactive)
  (mapc 'kill-buffer (delq (current-buffer) (buffer-list))))

;;------------------------------------------------------------------------------
;; Function use for install packages
;; If package isn't installed, fetch it
(defun require-package (package)
  (setq-default highlight-tabs t)
  "Install given PACKAGE."
  (unless (package-installed-p package)
    (unless (assoc package package-archive-contents)
      (package-refresh-contents))
    (package-install package)))

;;------------------------------------------------------------------------------
;; Test Remap
(eval-after-load "elpy"
  '(cl-dolist (key '("C-<return>"))
     (define-key elpy-mode-map (kbd key) nil)))

;;------------------------------------------------------------------------------
;; Indente file
(defun indent-file (file)
  "prompt for a file and indent it according to its major mode"
  (interactive "fWhich file do you want to indent: ")
  (find-file file)
  ;; uncomment the next line to force the buffer into a c-mode
  ;; (c-mode)
  (indent-region (point-min) (point-max)))

;;------------------------------------------------------------------------------


;; Kill region
(defadvice kill-region (before slick-cut activate compile)
  "When called interactively with no active region, kill a single line instead."
  (interactive
    (if mark-active (list (region-beginning) (region-end))
      (list (line-beginning-position)
            (line-beginning-position 2)))))

;; Pop to mark
(bind-key "C-x p" 'pop-to-mark-command)
(setq set-mark-command-repeat-pop t)

;; auto export to html
(defun toggle-org-html-export-on-save ()
  (interactive)
  (if (memq 'org-html-export-to-html after-save-hook)
      (progn
        (remove-hook 'after-save-hook 'org-html-export-to-html t)
        (message "Disabled org html export on save for current buffer..."))
    (add-hook 'after-save-hook 'org-html-export-to-html nil t)
    (message "Enabled org html export on save for current buffer...")))


(global-set-key (kbd "M-a") 'backward-paragraph)
(global-set-key (kbd "M-e") 'forward-paragraph)

(provide 'base-keybindings)
;;; keybindings.el ends here
