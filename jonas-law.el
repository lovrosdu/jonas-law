;; -*- lexical-binding: t; -*-

(require 'dash)
(require 'flymake)
(require 'treesit)

(defun jonas-law--fontify-date (node override start end)
  (let ((node-start (treesit-node-start node))
        (node-end (treesit-node-end node)))
    (treesit-fontify-with-override
     (max node-start start) (min (+ node-start 2) end)
     font-lock-constant-face override)
    (treesit-fontify-with-override
     (max (+ node-start 2) start) (min node-end end)
     font-lock-string-face override)))

(defvar jonas-law-font-lock-query
  '((head (atom) @font-lock-keyword-face)
    (head (operator (atom) @font-lock-keyword-face))
    (operator (atom) @font-lock-function-name-face)
    ((atom) @font-lock-constant-face)
    ((variable) @font-lock-variable-name-face)
    ((string) @font-lock-string-face)
    ((date) @jonas-law--fontify-date)
    ((number) @font-lock-constant-face)
    ([":-" "(" ")" "," "."] @font-lock-delimiter-face)))

(defvar jonas-law-font-lock-settings
  (treesit-font-lock-rules
   :language 'jonas-law
   :feature 'basic
   jonas-law-font-lock-query))

(defvar jonas-law-allowed-operators '())

(defun jonas-law--date-valid-p (node)
  (string-match (rx-let ((d digit))
                  (rx "#d'" (group d d d d "-" d d "-" d d) "'"))
                (treesit-node-text node)))

(defun jonas-law--diagnostic (node type message)
  (flymake-make-diagnostic
   (current-buffer) (treesit-node-start node) (treesit-node-end node)
   type message))

(defun jonas-law--flymake (report-fn &rest _args)
  (let* ((nodes (treesit-query-capture 'jonas-law jonas-law-font-lock-query))
         (date (->> nodes
                    (--filter (eq (car it) 'jonas-law--fontify-date))
                    (-map #'cdr)
                    (-remove #'jonas-law--date-valid-p)
                    (--map (jonas-law--diagnostic it :warning "Nonstandard date"))))
         (used (->> nodes
                    (--filter (eq (car it) 'font-lock-function-name-face))
                    (-map #'cdr)
                    (--remove (member (treesit-node-text it) jonas-law-allowed-operators))
                    (--map (jonas-law--diagnostic it :error "Unknown operator")))))
    (funcall report-fn (append date used))))

(define-derived-mode jonas-law-mode prog-mode "Jonas-Law"
  "Major mode for editing Jonas Law code."
  (when (treesit-ready-p 'jonas-law)
    (treesit-parser-create 'jonas-law)
    (setq treesit-font-lock-settings jonas-law-font-lock-settings
          treesit-font-lock-feature-list '((basic)))
    (treesit-major-mode-setup)
    (add-hook 'flymake-diagnostic-functions #'jonas-law--flymake nil t)
    (flymake-mode)))

(add-to-list 'auto-mode-alist '("\\.jlaw\\'" . jonas-law-mode))

(provide 'jonas-law)
