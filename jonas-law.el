;; -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'dash)
(require 'flymake)
(require 'treesit)

;;; Util

(defun jonas-law--query-list (node query)
  (treesit-query-capture node query nil nil 'node-only))

;;; Syntax highlighting

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
    (head (compound (atom) @font-lock-keyword-face))
    (compound (atom) @font-lock-function-name-face)
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

;;; Semantic checks

(defvar jonas-law-known-compounds '())
(defvar jonas-law-explanations-path nil)

(defun jonas-law--head-list (node)
  (let* ((nodes (jonas-law--query-list node '((head (atom) @x)))))
    (cl-destructuring-bind (&optional atom) nodes
      (if atom
          (list atom nil)
        (let ((nodes (jonas-law--query-list node '((head (compound) @x)))))
          (cl-destructuring-bind (&optional compound) nodes
            (treesit-node-children compound 'named)))))))

(defun jonas-law--read-explanations-1 (filename)
  (with-temp-buffer
    (insert-file-contents filename)
    (let* ((parser (treesit-parser-create 'jonas-law-explanations))
           (nodes (jonas-law--query-list parser '((clause) @x))))
      (->> nodes
           (--map (list (car (jonas-law--head-list it))
                        (treesit-node-child it 1 'named)))
           (--map (-map #'treesit-node-text it))))))

(defun jonas-law--read-explanations (directory)
  (-mapcat #'jonas-law--read-explanations-1
           (directory-files directory 'full (rx (+ nonl) ".txt"))))

(defun jonas-law--load-known-compounds ()
  (when jonas-law-explanations-path
    (let* ((path jonas-law-explanations-path)
           (explanations (jonas-law--read-explanations path)))
      (setq jonas-law-known-compounds (-map #'car explanations)))))

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
                    (--remove (member (treesit-node-text it) jonas-law-known-compounds))
                    (--map (jonas-law--diagnostic it :error "Unknown compound term")))))
    (funcall report-fn (append date used))))

;;; Major mode

(define-derived-mode jonas-law-mode prog-mode "Jonas-Law"
  "Major mode for editing Jonas Law code."
  (when (treesit-ready-p 'jonas-law)
    (treesit-parser-create 'jonas-law)
    (setq treesit-font-lock-settings jonas-law-font-lock-settings
          treesit-font-lock-feature-list '((basic)))
    (treesit-major-mode-setup)
    (add-hook 'hack-local-variables-hook #'jonas-law--load-known-compounds nil t)
    (add-hook 'flymake-diagnostic-functions #'jonas-law--flymake nil t)
    (flymake-mode)))

(add-to-list 'auto-mode-alist '("\\.jlaw\\'" . jonas-law-mode))

(provide 'jonas-law)
