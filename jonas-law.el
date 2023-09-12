;; -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'dash)
(require 'filenotify)
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

(defvar jonas-law-known-functors '())
(defvar jonas-law-explanations-path nil)
(defvar jonas-law--watchers '())

(defun jonas-law--explanation-files (directory)
  (directory-files directory 'full (rx ".txt" eos)))

(defun jonas-law--read-explanations-1 (filename)
  (with-temp-buffer
    (insert-file-contents filename)
    (let* ((parser (treesit-parser-create 'jonas-law-explanations))
           (nodes (jonas-law--query-list parser '((clause) @x))))
      (--map (cl-destructuring-bind (compound explanation)
                 (treesit-node-children it 'named)
               (-map #'treesit-node-text
                     (append (treesit-node-children compound 'named)
                             (list explanation))))
             nodes))))

(defun jonas-law--load-known-functors ()
  (when-let ((path jonas-law-explanations-path))
    (let ((explanations (-mapcat #'jonas-law--read-explanations-1
                                 (jonas-law--explanation-files path))))
      (setq-local jonas-law-known-functors (-map #'car explanations)))))

(defun jonas-law--remove-watchers ()
  (interactive)
  (dolist (w jonas-law--watchers)
    (file-notify-rm-watch w))
  (setq-local jonas-law--watchers '()))

(defun jonas-law--watcher (filename buffer)
  (lambda (event)
    (cl-destructuring-bind (_d action &rest _f) event
      (when (eq action 'changed)
        (with-current-buffer buffer
          (jonas-law--load-known-functors)
          (flymake-start))))))

(defun jonas-law--refresh-watchers ()
  (jonas-law--remove-watchers)
  (let ((buffer (current-buffer)))
    (dolist (f (jonas-law--explanation-files jonas-law-explanations-path))
      (push (file-notify-add-watch f '(change) (jonas-law--watcher f buffer))
            jonas-law--watchers))))

(defun jonas-law--date-valid-p (node)
  (string-match (rx-let ((d digit))
                  (rx "#d'" d d d d "-" d d "-" d d "'"))
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
         (defined (->> nodes
                       (--filter (eq (car it) 'font-lock-keyword-face))
                       (-map (-compose #'treesit-node-text #'cdr))))
         (known (append defined jonas-law-known-functors))
         (used (->> nodes
                    (--filter (eq (car it) 'font-lock-function-name-face))
                    (-map #'cdr)
                    (--remove (member (treesit-node-text it) known))
                    (--map (jonas-law--diagnostic it :error "Unknown functor")))))
    (funcall report-fn (append date used))))

;;; Major mode

(defun jonas-law--watchers-hook ()
  (jonas-law--load-known-functors)
  (jonas-law--refresh-watchers))

(define-derived-mode jonas-law-mode prog-mode "Jonas-Law"
  "Major mode for editing Jonas Law code."
  (when (treesit-ready-p 'jonas-law)
    (treesit-parser-create 'jonas-law)
    (setq treesit-font-lock-settings jonas-law-font-lock-settings
          treesit-font-lock-feature-list '((basic)))
    (treesit-major-mode-setup)
    (add-hook 'hack-local-variables-hook #'jonas-law--watchers-hook nil t)
    (add-hook 'change-major-mode-hook #'jonas-law--remove-watchers nil t)
    (add-hook 'kill-buffer-hook #'jonas-law--remove-watchers nil t)
    (add-hook 'flymake-diagnostic-functions #'jonas-law--flymake nil t)
    (flymake-mode)))

(add-to-list 'auto-mode-alist `(,(rx ".jlaw" eos) . jonas-law-mode))

(provide 'jonas-law)
