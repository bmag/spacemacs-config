(defun python-split-args (arg-string)
  "Split a python argument string into ((name, default)..) tuples"
  (mapcar (lambda (x)
             (split-string x "[[:blank:]]*=[[:blank:]]*" t))
          (split-string arg-string "[[:blank:]]*,[[:blank:]]*" t)))

(defun make-indent (to-column)
  (make-string to-column 32))

(defun make-arg-line (arg)
  (let ((name (nth 0 arg))
        (default (nth 1 arg)))
    ;; (setq my-last-field (1+ my-last-field))
    (cond ((string-prefix-p "**" name)
           (format ":type %s: {}" name))
          ((string-prefix-p "*" name)
           ;; (format ":type %s: ($%s)" name my-last-field))
           (format ":type %s: ()" name))
          (t
           ;; (format ":type %s: $%s" name my-last-field)))
           (format ":type %s:" name)))))

(defun make-indented-arg-line (arg indent)
  (concat indent (make-arg-line arg)))

(defun python-args-docstring ()
  (let* ((my-last-field 3)
         (indent (make-indent (current-column)))
         (newline-and-indent (concat "\n" indent))
         (args (python-split-args yas-text))
         (arg-lines (mapcar #'make-arg-line args)))
    (mapconcat #'identity arg-lines newline-and-indent)))

(add-hook 'python-mode-hook
          '(lambda () (set (make-local-variable 'yas-indent-line) 'fixed)))
