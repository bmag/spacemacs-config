;;; purposes

(defun winconf2/set-purpose (conf-obj slot key value test)
  (eieio-oset conf-obj slot
              (append (list (cons key value))
                      (cl-delete key (eieio-oref conf-obj slot) :key #'car :test test))))

(defun winconf2/add-purpose (type key purpose)
  (unless (s-uppercase? (symbol-name purpose))
    (error "Winconf allows only uppercase purposes"))
  (cl-case type
    (name (winconf2/set-purpose winconf2-base-purpose-conf :name-purposes
                                key purpose #'string=))
    (regexp (winconf2/set-purpose winconf2-base-purpose-conf :regexp-purposes
                                  key purpose #'string=))
    (mode (winconf2/set-purpose winconf2-base-purpose-conf :mode-purposes
                                key purpose #'eq))
    (t (error "Unknown type: %S, should be one of: %s" type '(name regexp mode)))))

(defun winconf2/reset-purpose-conf (conf-obj)
  (oset conf-obj :name-purposes nil)
  (oset conf-obj :regexp-purposes nil)
  (oset conf-obj :mode-purposes nil))

(defun winconf2/generate-purpose-conf ()
  (winconf2/reset-purpose-conf winconf2-base-purpose-conf)
  (winconf2/add-purpose 'name "*Ilist*" 'ILIST)
  (winconf2/add-purpose 'name "*anaconda-doc*" 'HELP)
  (winconf2/add-purpose 'mode 'help-mode 'HELP)
  (dolist (mode '(inferior-emacs-lisp-mode inferior-python-mode))
    (winconf2/add-purpose 'mode mode 'REPL)))

(defun winconf2/remove-display-actions ()
  (setq purpose-special-action-sequences
        (cl-delete-if (lambda (item) (memq (car item) '(REPL HELP ILIST)))
                      purpose-special-action-sequences)))

(defun winconf2/get-display-actions ()
  '((REPL . (winconf2/display-w1))
    (HELP . (winconf2/display-w1))
    (ILIST . (purpose-display-reuse-window-buffer
              purpose-display-reuse-window-purpose
              winconf2/display-ilist))))

(defun winconf2/generate-display-actions ()
  (winconf2/remove-display-actions)
  (setq purpose-special-action-sequences
        (append (winconf2/get-display-actions)
                purpose-special-action-sequences)))


;;; display control

;; w1: side parent window, contains help window and repl window
;; +-----------+------+
;; |           |help  |
;; |  main     +------+
;; |           |repl  |
;; +-----------+------+

(defun winconf2//get-window (purpose)
  (car (purpose-windows-with-purpose purpose)))

(defun winconf2/display-w1 (buffer alist)
  (let ((buffer-purpose (purpose-buffer-purpose buffer))
        (repl-window (winconf2//get-window 'REPL))
        (help-window (winconf2//get-window 'HELP)))
    (cond
     ((or (and repl-window (eq buffer-purpose 'REPL))
          (and help-window (eq buffer-purpose 'HELP)))
      (purpose-display-reuse-window-purpose buffer alist))
     ((not (or repl-window help-window))
      (let ((purpose-display-at-bottom-height 0.3))
        (purpose-display-at-bottom buffer alist)))
     (repl-window
      (purpose-display--at #'ignore (lambda () (split-window repl-window nil 'left))
                           buffer alist))
     (help-window
      (purpose-display--at #'ignore (lambda () (split-window help-window nil 'right))
                           buffer alist))
     (t (error "Wrong flow: should never have gotten here")))))

(defun winconf2/display-ilist (buffer alist)
  (let ((main-window (window-at (frame-width) 0)))
    (purpose-display--at #'ignore
                         (lambda () (split-window main-window nil 'right))
                         buffer alist)))

(defun winconf2/maybe-autofit-width (window)
  (let ((fit-window-to-buffer-horizontally t))
    (when (eq (purpose-window-purpose window) 'ILIST)
      (fit-window-to-buffer window nil nil 40))))

(defun winconf2/maybe-dedicate-window (window)
  (when (memq (purpose-window-purpose window) '(ILIST HELP REPL))
    (purpose-set-window-purpose-dedicated-p window t)))
