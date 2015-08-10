;;; -*- lexical-binding: t -*-

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

(defun winconf2//get-buffer (purpose)
  (car (purpose-buffers-with-purpose purpose)))

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



;;; convenience

(defun winconf2/alternate-buffer (&optional purpose)
  "Switch to last invisible buffer with PURPOSE.
In effect, when called interactively, switches back and forth between two
buffers with the same purpose."
  (interactive)
  (let* ((purpose (or purpose (purpose-buffer-purpose (current-buffer))))
         (hidden-buffer (cl-loop for buf in (purpose-buffers-with-purpose purpose)
                                 unless (get-buffer-window buf)
                                 return buf)))
    (when hidden-buffer
      (switch-to-buffer hidden-buffer))))

(defun winconf2/close-window-and-bury (&optional window)
  (interactive)
  (let ((buffer (window-buffer window)))
    (delete-window window)
    (bury-buffer buffer)))

(defun winconf2//next-buffer (next-fn boring-fn)
  "Show next non-boring buffer.
NEXT-FN should switch to the next buffer. When it switches to the originally
current buffer, it means we got to the end.
BORING-FN should return non-nil if the current buffer should not be shown."
  (let ((start-buffer (current-buffer)))
    (funcall next-fn)
    (while (and (not (eq (current-buffer) start-buffer))
                (funcall boring-fn))
      (funcall next-fn))))

(defun winconf2/next-useful-buffer ()
  (interactive)
  (let ((purpose (purpose-buffer-purpose (current-buffer))))
    (winconf2//next-buffer
     #'next-buffer
     (lambda ()
       (not (eq (purpose-buffer-purpose (current-buffer)) purpose))))))

(defun winconf2/previous-useful-buffer ()
  (interactive)
  (let ((purpose (purpose-buffer-purpose (current-buffer))))
    (winconf2//next-buffer
     #'previous-buffer
     (lambda ()
       (not (eq (purpose-buffer-purpose (current-buffer)) purpose))))))

(defun winconf2//toggle-window (purpose &optional show-fn hide-fn)
  (let ((buffer (winconf2//get-buffer purpose))
        (window (winconf2//get-window purpose))
        (show-fn (or show-fn #'display-buffer))
        (hide-fn (or hide-fn #'winconf2/close-window-and-bury)))
    (if (not buffer)
        (user-error "No buffer with purpose: %s" purpose)
      (if window
          (funcall hide-fn window)
        (funcall show-fn buffer)))))

(defun winconf2/toggle-help-window ()
  (interactive)
  (winconf2//toggle-window 'HELP))

(defun winconf2/toggle-repl-window ()
  (interactive)
  (winconf2//toggle-window 'REPL))

(defun winconf2/toggle-ilist-window ()
  (interactive)
  (winconf2//toggle-window 'ILIST
                           (lambda (_buffer)
                             (let ((imenu-list-focus-after-activation nil))
                               (imenu-list-minor-mode)))
                           (lambda (_window)
                             (imenu-list-minor-mode -1))))
(defun open-recent-repl ()
  "Select open REPL window, or pop to recently used REPL."
  (interactive)
  (let ((window (car (purpose-windows-with-purpose 'REPL))))
    (if window
        (select-window window)
      (let ((buffer (car (purpose-buffers-with-purpose 'REPL))))
        (if buffer
            (pop-to-buffer buffer)
          (user-error "No REPL buffer exists"))))))
