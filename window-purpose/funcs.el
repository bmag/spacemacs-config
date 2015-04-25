(defun spacemacs/display-repl-at-right (buffer alist)
  (purpose-display-at-right buffer alist spacemacs-right-window-width))

(defun spacemacs/display-repl-at-bottom (buffer alist)
  (purpose-display-at-bottom buffer alist spacemacs-bottom-window-height))

(defun spacemacs/move-repl-to-bottom ()
  (interactive)
  (let ((buffer (window-buffer)))
    (delete-window)
    (select-window (spacemacs/display-repl-at-bottom buffer nil))))

(defun spacemacs/move-repl-to-right ()
  (interactive)
  (let ((buffer (window-buffer)))
    (delete-window)
    (select-window (spacemacs/display-repl-at-right buffer nil))))

(defun spacemacs/swap-windows (win1 win2)
  "Swap buffers and parameters of windows WIN1 and WIN2."
  (message "here4 %s %s" win1 win2)
  (let ((buf1 (window-buffer win1))
        (buf2 (window-buffer win2)))
    (set-window-buffer win1 buf2)
    (set-window-buffer win2 buf1)))

(defun spacemacs/move-repl-window-dwim ()
  (interactive)
  (let* ((repl-old-windows (purpose-windows-with-purpose 'repl))
         (repl-old-window (car repl-old-windows))
         (repl-old-position (and repl-old-window
                                 (cond ((eql (purpose-get-right-window) repl-old-window) 'right)
                                       ((eql (purpose-get-bottom-window) repl-old-window) 'bottom)))))
    (when (null repl-old-position)
      (user-error "REPL window isn't at left or at bottom"))
    (let* ((target-window (cl-case repl-old-position
                            ('right (purpose-get-bottom-window))
                            ('bottom (purpose-get-right-window))
                            (t nil))))
      (if target-window
          (spacemacs/swap-windows repl-old-window target-window)
        (cl-case repl-old-position
          ('right (spacemacs/display-repl-at-bottom (window-buffer repl-old-window) nil)
                  (delete-window repl-old-window))
          ('bottom (spacemacs/display-repl-at-right (window-buffer repl-old-window) nil)
                   (delete-window repl-old-window)))))))
