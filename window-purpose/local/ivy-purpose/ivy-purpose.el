(require 'ivy)
(require 'window-purpose)

;;;###autoload
(defun ivy-purpose-switch-buffer-with-purpose (&optional purpose)
  (interactive)
  (let* ((current-buffer (current-buffer))
         (current-purpose (or purpose
                              (purpose-buffer-purpose current-buffer))))
    (ivy-read "Buffer: "
              (mapcar #'buffer-name
                      (delq current-buffer
                            (purpose-buffers-with-purpose current-purpose)))
              :action #'ivy--switch-buffer-action
              :keymap ivy-switch-buffer-map
              :caller #'ivy-switch-buffer)))

;;;###autoload
(defun ivy-purpose-switch-buffer-with-some-purpose ()
  (interactive)
  (ivy-purpose-switch-buffer-with-purpose
   (purpose-read-purpose "Purpose: "
                         ;; don't show purposes that have no buffers
                         (cl-delete-if-not #'purpose-buffers-with-purpose
                                           (purpose-get-all-purposes))
                         t)))

;;;###autoload
(defun ivy-purpose-switch-buffer-without-purpose ()
  (interactive)
  (without-purpose (ivy-switch-buffer)))

;;;###autoload
(defun ivy-purpose-setup ()
  (setq purpose-preferred-prompt 'vanilla))

(provide 'ivy-purpose)
