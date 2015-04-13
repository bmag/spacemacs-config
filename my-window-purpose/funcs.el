;; -*- lexical-binding: t -*-

(defun purpose-persp-buffers (&optional persp purpose)
  "Get all live buffers with PURPOSE in perspective PERSP.
PERSP defaults to the current perspective.
PURPOSE defaults to the current buffer's purpose."
  (let ((persp (or persp persp-curr))
        (purpose (or purpose (purpose-buffer-purpose (current-buffer)))))
    (cl-remove-if-not (lambda (buffer)
                        (and (buffer-live-p buffer)
                             (eql purpose (purpose-buffer-purpose buffer))))
                      (persp-buffers persp))))

(defun purpose-persp-other-buffer (&optional buffer visible-ok)
  "Switch to other buffer with current purpose in current perspective.
BUFFER defaults to the current buffer.
If VISIBLE-OK is non-nil, visible buffers are considered legitimate
buffers to switch to.

BUG: doesn't always bring the recently used buffer.  I suspect that
`persp-buffers' doesn't sort the buffer by recent use."
  (interactive)
  (let ((other-buffs (purpose-persp-buffers nil
                                            (and buffer
                                                 (purpose-buffer-purpose buffer))))
        (buffer (or buffer (current-buffer))))
    (setq other-buffs (delq buffer other-buffs))
    (unless visible-ok
      (setq other-buffs
            (cl-delete-if 'get-buffer-window other-buffs)))
    (when other-buffs
      (switch-to-buffer (car other-buffs)))))
