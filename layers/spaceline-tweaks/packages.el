;;; packages.el --- spaceline-tweaks layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Bar <bar@matrix>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Commentary:

;;; Code:

(defconst spaceline-tweaks-packages
  '(spaceline))

(defun spaceline-tweaks/post-init-spaceline ()
  (setq powerline-default-separator 'bar)
    (set-face-attribute 'mode-line-inactive nil :box nil)
    (spaceline-toggle-minor-modes-off)
    (with-eval-after-load 'window-purpose
      (spaceline-define-segment purpose
        "Purpose of buffer."
        ;; (purpose--modeline-string)
        (when purpose-mode (format "%s%s%s"
                                   (purpose-buffer-purpose (current-buffer))
                                   (if (window-dedicated-p) "#" "")
                                   (if (purpose-window-purpose-dedicated-p) "!" ""))))

      (let* ((main-mode-line (cdr (assq 'main spaceline--mode-lines)))
             (left-mode-line (car main-mode-line))
             (right-mode-line (cdr main-mode-line)))
        (unless (memq 'purpose left-mode-line)
          (setcar main-mode-line
                  (-insert-at (1+ (-elem-index 'major-mode left-mode-line))
                              'purpose
                              left-mode-line))))
      ;; remove purpose-mode from minor-modes list
      (diminish 'purpose-mode))
    (spaceline-compile))


;;; packages.el ends here
