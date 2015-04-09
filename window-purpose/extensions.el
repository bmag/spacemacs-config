;;; extensions.el --- window-purpose Layer extensions File for Spacemacs
;;
;; Copyright (c) 2012-2014 Sylvain Benner
;; Copyright (c) 2014-2015 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defvar window-purpose-load-extensions nil
  "Non-nil to load window-purpose-x (extensions for window-purspose).")

(defvar window-purpose-pre-extensions
  '(
    ;; pre extension window-purposes go here
    )
  "List of all extensions to load before the packages.")

(defvar window-purpose-post-extensions
  '(
    ;; post extension window-purposes go here
    window-purpose
    )
  "List of all extensions to load after the packages.")

;; For each extension, define a function window-purpose/init-<extension-window-purpose>

;; (defmacro my-display-and-dedicate (display-fn &rest extra-args)
;;   `(lambda (buffer alist)
;;      (let ((window (apply ,display-fn buffer alist (list,@extra-args))))
;;        (when window
;;          (purpose-set-window-purpose-dedicated-p window t))
;;        window)))

;; (defun my-dedicate-adv (&rest args)
;;   (set-window-dedicated-p nil t))

(defun window-purpose/init-window-purpose ()
  "Initialize window-purpose."
  (use-package window-purpose
    :config
    (progn
      (defun window-purpose/other-buffers (&optional buffer visible-ok)
        (let* ((buffer (or buffer (current-buffer)))
               (buffers (purpose-buffers-with-purpose (purpose-buffer-purpose buffer)))
               )
          (setq buffers (delq buffer buffers))
          (unless visible-ok
            (setq buffers (cl-delete-if 'get-buffer-window buffers)))
          buffers))

      (defun window-purpose/other-buffer (&optional buffer)
        (interactive)
        (let ((other-buff (car (window-purpose/other-buffers buffer))))
          (if other-buff
              (switch-to-buffer other-buff)
            (user-error "No other buffer"))))

      (purpose-mode 1)
      (defvar spacemacs-purpose-conf
        (purpose-conf "spacemacs"
                      :name-purposes `((,spacemacs-buffer-name . splash))))
      (purpose-set-extension-configuration :spacemacs spacemacs-purpose-conf)
      (setq purpose-special-action-sequences (cl-delete 'terminal purpose-special-action-sequences :key 'car))
      (add-to-list 'purpose-special-action-sequences
                   `(terminal purpose-display-reuse-window-buffer
                              purpose-display-reuse-window-purpose
                              ,(purpose-generate-display-and-dedicate 'purpose-display-at-bottom 8)))
      ;; (eval-after-load
      ;;  'helm
      ;;  (advice-add 'helm-default-display-buffer :after #'my-dedicate-adv))

      (when window-purpose-load-extensions
        (use-package imenu-list)
        (require 'window-purpose-x))
      )
    ))
