;;; packages.el --- window-purpose Layer packages File for Spacemacs
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

(setq window-purpose-packages '(window-purpose
                                imenu-list
                                let-alist
                                (purpose-popwin :location local)))

(setq window-purpose-excluded-packages '())

(defun window-purpose/init-window-purpose ()
  (use-package window-purpose
    :init
    ;; overriding `purpose-mode-map' with empty keymap, so it doesn't conflict
    ;; with original `C-x C-f', `C-x b', etc. and `semantic' key bindings. must
    ;; be done before `window-purpose' is loaded
    (setq purpose-mode-map (make-sparse-keymap))
    :config
    (evil-leader/set-key
      "rb" 'window-purpose/switch-buffer-with-purpose ; 'purpose-switch-buffer-with-purpose
      "rd" 'purpose-toggle-window-purpose-dedicated
      "rB" 'spacemacs/helm-mini-ignore-purpose
      "rD" 'purpose-delete-non-dedicated-windows
      "rp" 'window-purpose/switch-buffer-with-some-purpose ; 'purpose-switch-buffer-with-some-purpose
      "rP" 'purpose-set-window-purpose)
    (setq purpose-preferred-prompt 'helm)
    (defalias 'spacemacs/helm-mini-ignore-purpose
      (without-purpose-command #'helm-mini)
      "Same as `helm-mini', but disable window-purpose while this command executes.")

    (defvar window-purpose--dedicated-windows nil)

    (defadvice popwin:create-popup-window (before window-purpose/save-dedicated-windows)
      (setq window-purpose--dedicated-windows
            (cl-loop for window in (window-list)
                     if (purpose-window-purpose-dedicated-p window)
                     collect (window-buffer window))))

    (defadvice popwin:create-popup-window (after window-purpose/restore-dedicated-windows)
      (cl-loop for buffer in window-purpose--dedicated-windows
               do (cl-loop for window in (get-buffer-window-list buffer)
                           do (purpose-set-window-purpose-dedicated-p window t))))

    (defun window-purpose/sync-advices ()
      (if purpose-mode
          (progn
            (ad-enable-advice 'popwin:create-popup-window
                              'before 'window-purpose/save-dedicated-windows)
            (ad-enable-advice 'popwin:create-popup-window
                              'after 'window-purpose/restore-dedicated-windows)
            (ad-update 'popwin:create-popup-window)
            (ad-activate 'popwin:create-popup-window))
        (ad-disable-advice 'popwin:create-popup-window
                           'before 'window-purpose/save-dedicated-windows)
        (ad-disable-advice 'popwin:create-popup-window
                           'after 'window-purpose/restore-dedicated-windows)
        (ad-update 'popwin:create-popup-window)))
    (add-hook 'purpose-mode-hook #'window-purpose/sync-advices)

    (eval-after-load 'helm-buffers
      '(progn
         (defvar window-purpose--current-purpose 'edit)

         (defclass helm-source-purpose-buffers (helm-source-buffers)
           ((buffer-list :initform
                         (lambda ()
                           (mapcar #'buffer-name
                                   (delq (current-buffer)
                                         (purpose-buffers-with-purpose window-purpose--current-purpose)))))))

         (defvar helm-source-purpose-buffers-list
           (helm-make-source "Purpose buffers" 'helm-source-purpose-buffers))

         (defun window-purpose/switch-buffer-with-purpose (&optional purpose)
           "Switch to buffer, choose from buffers with purpose PURPOSE.
PURPOSE defaults to the purpose of the current buffer."
           (interactive)
           (setq window-purpose--current-purpose
                 (or purpose (purpose-buffer-purpose (current-buffer))))
           (helm :sources 'helm-source-purpose-buffers-list
                 :buffer "*helm purpose*"
                 :prompt "Buffer: "))

         (defun window-purpose/switch-buffer-with-some-purpose ()
           "Choose a purpose, then switch to a buffer with that purpose."
           (interactive)
           (window-purpose/switch-buffer-with-purpose
            (purpose-read-purpose "Purpose: "
                                  ;; don't show purposes that have no buffers
                                  (cl-delete-if-not #'purpose-buffers-with-purpose
                                                    (purpose-get-all-purposes))
                                  t)))))

    (purpose-mode)
    (purpose-x-golden-ratio-setup)
    (purpose-x-kill-setup)))

(defun window-purpose/post-init-window-purpose ()
  ;; *LV* buffer is used by corelv.el
  (push "^\\*LV\\*$" purpose-action-function-ignore-buffer-names)

  (eval-after-load 'eyebrowse
    '(progn
       (defvar window-purpose--eyebrowse-new-workspace eyebrowse-new-workspace)

       (defun window-purpose/new-workspace ()
         "Create a new eyebrowse workspace."
         ;; partially copied from `eyebrowse-switch-to-window-config'
         (cond
          ((stringp window-purpose--eyebrowse-new-workspace)
           (switch-to-buffer (get-buffer-create window-purpose--eyebrowse-new-workspace)))
          ((functionp window-purpose--eyebrowse-new-workspace)
           (funcall window-purpose--eyebrowse-new-workspace))
          (t (switch-to-buffer "*scratch*")))

         ;; in case opening the new buffer splitted the frame (e.g.
         ;; `eyebrowse-switch-to-window-config' was called from a purpose-dedicated
         ;; buffer)
         (delete-other-windows))

       (defun window-purpose/sync-eyebrowse ()
         (if purpose-mode
             (setq eyebrowse-new-workspace #'window-purpose/new-workspace)
           (setq eyebrowse-new-workspace window-purpose--eyebrowse-new-workspace)))
       (add-hook 'purpose-mode-hook #'window-purpose/sync-eyebrowse)
       (window-purpose/sync-eyebrowse))))

(defun window-purpose/init-purpose-popwin ()
  (with-eval-after-load 'window-purpose
    (use-package purpose-popwin
      :config
      (pupo-mode)
      (evil-leader/set-key
        "wpp" #'pupo/close-window
        "wpP" #'pupo/close-all-windows))))
