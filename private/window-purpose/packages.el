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

(setq window-purpose-packages '(eyebrowse
                                helm
                                popwin
                                (purpose-popwin :location local)
                                window-purpose))

(setq window-purpose-excluded-packages '())

(defun window-purpose/post-init-eyebrowse ()
  (defvar window-purpose--eyebrowse-new-workspace eyebrowse-new-workspace
    "Internal backup of `eyebrowse-new-workspace'.")

  ;; replacement for `eyebrowse-new-workspace' that handles purpose-dedicated
  ;; windows correctly
  (defun window-purpose/new-workspace ()
    "Create a new eyebrowse workspace."
    ;; call original `eyebrowse-new-workspace' (partially copied from
    ;; `eyebrowse-switch-to-window-config')
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
    "Synchronize window-purpose layer with eyebrowse.
Set `eyebrowse-new-workspace' value depending on the state of `purpose-mode'."
    (if purpose-mode
        (setq eyebrowse-new-workspace #'window-purpose/new-workspace)
      (setq eyebrowse-new-workspace window-purpose--eyebrowse-new-workspace)))
  (add-hook 'purpose-mode-hook #'window-purpose/sync-eyebrowse)
  (when (boundp 'purpose-mode)
    ;; sync with eyebrowse now if window-purpose was already loaded
    (window-purpose/sync-eyebrowse)))

(defun window-purpose/pre-init-helm ()
  ;; create helm sources for purpose commands, for a behavior consistent with
  ;; `helm-mini' and other buffer-related helm commands
  (spacemacs|use-package-add-hook helm
    :post-init
    (progn
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
                               t))))
    :post-config
    (progn
      (require 'helm-buffers)
      (defvar window-purpose--current-purpose 'edit)
      (defvar helm-source-purpose-buffers-list
        (helm-make-source "Purpose buffers" 'helm-source-buffers
          :buffer-list
          (lambda ()
            ;; return names of buffers with the same purpose as current buffer,
            ;; excluding current buffer
            (mapcar #'buffer-name
                    (delq (current-buffer)
                          (purpose-buffers-with-purpose
                           window-purpose--current-purpose)))))))))

(defun window-purpose/post-init-popwin ()
  ;; when popwin creates a popup window, it removes the `purpose-dedicated'
  ;; window parameter from all windows, so we must save and restore it
  ;; ourselves. this works well as long as no buffer is displayed in more than
  ;; one window. if a buffer is displayed in several windows, and at least one
  ;; of these windows is purpose-dedicated, then all these windows will become
  ;; purpose-dedicated after popwin creates a popup window.
  ;; there is no problem if the local purpose-popwin extension is used, as long
  ;; as the user doesn't call `popwin:create-popup-window' directly (e.g. <f2>
  ;; from `helm-mini')

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

  (defun window-purpose/sync-popwin ()
    "Synchronize window-purpose layer with popwin.
Enable or disable advices to popwin, according to the state of `purpose-mode'."
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
  (add-hook 'purpose-mode-hook #'window-purpose/sync-popwin)
  (when (boundp 'purpose-mode)
    ;; sync with popwin now if window-purpose was already loaded
    (window-purpose/sync-popwin)))

(defun window-purpose/init-purpose-popwin ()
  ;; purpose-popwin needs to be configured after popwin and window-purpose.
  ;; popwin is guaranteed to run after purpose-popwin due to alphabetic order,
  ;; but for window-purpose we need `with-eval-after-load'
  (with-eval-after-load 'window-purpose
    (use-package purpose-popwin
      :config
      (progn
        (pupo-mode)
        ;; override popwin commands with pupo commands
        (evil-leader/set-key
          "wpp" #'pupo/close-window
          "wpP" #'pupo/close-all-windows)))))

(defun window-purpose/init-window-purpose ()
  (use-package window-purpose
    :init
    ;; overriding `purpose-mode-map' with empty keymap, so it doesn't conflict
    ;; with original `C-x C-f', `C-x b', etc. and `semantic' key bindings. must
    ;; be done before `window-purpose' is loaded
    (setq purpose-mode-map (make-sparse-keymap))
    :config
    (progn
      ;; 'r' is for "puRpose" ('w', 'p' are crowded, 'W', 'P' aren't comfortable)
      (evil-leader/set-key
        "rb" 'window-purpose/switch-buffer-with-purpose
        "rB" 'window-purpose/helm-mini-ignore-purpose
        "rd" 'purpose-toggle-window-purpose-dedicated
        "rD" 'purpose-delete-non-dedicated-windows
        "rp" 'window-purpose/switch-buffer-with-some-purpose
        "rP" 'purpose-set-window-purpose)
      ;; not respecting the value of `dotspacemacs-use-ido', because nearly
      ;; nothing in spacemacs seems to respect it anyway
      (setq purpose-preferred-prompt 'helm)
      (defalias 'window-purpose/helm-mini-ignore-purpose
        (without-purpose-command #'helm-mini)
        "Same as `helm-mini', but disable window-purpose while this command executes.")

      ;; *LV* buffer is used by corelv.el
      (add-to-list 'purpose-action-function-ignore-buffer-names "^\\*LV\\*$")
      (purpose-mode)
      (purpose-x-golden-ratio-setup)
      ;; when killing a purpose-dedicated buffer that is displayed in a window,
      ;; ensure that the buffer is replaced by a buffer with the same purpose
      ;; (or the window deleted, if no such buffer)
      (purpose-x-kill-setup))))
