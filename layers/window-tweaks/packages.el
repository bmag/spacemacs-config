;;; packages.el --- window-tweaks layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: %USER_FULL_NAME% <%USER_MAIL_ADDRESS%>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Commentary:

;; See the Spacemacs documentation and FAQs for instructions on how to implement
;; a new layer:
;;
;;   SPC h SPC layers RET
;;
;;
;; Briefly, each package to be installed or configured by this layer should be
;; added to `window-tweaks-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `window-tweaks/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `window-tweaks/pre-init-PACKAGE' and/or
;;   `window-tweaks/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(defconst window-tweaks-packages
  '((magit :toggle (configuration-layer/layer-usedp 'window-purpose))
    (slime :toggle (configuration-layer/layer-usedp 'window-purpose))
    window-purpose))

(defun window-tweaks/post-init-magit ()
  ;; go fullscreen only for magit status, not other buffers
  (with-eval-after-load 'window-purpose
    (purpose-x-magit-multi-on)
    (when git-magit-status-fullscreen
      (setq magit-display-buffer-function
            (lambda (buffer)
              (if (or
                   ;; the original should stay alive, so we can't go fullscreen
                   magit-display-buffer-noselect
                   ;; go fullscreen only for magit status
                   (not (with-current-buffer buffer (derived-mode-p 'magit-status-mode))))
                  ;; open buffer according to original magit rules
                  (magit-display-buffer-traditional buffer)
                ;; open buffer in fullscreen
                (delete-other-windows)
                ;; make sure the window isn't dedicated, otherwise
                ;; `set-window-buffer' throws an error
                (set-window-dedicated-p nil nil)
                (purpose-set-window-purpose-dedicated-p nil nil)
                (set-window-buffer nil buffer)
                ;; return buffer's window
                (get-buffer-window buffer)))))))

(defun window-tweaks/post-init-slime ()
  (with-eval-after-load 'window-purpose
    (purpose-add-user-purposes :modes '((slime-repl-mode . terminal)))
    (defun display-buffer-split (buffer alist)
      (let ((window (split-window nil nil (cdr (assq 'split-side alist)))))
        (window--display-buffer buffer window 'window alist)
        window))
    ;; slime doesn't handle case where fuzzy completion is displayed in the
    ;; repl's window (can happen if the other window is dedicated - bug in slime)
    (push "\\*Fuzzy Completions\\*" purpose-action-function-ignore-buffer-names)
    (push '("\\*Fuzzy Completions\\*"
            (purpose-display-reuse-window-buffer
             purpose-display-reuse-window-purpose
             purpose-display-maybe-other-window
             display-buffer-split)
            (split-side . right)
            (window-width . 60)
            (window-height . 24))
          display-buffer-alist)))

(defun window-tweaks/post-init-window-purpose ()
  ;; no `with-eval-after-load' here because `window-purpose' isn't lazy-loaded
  (push (expand-file-name "purpose-layouts/" dotspacemacs-directory)
        purpose-layout-dirs)
  (purpose-add-user-purposes :modes '((org-mode . org))))

;;; packages.el ends here
