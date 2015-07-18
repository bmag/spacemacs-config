;;; -*- lexical-binding: t -*-

(setq winconf2-packages '(window-purpose))
(setq winconf2-excluded-packages '())

(setq winconf2-base-purpose-conf nil)

(defun winconf2/post-init-window-purpose ()
  (when (bound-and-true-p popwin-mode)
    (popwin-mode -1))
  (when (bound-and-true-p pupo-mode)
    (pupo-mode -1))
  (unless winconf2-base-purpose-conf
    (setq winconf2-base-purpose-conf (purpose-conf "base")))
  ;; (setq purpose-use-default-configuration nil)
  (winconf2/generate-purpose-conf)
  (purpose-set-extension-configuration :winconf winconf2-base-purpose-conf)
  (winconf2/generate-display-actions)
  (add-hook 'purpose-display-buffer-functions #'winconf2/maybe-autofit-width)
  (add-hook 'purpose-display-buffer-functions #'winconf2/maybe-dedicate-window)
  ;; (with-eval-after-load 'imenu-list
  ;;   (setq purpose-special-action-sequences
  ;;         (cl-delete #'imenu-list-purpose-display-condition
  ;;                    purpose-special-action-sequences
  ;;                    :key #'car)))

  ;; TODO: micro-state to open/close side windows

  ;; (spacemacs|define-micro-state buffers
  ;;   :use-minibuffer t
  ;;   :evil-leader "b ."
  ;;   :bindings
  ;;   ("q" nil :exit t)
  ;;   ;; change current buffer
  ;;   ("n" winconf2/next-useful-buffer)
  ;;   ("N" winconf2/previous-useful-buffer)
  ;;   ("p" winconf2/previous-useful-buffer)
  ;;   ("K" kill-this-buffer)
  ;;   ;; toggle side windows
  ;;   ("h" neotree-toggle)
  ;;   ("j" winconf2/toggle-help-window)
  ;;   ("k" winconf2/toggle-repl-window)
  ;;   ("l" winconf2/toggle-ilist-window))

  ;; (evil-leader/set-key
  ;;   "TAB" #'winconf2/alternate-buffer
  ;;   "wq" #'winconf2/close-window-and-bury)
  )

