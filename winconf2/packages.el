;;; -*- lexical-binding: t -*-

;; bugs:
;; - `open-recent-repl' doesn't open recent repl, if repl was closed with ,' (`quit-window')
;; - `open-recent-repl' should be renamed to `winconf2/open-recent-repl'

(setq winconf2-packages '(window-purpose
                          evil
                          python))
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
  (setq popwin:special-display-config
        (cl-delete "*Help*" popwin:special-display-config :key #'car :test #'equal))
  ;; (with-eval-after-load 'imenu-list
  ;;   (setq purpose-special-action-sequences
  ;;         (cl-delete #'imenu-list-purpose-display-condition
  ;;                    purpose-special-action-sequences
  ;;                    :key #'car)))

  ;; TODO: micro-state to open/close side windows
  (define-key purpose-mode-map (kbd "<f5>") #'winconf2/toggle-neotree-window)
  (define-key purpose-mode-map (kbd "<f6>") #'winconf2/toggle-help-window)
  (define-key purpose-mode-map (kbd "<f7>") #'winconf2/toggle-repl-window)
  (define-key purpose-mode-map (kbd "<f8>") #'winconf2/toggle-ilist-window)

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

(defun winconf2/post-init-python ()
  (evil-leader/set-key-for-mode 'python-mode
    "m'" #'python-start-or-switch-repl)
  (evil-leader/set-key-for-mode 'inferior-python-mode
    "m'" #'quit-window))

(defun winconf2/post-init-evil ()
  (evil-leader/set-key-for-mode 'emacs-lisp-mode
    "m'" #'ielm))

(with-eval-after-load 'ielm
  (evil-leader/set-key-for-mode 'inferior-emacs-lisp-mode
    "m'" #'quit-window))

(with-eval-after-load 'dired
  (evil-leader/set-key-for-mode 'dired-mode
    "m'" #'open-recent-repl))
