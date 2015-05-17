;;; packages.el --- window-purpose Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2014 Sylvain Benner
;; Copyright (c) 2015 Bar Magal & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defvar window-purpose-packages
  '(window-purpose
    imenu-list
    let-alist)
  "List of all packages to install and/or initialize. Built-in packages
which require an initialization must be listed explicitly in the list.")

(defvar window-purpose-excluded-packages
  ;; this doesn't stop popwin package from being installed and used :-(
  '(popwin)
  "List of packages to exclude.")

(defvar spacemacs-repl-modes '(inferior-python-mode slime-repl-mode))
(defvar spacemacs-right-window-width 60)
(defvar spacemacs-bottom-window-height 12)

(defun window-purpose/init-window-purpose ()
  (use-package window-purpose
    :config
    (progn
      (defvar window-purpose-spacemacs-conf
        (purpose-conf "spacemacs"
                      :name-purposes `((,spacemacs-buffer-name . home))
                      :mode-purposes '((inferior-python-mode . repl)
                                       (slime-repl-mode . repl))))
      (dolist (mode spacemacs-repl-modes)
        (push `(,mode . repl) (oref window-purpose-spacemacs-conf :mode-purposes))
        (evil-leader/set-key-for-mode mode
          "mRR" 'spacemacs/move-repl-to-right
          "mRB" 'spacemacs/move-repl-to-bottom))
      (purpose-set-extension-configuration :spacemacs
                                           window-purpose-spacemacs-conf)

      (setq purpose-special-action-sequences
            (cl-delete 'repl purpose-special-action-sequences
                       :key 'car))
      (push '(repl purpose-display-reuse-window-buffer
                   purpose-display-reuse-window-purpose
                   spacemacs/display-repl-at-bottom)
            purpose-special-action-sequences)

      (setq purpose-default-layout-file
            (concat
             (file-name-as-directory configuration-layer-private-directory)
             (file-name-as-directory "layouts")))

      (purpose-mode)

      ;; "glue" golden-ration and window-purpose
      (purpose-x-golden-ratio-setup)
      ;; enable magit purpose-conf
      (purpose-x-magit-single-on)

      ;; bug - using `purpose-set-window-purpose' doesn't trigger all of
      ;; setup actions needed by spacemacs.
      ;; not a problem if there already is a buffer with the chosen purpose
      ;; (spacemacs setup for the buffer already happened)
      (evil-leader/set-key
        "rd" 'purpose-toggle-window-purpose-dedicated
        "rb" 'purpose-switch-buffer-with-purpose
        "rB" 'switch-buffer-without-purpose
        "rl" 'purpose-load-window-layout
        "rL" 'purpose-load-frame-layout
        "rs" 'purpose-save-window-layout
        "rS" 'purpose-save-frame-layout
        "rn" 'purpose-delete-non-dedicated-windows
        "rt" 'purpose-reset-window-layout
        "rT" 'purpose-reset-frame-layout
        "rp" 'purpose-switch-buffer-with-some-purpose
        "rP" 'purpose-set-window-purpose))))

(defun window-purpose/init-imenu-list ()
  (use-package imenu-list))
