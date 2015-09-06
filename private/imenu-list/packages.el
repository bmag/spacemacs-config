;;; packages.el --- imenu-list Layer packages File for Spacemacs
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

;; List of all packages to install and/or initialize. Built-in packages
;; which require an initialization must be listed explicitly in the list.
(setq imenu-list-packages '(imenu-list))

;; List of packages to exclude.
(setq imenu-list-excluded-packages '())

(defun imenu-list/init-imenu-list ()
  (use-package imenu-list
    :defer t
    :init
    (progn
      (setq imenu-list-focus-after-activation t)
      (evil-leader/set-key "oi" #'imenu-list-minor-mode))
    :config
    (progn
      (defun imenu-list/resize-window (&rest _args)
        (-when-let (window (get-buffer-window imenu-list-buffer-name))
          (let ((fit-window-to-buffer-horizontally t))
            (fit-window-to-buffer window))))
      (advice-add 'imenu-list-update :after #'imenu-list/resize-window)

      (evilify imenu-list-major-mode imenu-list-major-mode-map
               "d" #'imenu-list-display-entry
               "s" #'hs-toggle-hiding
               "q" #'imenu-list-minor-mode))))
