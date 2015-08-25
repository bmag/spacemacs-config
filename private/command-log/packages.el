;;; packages.el --- command-log Layer packages File for Spacemacs
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

(setq command-log-packages '(command-log-mode))

(setq command-log-excluded-packages '())

(defun command-log/init-command-log-mode ()
  (use-package command-log-mode
    :config
    (dolist (cmd '(
                   evil-next-line evil-previous-line
                   evil-forward-char evil-backward-char
                   ))
      (add-to-list 'clm/log-command-exceptions* cmd))))
