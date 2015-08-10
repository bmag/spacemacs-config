;;; packages.el --- avy Layer packages File for Spacemacs
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

(setq avy-packages '(avy))

(setq avy-excluded-packages '())

(defun avy/init-avy ()
  (use-package avy
    :defer t
    :init
    (progn
      (evil-leader/set-key
        "SPC" #'avy-goto-word-or-subword-1
        "l" #'avy-goto-line
        ";" #'avy-goto-char))
    :config
    (progn
      (setq avy-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
      )))

