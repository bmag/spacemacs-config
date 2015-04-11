;;; packages.el --- my-python Layer packages File for Spacemacs
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

(defvar my-python-packages
  '(
    ;; package my-pythons go here
    pydoc
    )
  "List of all packages to install and/or initialize. Built-in packages
which require an initialization must be listed explicitly in the list.")

(defvar my-python-excluded-packages '()
  "List of packages to exclude.")

(defun my-python/init-pydoc ()
  (use-package pydoc))
