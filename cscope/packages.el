;;; packages.el --- cscope Layer packages File for Spacemacs
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

(defvar cscope-packages
  '(
    ;; package cscopes go here
    evil-jumper
    helm-cscope
    xcscope
    )
  "List of all packages to install and/or initialize. Built-in packages
which require an initialization must be listed explicitly in the list.")

(defvar cscope-excluded-packages '()
  "List of packages to exclude.")

(defun cscope/init-evil-jump ()
  (defadvice helm-cscope-find-this-symbol (before cscope/goto activate)
    (evil-jumper--push)))

(defun cscope/init-helm-cscope ()
  (use-package helm-cscope
    :config
    (use-package xcscope
      :config
      (progn
        ;; for python projects, we don't want xcscope to rebuild the databse,
        ;; because it uses cscope instead of pycscope
        (setq-default cscope-option-do-not-update-database t)
        (setq-default cscope-display-cscope-buffer nil)

        (defun cscope/safe-project-root ()
          "Return project's root, or nil if not in a project."
          (and (fboundp 'projectile-project-root)
               (projectile-project-p)
               (projectile-project-root)))

        (defun cscope/run-pycscope-1 (source-directory output-file)
          "Run pycscope in directory to create reference file.
SOURCE-DIRECTORY is the directory to index.  OUTPUT-FILE is the name to
use for the refernece file."
          (let ((default-directory source-directory))
            (shell-command (message "pycscope -R -f '%s'" output-file))))

        (defun cscope/run-pycscope (directory)
          (interactive
           (list (file-name-as-directory
                  (read-directory-name "Run pycscope in directory: "
                                       (cscope/safe-project-root)))))
          (cscope/run-pycscope-1 directory
                                 (concat directory "cscope.out")))

        (evil-leader/set-key-for-mode 'python-mode
          "osS" 'cscope/run-pycscope
          "oss" 'helm-cscope-find-this-symbol
          "os=" 'helm-cscope-find-assignments-to-this-symbol
          "osd" 'helm-cscope-find-global-definition
          "osD" 'cscope-find-global-definition-no-prompting
          "osc" 'helm-cscope-find-calling-this-funtcion
          "osC" 'helm-cscope-find-called-function
          "ost" 'helm-cscope-find-this-text-string
          "ose" 'helm-cscope-find-egrep-pattern
          "osf" 'helm-cscope-find-this-file
          "osi" 'helm-cscope-find-files-including-file ; finds imports?
          "osp" 'helm-cscope-pop-mark   ; alias for cscope-pop-mark
          )))))
