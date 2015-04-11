;;; packages.el --- gtags Layer packages File for Spacemacs
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

(defvar gtags-packages
  '(
    ;; package gtagss go here
    helm-gtags
    ggtags
    )
  "List of all packages to install and/or initialize. Built-in packages
which require an initialization must be listed explicitly in the list.")

(defvar gtags-excluded-packages '()
  "List of packages to exclude.")

(defun gtags/init-ggtags ()
  (use-package ggtags
    :config
    ;; (progn
    ;;   (spacemacs/declare-prefix "og" "gtags")
    ;;   (evil-leader/set-key "ogg" 'ggtags-find-tag-dwim
    ;;     "ogd" 'ggtags-find-definition
    ;;     "ogr" 'ggtags-find-reference
    ;;     "ogS" 'ggtags-find-other-symbol
    ;;     "ogF" 'ggtags-find-file
    ;;     ;; need to find good bingings for these
    ;;     ;; "oge" 'ggtags-find-tag-regexp
    ;;     ;; "ogp" 'ggtags-grep
    ;;     ;; "ogl" 'ggtags-query-replace
    ;;     ))
    ))

(defun gtags/init-helm-gtags ()
  (use-package helm-gtags
    :config
    (progn
      (spacemacs/declare-prefix "og" "gtags")
      (evil-leader/set-key "ogg" 'helm-gtags-dwim
        "ogd" 'helm-gtags-find-tag      ; find definition
        "ogr" 'helm-gtags-find-rtag     ; find references
        "ogs" 'helm-gtags-select
        "ogp" 'helm-gtags-pop-stack     ; jump back
        ;; "ogS" 'helm-gtags-find-symbol
        "ogf" 'helm-gtags-tags-in-this-function
        "ogF" 'helm-gtags-find-files
        "ogU" 'helm-gtags-update-tags
        "ogP" 'helm-gtags-parse-file)))
  )
