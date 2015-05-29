;;; extensions.el --- window-purpose Layer extensions File for Spacemacs
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

(setq window-purpose-pre-extensions '())

(setq window-purpose-post-extensions '(popwin))

(defun window-purpose/init-popwin ()
  ;; tried to force spacemacs to load slightly modified popwin, but was
  ;; not successfull :-(
  ;;
  ;; (use-package 'popwin
  ;;   :defer 5
  ;;   :config
  ;; (let ((popwin-path (concat (file-name-as-directory configuration-layer-private-directory)
  ;;                            (file-name-as-directory "window-purpose")
  ;;                            (file-name-as-directory "extensions")
  ;;                            "popwin.el")))
  ;;   (load-file popwin-path)))
  ;; (require 'popwin)
  )
