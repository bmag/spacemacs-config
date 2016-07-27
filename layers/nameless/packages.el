;;; packages.el --- nameless layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Bar <bar@matrix>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Commentary:

;;; Code:

(defconst nameless-packages '(nameless))

(defun nameless/init-nameless ()
  (use-package nameless
    :defer t
    :init
    (add-hook 'emacs-lisp-mode-hook #'nameless-mode-from-hook)
    :config
    (define-key nameless-mode-map (kbd "C-;") #'nameless-insert-name)))

;;; packages.el ends here
