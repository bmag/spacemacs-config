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

(setq my-python-packages '(anaconda-mode
                           python))

(setq my-python-excluded-packages '())

(defun my-python/post-init-anaconda-mode ()
  (evil-leader/set-key-for-mode 'python-mode
    "mhu" 'anaconda-mode-usages))

(defun my-python/post-init-python ()
  (defvar remote-ipython-buffer nil)
  (defun open-remote-ipython ()
    (interactive)
    (if (buffer-live-p remote-ipython-buffer)
        (pop-to-buffer remote-ipython-buffer)
      (prog1
          (run-python "/usr/bin/ipython console --existing" t 0)
        ;; bug: `current-buffer' isn't appropriate here
        (setq remote-ipython-buffer (current-buffer)))))
  (evil-leader/set-key "or" #'open-remote-ipython))
