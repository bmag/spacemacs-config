;;; packages.el --- python-private Layer packages File for Spacemacs
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

(setq python-private-packages '(anaconda-mode
                                popwin
                                python))

(setq python-private-excluded-packages '())

;; (defun python-private/pre-init-anaconda-mode ()
;;   ;; anaconda-mode has/had problems detecting anaconda_mode.py's directory
;;   (setq anaconda-mode-server-directory "/usr/local/lib/python2.7/dist-packages"))

(defun python-private/post-init-anaconda-mode ()
  (evilify anaconda-mode-view-mode anaconda-mode-view-mode-map
           "j" #'forward-button
           "k" #'backward-button
           "J" #'next-error-no-select
           "K" #'previous-error-no-select
           "q" #'quit-window))

(defun python-private/post-init-popwin ()
  (push '(anaconda-mode-view-mode :dedicated t :position bottom :stick t :noselect nil)
        popwin:special-display-config))

(defun python-private/post-init-python ()
  (defvar remote-ipython-buffer nil)
  (defun open-remote-ipython ()
    (interactive)
    (if (buffer-live-p remote-ipython-buffer)
        (pop-to-buffer remote-ipython-buffer)
      (prog1
          (run-python "/usr/bin/ipython console --existing" t 0)
        ;; bug: `current-buffer' isn't appropriate here
        (setq remote-ipython-buffer (current-buffer)))))
  (add-hook 'python-mode-hook
            (lambda ()
              (setq imenu-create-index-function #'python-imenu-create-index))
            t)
  (evil-leader/set-key "or" #'open-remote-ipython))
