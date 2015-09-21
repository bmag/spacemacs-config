;;; packages.el --- misc Layer packages File for Spacemacs
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

(setq misc-packages '(projectile))

(setq misc-excluded-packages '())

(defun misc/post-init-projectile ()
  (defun misc/open-projects ()
    (->> (buffer-list)
         (--filter (with-current-buffer it
                     (and (projectile-project-p)
                          (or (derived-mode-p 'dired-mode)
                              (buffer-file-name)))))
         (--map (with-current-buffer it (projectile-project-root)))
         (-uniq)))

  (defun misc/switch-project (project-path)
    (interactive
     (list (completing-read "Project: " (misc/open-projects) nil t)))
    (projectile-switch-project-by-name project-path))

  (evil-leader/set-key "op" #'misc/switch-project))
