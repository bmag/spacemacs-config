;;; packages.el --- ivy-tweaks layer packages file for Spacemacs.
;;; Commentary:
;;; Code:

(defconst ivy-tweaks-packages
  '(ivy)
  )

(defun ivy-tweaks/post-init-ivy ()
  (with-eval-after-load 'ivy
    (with-eval-after-load 'projectile
      (ivy-tweaks/copy-actions 'ivy-recentf 'projectile-recentf))
    (define-key ivy-minibuffer-map (kbd "C-c <C-i>") #'ivy-tweaks/ivy-insert-current)))


;;; packages.el ends here
