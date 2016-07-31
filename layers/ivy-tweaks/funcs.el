(defun ivy-tweaks/copy-actions (from to)
  "Copy Ivy actions between two commands.
Override TO's actions with FROM's actions."
  (ivy-set-actions to (plist-get ivy--actions-list from)))

(defun ivy-tweaks/ivy-insert-current ()
  (interactive)
  (with-ivy-window
    (insert ivy--current)))
