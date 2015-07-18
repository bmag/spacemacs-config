(evil-leader/set-key
  "TAB" #'winconf2/alternate-buffer
  "wq" #'winconf2/close-window-and-bury)

(spacemacs|define-micro-state buffers
  :doc "[n]ext [p]revious [K]ill [q]uit
toggle: [h] neotree [j] help [k] repl [l] ilist"
  :use-minibuffer t
  :evil-leader "b ."
  :bindings
  ("q" nil :exit t)
  ;; change current buffer
  ("n" winconf2/next-useful-buffer)
  ("N" winconf2/previous-useful-buffer)
  ("p" winconf2/previous-useful-buffer)
  ("K" kill-this-buffer)
  ;; toggle side windows
  ("h" neotree-toggle)
  ("j" winconf2/toggle-help-window)
  ("k" winconf2/toggle-repl-window)
  ("l" winconf2/toggle-ilist-window))
