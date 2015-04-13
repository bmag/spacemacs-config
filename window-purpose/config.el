;; overriding stock popwin with this layer's popwin
;; doing this here, because doing it in packages.el and extensions.el
;; doesn't work :-(
(let ((popwin-path (concat (file-name-as-directory configuration-layer-private-directory)
                           (file-name-as-directory "window-purpose")
                           (file-name-as-directory "extensions")
                           "popwin.el")))
  (load-file popwin-path))
