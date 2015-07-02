;;; extensions.el --- window-purpose Layer extensions File for Spacemacs
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

(setq window-purpose-pre-extensions '())

(setq window-purpose-post-extensions '(purpose-popwin))

(defun window-purpose/init-purpose-popwin ()
  (byte-recompile-directory
   (configuration-layer/get-layer-property 'window-purpose :ext-dir)
   0)
  (use-package purpose-popwin
    :config
    (pupo-mode)
    (evil-leader/set-key
      "wpp" #'pupo/close-window
      "wpP" #'pupo/close-all-windows)))
