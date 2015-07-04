;;; packages.el --- helm-smex Layer packages File for Spacemacs
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

(setq helm-smex-packages '(helm smex))

(setq helm-smex-excluded-packages '())

(defun helm-smex/init-helm ()
  (use-package helm-source
    :init
    (use-package smex
      :defer t
      :config
      (progn
        (defvar helm-smex-source--candidates nil)
        (defvar helm-smex-source--cache (make-hash-table :test #'eq))
        (defun helm-smex//score-no-cache (command)
          (or (cdr (car (cl-member (symbol-name command) smex-cache
                                   :test #'string=)))
              0))
        (defun helm-smex//score (command)
          (or (gethash command helm-smex-source--cache)
              (puthash command (helm-smex//score-no-cache command)
                       helm-smex-source--cache)))
        (defun helm-smex//compare-candidates (command-name1 command-name2)
          (> (helm-smex//score (intern-soft command-name1))
             (helm-smex//score (intern-soft command-name2))))
        (defclass helm-smex-source (helm-source-sync)
          ((init
            :initform (lambda ()
                        (setq helm-smex-source--candidates
                              (smex-convert-for-ido smex-cache))
                        (clrhash helm-smex-source--cache)))
           (candidates :initform 'helm-smex-source--candidates)
           (match :initform 'helm-fuzzy-match)
           (filtered-candidates-transformer
            :initform (lambda (candidates source)
                        (sort candidates #'helm-smex//compare-candidates)))
           (action
            :initform (lambda (command-name)
                        (unwind-protect
                            (execute-extended-command current-prefix-arg
                                                      command-name)
                          (smex-rank (intern command-name)))))
           ))
        (defun helm-smex/run ()
          (interactive)
          (helm :buffer "*helm-smex*"
                :sources (helm-make-source "Smex" helm-smex-source)))
        (global-set-key (kbd "M-x") #'helm-smex/run)
        (define-key helm-map (kbd "C-M-h") #'help-command)
        (define-purpose-prefix-overload helm-smex/test-prefix
          (list (lambda () (interactive) (message "No prefix"))
                (lambda () (interactive) (message "1st prefix"))
                (lambda () (interactive) (message "2nd prefix"))))
        ))))


;; For each package, define a function helm-smex/init-<package-name>
;;
;; (defun helm-smex/init-my-package ()
;;   "Initialize my package"
;;   )
;;
;; Often the body of an initialize function uses `use-package'
;; For more info on `use-package', see readme:
;; https://github.com/jwiegley/use-package
