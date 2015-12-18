;; -*- mode: emacs-lisp -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

(defun dotspacemacs/layers ()
  "Configuration Layers declaration.
You should not put any user code in this function besides modifying the variable
values."
  (setq-default
   dotspacemacs-distribution 'spacemacs
   dotspacemacs-configuration-layer-path '()
   dotspacemacs-configuration-layers
   `(
     (auto-completion :variables
                      auto-completion-enable-help-tooltip nil
                      auto-completion-enable-sort-by-usage nil
                      auto-completion-enable-snippets-in-popup t)
     ;; better-defaults
     (git :variables git-magit-status-fullscreen t)
     version-control
     markdown
     org
     (syntax-checking :variables
                      flycheck-check-syntax-automatically '(save mode-enabled))
     evil-cleverparens

     ;; additional contrib layers
     c-c++
     (clojure :variables clojure-enable-fancify-symbols t)
     emacs-lisp
     ;; html
     ;; javascript
     python
     ;; php
     ;; sql

     (shell :variables shell-default-shell 'shell)
     ;; gtags
     ;; cscope
     search-engine
     ;; evil-snipe
     ibuffer

     smex
     themes-megapack
     theming
     command-log

     eyebrowse
     window-purpose
     colors

     ;; private layers
     ;; python-private
     ;; my-smartparens
     ;; multiple-cursors
     imenu-list
     beacon
     ;; misc
     ;; winconf
     ;; winconf2
     ;; winconf3
     )
   dotspacemacs-additional-packages
   '(nlinum let-alist irfc f jinja2-mode helm-company help-fns+)
   dotspacemacs-excluded-packages '(drupal-mode)
   dotspacemacs-delete-orphan-packages nil))

(defun dotspacemacs/init ()
  "Initialization function.
This function is called at the very startup of Spacemacs initialization
before layers configuration.
You should not put any user code in there besides modifying the variable
values."
  (setq-default
   dotspacemacs-elpa-https t
   dotspacemacs-elpa-timeout 5
   dotspacemacs-editing-style 'hybrid
   dotspacemacs-verbose-loading nil
   dotspacemacs-startup-banner 'official
   dotspacemacs-startup-lists '(recents projects)
   dotspacemacs-startup-recent-list-size 10
   dotspacemacs-themes '(spacemacs-dark
                         spacemacs-light
                         monokai)
   dotspacemacs-colorize-cursor-according-to-state t
   dotspacemacs-default-font '("Sauce Code Powerline"
                               :size 12
                               :weight normal
                               :width normal
                               :powerline-scale 1.1)
   dotspacemacs-leader-key "SPC"
   dotspacemacs-emacs-leader-key "M-m"
   dotspacemacs-major-mode-leader-key ","
   dotspacemacs-major-mode-emacs-leader-key "C-M-m"
   dotspacemacs-command-key ":"
   dotspacemacs-remap-Y-to-y$ t
   dotspacemacs-default-layout-name "Default"
   dotspacemacs-display-default-layout nil
   dotspacemacs-auto-resume-layouts nil
   dotspacemacs-auto-save-file-location 'cache
   dotspacemacs-max-rollback-slots 7
   dotspacemacs-use-ido nil
   dotspacemacs-helm-resize nil
   dotspacemacs-helm-no-header nil
   dotspacemacs-helm-position 'bottom
   dotspacemacs-enable-paste-micro-state nil
   dotspacemacs-which-key-delay 0.4
   dotspacemacs-which-key-position 'right-then-bottom
   dotspacemacs-loading-progress-bar t
   dotspacemacs-fullscreen-at-startup nil
   dotspacemacs-fullscreen-use-non-native nil
   dotspacemacs-maximized-at-startup t
   dotspacemacs-active-transparency 90
   dotspacemacs-inactive-transparency 90
   dotspacemacs-mode-line-unicode-symbols t
   dotspacemacs-smooth-scrolling t
   dotspacemacs-line-numbers nil
   dotspacemacs-smartparens-strict-mode t
   dotspacemacs-highlight-delimiters 'all
   dotspacemacs-persistent-server nil
   dotspacemacs-search-tools '("ag" "pt" "ack" "grep")
   dotspacemacs-default-package-repository nil
   dotspacemacs-whitespace-cleanup 'changed))

(defun dotspacemacs/user-init ()
  "Initialization function for user code.
It is called immediately after `dotspacemacs/init'.  You are free to put any
user code."
  (setq save-interprogram-paste-before-kill t)
  (let ((active1 "#222226") (active2 "#5d4d7a")
        (base "#b2b2b2") (cursor "#e3dedd")
        (comment "#2aa198") (comment-bg "#293234")
        (bg1 "#292b2e") (bg2 "#212026")
        (bg3 "#100a14") (bg4 "#0a0814")
        (war "#dc752f") (inf "#2f96dc") (green "#67b11d"))
    (setq theming-modifications
          `((spacemacs-dark
             (diff-hl-change :foreground ,inf :background "#001836")
             (diff-hl-delete :foreground ,war :background "#361800")
             (diff-hl-insert :foreground ,green :background "#003618")
             ;; (helm-visible-mark :foreground ,green :background ,bg4)
             ;; `(imenu-list-entry-face-0 ((t (:foreground "#bc6ec5"))))
             ;; `(imenu-list-entry-face-1 ((t (:foreground "#2f96dc"))))
             ;; `(imenu-list-entry-face-2 ((t (:foreground "#67b11d"))))

             ;; `popup-tip-face' used by flycheck tips
             ;; `(popup-tip-face :foreground ,cursor :background ,active2 :bold nil)
             ;; `(popup-tip-face :foreground "black" :background ,comment :bold nil :box t)
             ;; `tooltip' used by company-quickhelp tips
             ;; `(tooltip :foreground ,active1 :background "#ad9dca")
             ;; `(tooltip :foreground "#efefef" :background "#4d3d6a")
             )))))

(defun dotspacemacs/user-config ()
  "Configuration function for user code.
This function is called at the very end of Spacemacs initialization after
layers configuration. You are free to put any user code."
  (setq evil-escape-delay 0.2)

  (add-hook 'smartparens-enabled-hook #'evil-cleverparens-mode)

  (add-hook 'prog-mode-hook 'evil-mc-mode)
  (add-hook 'text-mode-hook 'evil-mc-mode)
  (with-eval-after-load 'evil-mc
    (diminish 'evil-mc-mode))

  ;; powerline changes
  (setq powerline-default-separator 'alternate)
  ;; (spaceline-define-segment purpose
  ;;   (substring (purpose--modeline-string) 2 -1)
  ;;   :when purpose-mode)
  ;; (unless (memq 'purpose spaceline-left)
  ;;   (setq spaceline-left
  ;;         (-insert-at (1+ (-elem-index 'major-mode spaceline-left))
  ;;                     'purpose
  ;;                     spaceline-left)))
  ;; (diminish 'purpose-mode)
  (setq spaceline-version-control-p nil)
  ;; (setq spaceline-minor-modes-p nil)

  ;; fix ffap pinging when trying to complete such as "a.is"
  (setq ffap-machine-p-known 'reject)

  ;; auto-completion
  (setq company-idle-delay 0.01)
  (setq company-minimum-prefix-length 1)
  (setq company-tooltip-align-annotations t)

  ;; comint
  (with-eval-after-load 'comint
    (dolist (state '(normal insert emacs))
      (evil-define-key state comint-mode-map
        (kbd "M-p") #'comint-previous-matching-input-from-input
        (kbd "M-n") #'comint-next-matching-input-from-input
        (kbd "C-c M-r") #'comint-previous-input
        (kbd "C-c M-s") #'comint-next-input)))
  (evil-leader/set-key
    "oh" #'helm-comint-input-ring)

  ;; smartparens (highlighting)
  (setq sp-highlight-pair-overlay nil)
  (show-smartparens-global-mode -1)

  (setq frame-title-format "Spacemacs")

  ;; dired
  (setq dired-guess-shell-alist-user
        '(("\\.pdf\\'" "evince")
          ("\\.ods\\'\\|\\.xlsx?\\'\\|\\.docx?\\'\\|\\.csv\\'" "libreoffice")
          ("\\.jpg\\'" "gpicview")))

  ;; nyan-mode
  (setq nyan-bar-length 16)

  ;; (cl-pushnew '("Cask$" . emacs-lisp-mode) auto-mode-alist
  ;;             :key #'car :test #'equal)

  ;; window-purpose
  ;; (with-eval-after-load 'window-purpose
  ;;   (cl-pushnew '(conf-mode . edit) purpose-user-mode-purposes :key #'car)
  ;;   (cl-pushnew '(cider-repl-mode . terminal) purpose-user-mode-purposes :key #'car)
  ;;   (cl-pushnew '("\\.log$" . log) purpose-user-regexp-purposes
  ;;               :key #'car :test #'equal)
  ;;   (cl-pushnew '(web-mode-prog-mode . edit) purpose-user-mode-purposes :key #'car)
  ;;   (cl-pushnew '(org-mode . org) purpose-user-mode-purposes :key #'car)
  ;;   (cl-pushnew '(".travis.yml" . edit) purpose-user-name-purposes
  ;;               :key #'car :test #'equal)
  ;;   ;; (purpose-compile-user-configuration)

  ;;   ;; (when (require 'window-purpose-x nil t)
  ;;   ;;   (purpose-x-magit-single-on))

  ;;   ;; because of winconf2
  ;;   ;; (popwin-mode -1)
  ;;   ;; (pupo-mode -1)
  ;;   ;; (setq popwin:special-display-config
  ;;   ;;       (cl-delete "*Help*" popwin:special-display-config
  ;;   ;;                  :key #'car :test #'equal))
  ;;   ;; (pupo/update-purpose-config)
  ;;   )

  ;; nlinum
  (spacemacs|add-toggle line-numbers
    :status nlinum-mode
    :on (global-nlinum-mode)
    :off (global-nlinum-mode -1)
    :documentation "Show the line numbers."
    :evil-leader "tn")

  ;; irfc
  (with-eval-after-load 'irfc-mode
    (evilified-state-evilify irfc-mode irfc-mode-map
             "g" #'irfc-table-jump
             "G" #'irfc-page-goto))

  ;; customize
  (with-eval-after-load 'cus-edit
    (evilified-state-evilify Custom-mode custom-mode-map))

  ;; (with-eval-after-load 'helm
  ;;   (define-key helm-map (kbd "C-M-h") #'help-command))
  (setq helm-locate-fuzzy-match nil)

  )
