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

     eyebrowse
     window-purpose
     colors

     ;; private layers
     ;; python-private
     my-smartparens
     ;; multiple-cursors
     command-log
     imenu-list
     beacon
     ;; misc
     ;; winconf
     ;; winconf2
     winconf3
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
   dotspacemacs-editing-style 'hybrid
   dotspacemacs-verbose-loading nil
   dotspacemacs-startup-banner 'official
   dotspacemacs-startup-lists '(recents projects)
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
   dotspacemacs-max-rollback-slots 10
   dotspacemacs-use-ido nil
   dotspacemacs-helm-resize nil
   dotspacemacs-helm-no-header nil
   dotspacemacs-helm-position 'bottom
   dotspacemacs-enable-paste-micro-state t
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
   dotspacemacs-smartparens-strict-mode nil
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
  (if (fboundp 'advice-add)
      (advice-add 'spacemacs/post-theme-init :after 'my-post-theme-init)
    (defadvice spacemacs/post-theme-init (after my-post-theme-init-adv activate)
      "Call `my-post-theme-init'."
      (my-post-theme-init theme)))
  )

(defun dotspacemacs/user-config ()
  "Configuration function for user code.
This function is called at the very end of Spacemacs initialization after
layers configuration. You are free to put any user code."
  (my-post-theme-init (car dotspacemacs-themes))
  (setq evil-escape-delay 0.2)

  (add-hook 'prog-mode-hook 'evil-mc-mode)
  (add-hook 'text-mode-hook 'evil-mc-mode)
  (with-eval-after-load 'evil-mc
    (diminish 'evil-mc-mode))

  ;; powerline changes
  (setq powerline-default-separator 'slant)
  (spaceline-define-segment purpose
    (substring (purpose--modeline-string) 2 -1)
    :when purpose-mode)
  (unless (memq 'purpose spaceline-left)
    (setq spaceline-left
          (-insert-at (1+ (-elem-index 'major-mode spaceline-left))
                      'purpose
                      spaceline-left)))
  (diminish 'purpose-mode)
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
  (with-eval-after-load 'window-purpose
    (cl-pushnew '(conf-mode . edit) purpose-user-mode-purposes :key #'car)
    (cl-pushnew '(cider-repl-mode . terminal) purpose-user-mode-purposes :key #'car)
    (cl-pushnew '("\\.log$" . log) purpose-user-regexp-purposes
                :key #'car :test #'equal)
    (cl-pushnew '(web-mode-prog-mode . edit) purpose-user-mode-purposes :key #'car)
    (cl-pushnew '(org-mode . org) purpose-user-mode-purposes :key #'car)
    (cl-pushnew '(".travis.yml" . edit) purpose-user-name-purposes
                :key #'car :test #'equal)
    ;; (purpose-compile-user-configuration)

    ;; (when (require 'window-purpose-x nil t)
    ;;   (purpose-x-magit-single-on))

    ;; because of winconf2
    ;; (popwin-mode -1)
    ;; (pupo-mode -1)
    ;; (setq popwin:special-display-config
    ;;       (cl-delete "*Help*" popwin:special-display-config
    ;;                  :key #'car :test #'equal))
    ;; (pupo/update-purpose-config)
    )

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

(defun my-post-theme-init (theme)
  "Personal additions to themes."
  (cond
   ((eq theme 'tangotango)
    (custom-theme-set-faces
     'tangotango
     '(diff-hl-delete ((t (:foreground "red3" :background "#553333"))))
     '(diff-hl-insert ((t (:foreground "green4" :background "#335533"))))
     '(diff-hl-change ((t (:foreground "blue3" :background "#333355"))))
     '(spacemacs-mode-line-flycheck-info-face ((t (:foreground "dodger blue" :box (:line-width 1 :style released-button)))))
     '(spacemacs-mode-line-flycheck-warning-face ((t (:foreground "#edd400" :box (:line-width 1 :style released-button)))))
     '(spacemacs-mode-line-flycheck-error-face ((t (:foreground "tomato" :box (:line-width 1 :style released-button)))))
     '(rainbow-delimiters-depth-1-face ((t (:foreground "#729fcf")))) ; hello
     '(rainbow-delimiters-depth-2-face ((t (:foreground "sandy brown"))))
     '(rainbow-delimiters-depth-3-face ((t (:foreground "green yellow"))))
     '(rainbow-delimiters-depth-4-face ((t (:foreground "hot pink"))))
     '(rainbow-delimiters-depth-5-face ((t (:foreground "LightGoldenrod1"))))
     '(rainbow-delimiters-depth-6-face ((t (:foreground "light sky blue"))))
     '(rainbow-delimiters-depth-7-face ((t (:foreground "light green"))))
     '(rainbow-delimiters-depth-8-face ((t (:foreground "goldenrod"))))
     '(rainbow-delimiters-depth-9-face ((t (:foreground "orchid"))))
     '(company-tooltip ((t (:foreground "gainsboro" :background "dim gray"))))
     '(company-tooltip-annotation ((t (:foreground "sky blue" :inherit (company-tooltip)))))
     '(company-tooltip-selection ((t (:background "steel blue" :inherit (company-tooltip)))))
     '(company-scrollbar-fg ((t (:background "black" :inherit (company-tooltip)))))
     '(company-scrollbar-bg ((t (:background "dark gray" :inherit (company-tooltip)))))
     '(helm-selection ((t (:background "dim gray" :distant-foreground "white"))))
     '(helm-source-header ((t (:foreground "white" :box (:line-width 2 :color "grey75" :style released-button) :weight bold :height 1.1 :family "Sans Serif"))))
     '(helm-buffer-process ((t (:foreground "light salmon"))))))
   ((eq theme 'spacemacs-dark)
    (let ((active1 "#222226")
          (active2 "#5d4d7a")
          (base "#b2b2b2")
          (comment "#2aa198")
          (comment-bg "#293234")
          (cursor "#e3dedd")
          (bg1 "#292b2e")
          (bg2 "#212026")
          (bg3 "#100a14")
          (bg4 "#0a0814")
          (war "#dc752f")
          (inf "#2f96dc")
          (green "#67b11d"))
      (custom-theme-set-faces
       'spacemacs-dark
       `(diff-hl-change ((t (:foreground ,inf :background "#001836"))))
       `(diff-hl-delete ((t (:foreground ,war :background "#361800"))))
       `(diff-hl-insert ((t (:foreground ,green :background "#003618"))))
       `(helm-visible-mark ((t (:foreground ,green :background ,bg4))))
       ;; `(imenu-list-entry-face-0 ((t (:foreground "#bc6ec5"))))
       ;; `(imenu-list-entry-face-1 ((t (:foreground "#2f96dc"))))
       ;; `(imenu-list-entry-face-2 ((t (:foreground "#67b11d"))))

       ;; `popup-tip-face' used by flycheck tips
       ;; `(popup-tip-face ((t (:foreground ,cursor :background ,active2 :bold nil))))
       ;; `(popup-tip-face ((t (:foreground "black" :background ,comment :bold nil :box t))))
       ;; `tooltip' used by company-quickhelp tips
       ;; `(tooltip ((t (:foreground ,active1 :background "#ad9dca"))))
       ;; `(tooltip ((t (:foreground "#efefef" :background "#4d3d6a"))))
       )))))
