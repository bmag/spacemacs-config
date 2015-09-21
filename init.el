;; -*- mode: emacs-lisp -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

(defun dotspacemacs/layers ()
  "Configuration Layers declaration."
  (setq-default
   dotspacemacs-configuration-layer-path '()
   dotspacemacs-configuration-layers
   `(
     (auto-completion :variables
                      auto-completion-enable-help-tooltip t
                      auto-completion-enable-sort-by-usage t)
     ;; better-defaults
     git
     version-control
     markdown
     org
     syntax-checking
     ;; ycmd
     evil-commentary                    ; replaces evilnc (nerd commentor)

     ;; additional contrib layers
     c-c++
     clojure
     emacs-lisp
     html
     javascript
     python
     php
     ;; sql

     (shell :variables shell-default-shell 'shell)
     gtags
     cscope
     search-engine
     unimpaired
     ;; evil-snipe

     smex
     themes-megapack

     perspectives
     eyebrowse
     window-purpose
     (colors :variables colors-enable-nyan-cat-progress-bar ,(display-graphic-p))

     ;; private layers
     python-private
     command-log
     imenu-list
     misc
     ;; helm-smex
     ;; winconf
     ;; winconf2
     )
   dotspacemacs-additional-packages
   '(nlinum let-alist irfc f tabbar tabbar-ruler jinja2-mode)
   dotspacemacs-excluded-packages '(drupal-mode)
   dotspacemacs-delete-orphan-packages t))

(defun dotspacemacs/init ()
  "Initialization function.
This function is called at the very startup of Spacemacs initialization
before layers configuration."
  (setq-default
   dotspacemacs-editing-style 'hybrid
   dotspacemacs-verbose-loading nil
   dotspacemacs-startup-banner 'official
   dotspacemacs-always-show-changelog t
   dotspacemacs-startup-lists '(recents projects)
   dotspacemacs-startup-recent-list-size 5
   dotspacemacs-themes '(spacemacs-dark
                         spacemacs-light
                         monokai)
   dotspacemacs-colorize-cursor-according-to-state t
   dotspacemacs-default-font '("Sauce Code Powerline"
                               :size 13
                               :weight normal
                               :width normal
                               :powerline-scale 1.1)
   dotspacemacs-leader-key "SPC"
   dotspacemacs-emacs-leader-key "M-m"
   dotspacemacs-major-mode-leader-key ","
   dotspacemacs-major-mode-emacs-leader-key "C-M-m"
   dotspacemacs-command-key ":"
   dotspacemacs-enable-paste-micro-state t
   dotspacemacs-which-key-delay 0.4
   dotspacemacs-loading-progress-bar t
   dotspacemacs-fullscreen-at-startup nil
   dotspacemacs-fullscreen-use-non-native nil
   dotspacemacs-maximized-at-startup t
   dotspacemacs-active-transparency 90
   dotspacemacs-inactive-transparency 90
   dotspacemacs-mode-line-unicode-symbols t
   dotspacemacs-smooth-scrolling t
   dotspacemacs-smartparens-strict-mode nil
   dotspacemacs-highlight-delimiters 'all
   dotspacemacs-persistent-server nil
   dotspacemacs-search-tools '("ag" "pt" "ack" "grep")
   dotspacemacs-default-package-repository nil))

(defun dotspacemacs/user-init ()
  (setq-default git-magit-status-fullscreen t)
  (setq save-interprogram-paste-before-kill t)
  ;; anaconda-mode has/had problems detecting anaconda_mode.py's directory
  (setq anaconda-mode-server-directory "/usr/local/lib/python2.7/dist-packages")
  (if (fboundp 'advice-add)
      (advice-add 'spacemacs/post-theme-init :after 'my-post-theme-init)
    (defadvice spacemacs/post-theme-init (after my-post-theme-init-adv activate)
      "Call `my-post-theme-init'."
      (my-post-theme-init theme))))

(defun dotspacemacs/user-config ()
  "Configuration function.
 This function is called at the very end of Spacemacs initialization after
layers configuration."
  (my-post-theme-init (car dotspacemacs-themes))
  (setq evil-escape-delay 0.2)

  ;; (define-key evil-normal-state-map (kbd ";") #'smex)
  ;; (define-key evil-motion-state-map (kbd ";") #'smex)
  ;; evil-snipe to repeat f/F/t/T by reptead typing of f/F/t/T, frees
  ;; ; for smex
  ;; (evil-snipe-override-mode)

  ;; powerline changes
  (setq powerline-default-separator 'slant)
  (spacemacs|define-mode-line-segment purpose
    (substring (purpose--modeline-string) 2 -1)
    :when purpose-mode)
  ;; (unless (memq 'purpose spacemacs-mode-line-left)
  ;;   (setq spacemacs-mode-line-left
  ;;         (-insert-at (1+ (-elem-index 'major-mode spacemacs-mode-line-left))
  ;;                     'purpose
  ;;                     spacemacs-mode-line-left)))
  ;; (diminish 'purpose-mode)
  (setq spacemacs-mode-line-version-controlp nil)
  ;; (setq spacemacs-mode-line-minor-modesp nil)

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

  ;; desktop save mode
  ;; (setq desktop-path (list spacemacs-cache-directory))
  ;; (desktop-save-mode)
  ;; (desktop-read)

  ;; smartparens (highlighting)
  (setq sp-highlight-pair-overlay nil)
  (setq sp-show-pair-from-inside t)

  ;; ycmd
  ;; (setq ycmd-server-command `("python" ,(expand-file-name "~/src/ycmd/ycmd/__main__.py")))

  (setq frame-title-format "Spacemacs")

  ;; dired
  (setq dired-guess-shell-alist-user
        '(("\\.pdf\\'" "evince")
          ("\\.ods\\'\\|\\.xlsx?\\'\\|\\.docx?\\'\\|\\.csv\\'" "libreoffice")
          ("\\.jpg\\'" "gpicview")))

  ;; nyan-mode
  (setq nyan-bar-length 16)
  ;; avy
  (setq avy-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))

  (which-key-setup-side-window-right-bottom)

  ;; (cl-pushnew '("Cask$" . emacs-lisp-mode) auto-mode-alist
  ;;             :key #'car :test #'equal)
  ;; window-purpose
  (with-eval-after-load 'window-purpose
    (cl-pushnew '(conf-mode . edit) purpose-user-mode-purposes :key #'car)
    (cl-pushnew '("\\.log$" . log) purpose-user-regexp-purposes
                :key #'car :test #'equal)
    (cl-pushnew '(web-mode-prog-mode . edit) purpose-user-mode-purposes :key #'car)
    (cl-pushnew '(org-mode . org) purpose-user-mode-purposes :key #'car)
    (cl-pushnew '(".travis.yml" . edit) purpose-user-name-purposes
                :key #'car :test #'equal)
    (purpose-compile-user-configuration)

    (when (require 'window-purpose-x nil t)
      (purpose-x-magit-single-on))

    ;; because of winconf2
    ;; (popwin-mode -1)
    ;; (pupo-mode -1)
    ;; (setq popwin:special-display-config
    ;;       (cl-delete "*Help*" popwin:special-display-config
    ;;                  :key #'car :test #'equal))
    (pupo/update-purpose-config)
    )

  ;; eyebrowse
  (with-eval-after-load 'eyebrowse
    (dolist (index (mapcar #'number-to-string '(0 1 2 3 4 5 6 7 8 9)))
      (define-key eyebrowse-mode-map
        (kbd (concat "M-" index))
        (intern-soft (concat "eyebrowse-switch-to-window-config-" index)))
      (define-key window-numbering-keymap
        (kbd (concat "M-" index)) nil)))

  ;; nlinum
  (spacemacs|add-toggle line-numbers
                        :status nlinum-mode
                        :on (global-nlinum-mode)
                        :off (global-nlinum-mode -1)
                        :documentation "Show the line numbers."
                        :evil-leader "tn")

  ;; centered-cursor
  ;; (add-hook 'prog-mode-hook #'centered-cursor-mode)
  ;; (add-hook 'text-mode-hook #'centered-cursor-mode)

  ;; irfc
  (with-eval-after-load 'irfc-mode
    (evilify irfc-mode irfc-mode-map
             "g" #'irfc-table-jump
             "G" #'irfc-page-goto))

  ;; customize
  (with-eval-after-load 'cus-edit
    (evilify Custom-mode custom-mode-map))

  ;; flycheck
  (add-to-list 'evil-motion-state-modes 'flycheck-error-list-mode)
  (with-eval-after-load 'flycheck
    (evil-define-key 'motion flycheck-error-list-mode-map
      (kbd "RET") #'flycheck-error-list-goto-error
      "j" #'flycheck-error-list-next-error
      "k" #'flycheck-error-list-previous-error))

  (evil-define-key 'motion help-mode-map
    (kbd "TAB") #'forward-button)

  (defun toggle-tabs-mode ()
    (interactive)
    (setq indent-tabs-mode (not indent-tabs-mode)))

  (with-eval-after-load 'helm
    (define-key helm-map (kbd "C-M-h") #'help-command))
  (setq helm-locate-fuzzy-match nil)

  ;; (define-key evil-motion-state-map (kbd "0") #'evil-first-non-blank)
  ;; (define-key evil-motion-state-map (kbd "0") #'evil-digit-argument-or-evil-beginning-of-line)

  (evil-leader/set-key "ot" #'toggle-tabs-mode)
  (evil-leader/set-key "ob" #'my-switch-buffer)
  (evil-leader/set-key "ol" #'helm-locate)

  (require 'f)
  (when (f-exists? (f-expand "~/.emacs.d/private/machine.el"))
    (load-file (f-expand "~/.emacs.d/private/machine.el"))
    (load-machine-config)))

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
       ;; `popup-tip-face' used by flycheck tips
       ;; `(popup-tip-face ((t (:foreground ,cursor :background ,active2 :bold nil))))
       ;; `(popup-tip-face ((t (:foreground "black" :background ,comment :bold nil :box t))))
       ;; `tooltip' used by company-quickhelp tips
       ;; `(tooltip ((t (:foreground ,active1 :background "#ad9dca"))))
       ;; `(tooltip ((t (:foreground "#efefef" :background "#4d3d6a"))))
       )))))

(with-eval-after-load 'helm-buffers
  (defclass my-buffers-source (helm-source-buffers)
    ((buffer-list
      :initform (lambda ()
                  (let* ((current-buffer (current-buffer))
                         (persp-p (fboundp 'persp-buffers))
                         (persp-buffs (and persp-p (persp-buffers persp-curr)))
                         (proj-p (and (fboundp 'projectile-project-buffers)
                                      (projectile-project-p)))
                         (proj-buffs (and proj-p (projectile-project-buffers)))
                         (purp-p (fboundp 'purpose-buffer-purpose))
                         (current-purpose (and purp-p (purpose-buffer-purpose current-buffer))))
                    (cl-loop for buffer in (buffer-list)
                             if (and (or (not persp-p)
                                         (memq buffer persp-buffs))
                                     (or (not proj-p)
                                         (memq buffer proj-buffs))
                                     (or (not purp-p)
                                         (eq (purpose-buffer-purpose buffer) current-purpose))
                                     (not (eq buffer current-buffer)))
                             collect (buffer-name buffer)))))))

  (defun my-switch-buffer ()
    (interactive)
    (helm :buffer "*helm-my-buffers*"
          :prompt "Buffer: "
          :sources (helm-make-source "My buffers" 'my-buffers-source))))


;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ahs-case-fold-search nil)
 '(ahs-default-range (quote ahs-range-whole-buffer))
 '(ahs-idle-interval 0.25)
 '(ahs-idle-timer 0 t)
 '(ahs-inhibit-face-list nil)
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["black" "red3" "ForestGreen" "yellow3" "blue" "magenta3" "DeepSkyBlue" "gray50"])
 '(expand-region-contract-fast-key "V")
 '(expand-region-reset-fast-key "r")
 '(fci-rule-color "#ECEFF1" t)
 '(paradox-github-token t)
 '(purpose-mode t)
 '(ring-bell-function (quote ignore) t)
 '(safe-local-variable-values
   (quote
    ((projectile-tags-file-name . "cscope.out")
     (cscope-option-do-not-update-database . t))))
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#FF5722")
     (40 . "#ff9800")
     (60 . "#fbc02d")
     (80 . "#558b2f")
     (100 . "#00796b")
     (120 . "#2196f3")
     (140 . "#4527A0")
     (160 . "#FF5722")
     (180 . "#ff9800")
     (200 . "#fbc02d")
     (220 . "#558b2f")
     (240 . "#00796b")
     (260 . "#2196f3")
     (280 . "#4527A0")
     (300 . "#FF5722")
     (320 . "#ff9800")
     (340 . "#fbc02d")
     (360 . "#558b2f"))))
 '(vc-annotate-very-old-color nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-tooltip-common ((t (:inherit company-tooltip :weight bold :underline nil))))
 '(company-tooltip-common-selection ((t (:inherit company-tooltip-selection :weight bold :underline nil)))))
