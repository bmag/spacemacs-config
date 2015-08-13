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
     markdown
     org
     ;; sql

     slime
     (shell :variables shell-default-shell 'shell)
     gtags
     cscope
     search-engine
     unimpaired

     smex
     themes-megapack

     perspectives
     eyebrowse
     window-purpose
     (colors :variables colors-enable-nyan-cat-progress-bar ,(display-graphic-p))

     ;; private layers
     my-python
     command-log
     ;; helm-smex
     ;; winconf
     winconf2
     )
   dotspacemacs-additional-packages
   '(nlinum imenu-list let-alist f tabbar tabbar-ruler jinja2-mode)
   dotspacemacs-excluded-packages '(php-extras)
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
                         ;; tangotango
                         ;; leuven
                         )
   dotspacemacs-colorize-cursor-according-to-state t
   dotspacemacs-default-font '("Source Code Pro"
                               :size 13
                               :weight normal
                               :width normal
                               :powerline-scale 1.1)
   dotspacemacs-leader-key "SPC"
   dotspacemacs-emacs-leader-key "M-m"
   dotspacemacs-major-mode-leader-key ","
   dotspacemacs-major-mode-emacs-leader-key "C-M-m"
   dotspacemacs-command-key ";"
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
   dotspacemacs-default-package-repository nil)

  ;; User initialization goes here
  (setq-default git-magit-status-fullscreen t)
  (setq save-interprogram-paste-before-kill t)
  (if (fboundp 'advice-add)
      (advice-add 'spacemacs/post-theme-init :after 'my-post-theme-init)
    (defadvice spacemacs/post-theme-init (after my-post-theme-init-adv activate)
      "Call `my-post-theme-init'."
      (my-post-theme-init theme))))

(defun dotspacemacs/config ()
  "Configuration function.
 This function is called at the very end of Spacemacs initialization after
layers configuration."
  (my-post-theme-init (car dotspacemacs-themes))
  (setq evil-escape-delay 0.2)

  ;; powerline changes
  (spacemacs|define-mode-line-segment purpose
    (substring (purpose--modeline-string) 2 -1)
    :when purpose-mode)
  (unless (memq 'purpose spacemacs-mode-line-left)
    (setq spacemacs-mode-line-left
          (-insert-at (1+ (-elem-index 'major-mode spacemacs-mode-line-left))
                      'purpose
                      spacemacs-mode-line-left)))
  ;; (diminish 'purpose-mode)
  (setq spacemacs-mode-line-version-controlp nil)
  (setq spacemacs-mode-line-minor-modesp nil)

  ;; fix ffap pinging when trying to complete such as "a.is"
  (setq ffap-machine-p-known 'reject)

  ;; auto-completion
  (setq company-idle-delay 0.01)
  (setq company-minimum-prefix-length 1)

  ;; comint
  (with-eval-after-load 'comint
    (evil-define-key 'normal comint-mode-map
      (kbd "M-p") #'comint-previous-matching-input-from-input
      (kbd "M-n") #'comint-next-matching-input-from-input
      (kbd "C-c M-r") #'comint-previous-input
      (kbd "C-c M-s") #'comint-next-input))

  ;; desktop save mode
  (setq desktop-path (list spacemacs-cache-directory))
  ;; (desktop-save-mode)
  ;; (desktop-read)

  ;; smartparens (highlighting)
  (setq sp-highlight-pair-overlay nil)

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
  (let ((new-descriptions
         ;; being higher in this list means the replacement is applied later
         '(;; avy
           ("avy-goto-word-or-subword-1" . "avy word")
           ("avy-goto-line"              . "avy line")
           ("pop-to-mark-command"        . "pop mark (local)")
           ("pop-global-mark"            . "pop mark (global)")

           ;; cscope
           ("helm-cscope-find-called-function"      . "find callies (cscope)")
           ;; using regexp here because `helm-cscope-find-called-this-function' is
           ;; truncated by which-key
           ("helm-cscope-find-calling-this-.*"      . "find callers (cscope)")
           ("helm-cscope-find-global-definition"    . "find definition (cscope)")
           ("helm-cscope-find-egrep-pattern"        . "find pattern (cscope)")
           ("helm-cscope-find-this-file"            . "find file (cscope)")
           ("helm-cscope-find-files-including-file" . "find includers (cscope)")
           ("helm-cscope-find-this-symbol"          . "find references (cscope)")
           ("helm-cscope-find-this-text-string"     . "find string (cscope)")
           ("cscope/run-pycscope"                   . "index files (cscope)")
           ("cscope-index-file"                     . "index files (cscope)")

           ;; python
           ("anaconda-mode-goto"                . "goto current")
           ("anaconda-mode-view-doc"            . "help current")
           ("anaconda-mode-usages"              . "usages current")
           ("python-shell-send-\\(.+\\)"        . "send \\1 to repl")
           ("python-shell-send-\\(.+\\)-switch" . "send \\1 to repl and switch")
           ("python-start-or-switch-repl"       . "open repl")

           ;; buffers
           ("copy-whole-buffer-to-clipboard" . "copy whole buffer")
           ("copy-clipboard-to-whole-buffer" . "paste whole buffer")
           ("spacemacs/safe-revert-buffer" . "revert buffer")
           ("spacemacs/safe-erase-buffer" . "erase buffer")
           ("spacemacs/next-useful-buffer" . "next buffer")
           ("spacemacs/previous-useful-buffer" . "previous buffer")

           ;; windows
           ("evil-window-\\(left\\|right\\|up\\|down\\)" . "move \\1")
           ("evil-window-move-\\(.*\\)-\\(.*\\)" . "move \\1 \\2")
           ("winner-undo" . "undo window change")
           ("winner-redo" . "redo window change")
           )))
    (dolist (nd new-descriptions)
      (setq which-key-description-replacement-alist
            (cl-delete (concat "\\`" (car nd) "\\'")
                       which-key-description-replacement-alist
                       :key #'car :test #'string=))
      ;; ensure the target matches the whole string
      (cl-pushnew (cons (concat "\\`" (car nd) "\\'") (cdr nd))
                  which-key-description-replacement-alist
                  :key #'car :test #'string=)))
  (which-key-add-key-based-replacements
   ", d" "debug"    "SPC m d" "debug"
   ", e" "eval"     "SPC m e" "eval"
   ", g" "goto"     "SPC m g" "goto"
   ", h" "help"     "SPC m h" "help"
   ", r" "refactor" "SPC m r" "refactor"
   ", s" "repl"     "SPC m s" "repl"
   ", t" "test"     "SPC m t" "test")

  ;; window-purpose
  (with-eval-after-load 'window-purpose
    (cl-pushnew '(conf-mode . edit) purpose-user-mode-purposes :key #'car)
    (cl-pushnew '("\\.log$" . log) purpose-user-regexp-purposes
                :key #'car :test #'equal)
    (cl-pushnew '(web-mode-prog-mode . edit) purpose-user-mode-purposes :key #'car)
    (cl-pushnew '(org-mode . org) purpose-user-mode-purposes :key #'car)
    (purpose-compile-user-configuration)

    (when (require 'window-purpose-x nil t)
      (purpose-x-magit-single-on))

    ;; because of winconf2
    ;; (popwin-mode -1)
    ;; (pupo-mode -1)
    (setq popwin:special-display-config
          (cl-delete "*Help*" popwin:special-display-config
                     :key #'car :test #'equal))
    (push '("*anaconda-nav*" :dedicated t :position bottom :stick t :noselect nil) popwin:special-display-config))

  ;; eyebrowse
  (with-eval-after-load 'eyebrowse
    (dolist (index (mapcar #'number-to-string '(0 1 2 3 4 5 6 7 8 9)))
      (define-key eyebrowse-mode-map
        (kbd (concat "M-" index))
        (intern-soft (concat "eyebrowse-switch-to-window-config-" index)))
      (define-key window-numbering-keymap
        (kbd (concat "M-" index)) nil))

    (defun my-workspace-from-window (&optional buffer)
      (interactive)
      (let ((buffer (or buffer (current-buffer))))
        (eyebrowse-switch-to-window-config-0)
        (delete-other-windows)
        (without-purpose (pop-to-buffer buffer))
        (delete-other-windows)))

    ;; copied from eyebrowse package and modified
    (spacemacs|define-micro-state workspaces
      :doc (spacemacs//workspaces-ms-documentation)
      :use-minibuffer t
      :evil-leader "W"
      :bindings
      ("W" my-workspace-from-window)
      ("0" eyebrowse-switch-to-window-config-0)
      ("1" eyebrowse-switch-to-window-config-1)
      ("2" eyebrowse-switch-to-window-config-2)
      ("3" eyebrowse-switch-to-window-config-3)
      ("4" eyebrowse-switch-to-window-config-4)
      ("5" eyebrowse-switch-to-window-config-5)
      ("6" eyebrowse-switch-to-window-config-6)
      ("7" eyebrowse-switch-to-window-config-7)
      ("8" eyebrowse-switch-to-window-config-8)
      ("9" eyebrowse-switch-to-window-config-9)
      ("<tab>" eyebrowse-last-window-config)
      ("C-i" eyebrowse-last-window-config)
      ("n" eyebrowse-next-window-config)
      ("N" eyebrowse-prev-window-config)
      ("p" eyebrowse-prev-window-config)
      ("r" spacemacs/workspaces-ms-rename)
      ("c" eyebrowse-close-window-config))
    )

  ;; imenu-list
  (global-set-key (kbd "C-`") #'imenu-list-minor-mode)
  (add-to-list 'evil-motion-state-modes 'imenu-list-major-mode)
  (with-eval-after-load 'imenu-list
    (evil-define-key 'motion imenu-list-major-mode
      "s" #'hs-toggle-hiding
      "d" #'imenu-list-display-entry
      "q" #'imenu-list-minor-mode))

  ;; nlinum
  (spacemacs|add-toggle line-numbers
                        :status nlinum-mode
                        :on (global-nlinum-mode)
                        :off (global-nlinum-mode -1)
                        :documentation "Show the line numbers."
                        :evil-leader "tn")

  ;; flycheck
  (add-to-list 'evil-motion-state-modes 'flycheck-error-list-mode)
  (with-eval-after-load 'flycheck
    (evil-define-key 'motion flycheck-error-list-mode-map
      (kbd "RET") #'flycheck-error-list-goto-error
      "j" #'flycheck-error-list-next-error
      "k" #'flycheck-error-list-previous-error))

  ;; anaconda
  (add-to-list 'evil-motion-state-modes 'anaconda-nav-mode)
  (with-eval-after-load 'anaconda-mode
    (evil-define-key 'motion anaconda-nav-mode-map
      (kbd "RET") #'anaconda-nav-goto-item
      "j" #'next-error
      "k" #'previous-error
      "J" #'anaconda-nav-next-module
      "K" #'anaconda-nav-previous-module
      "q" #'anaconda-nav-quit))

  (evil-define-key 'motion help-mode-map
    (kbd "TAB") #'forward-button)

  (defun toggle-tabs-mode ()
    (interactive)
    (setq indent-tabs-mode (not indent-tabs-mode)))

  (with-eval-after-load 'helm
    (define-key helm-map (kbd "C-M-h") #'help-command))

  (define-key evil-motion-state-map (kbd "0") #'evil-first-non-blank)

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
