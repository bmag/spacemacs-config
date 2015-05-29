;; -*- mode: dotspacemacs -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

(defun dotspacemacs/layers ()
  "Configuration Layers declaration."
  (setq-default
   ;; List of additional paths where to look for configuration layers.
   ;; Paths must have a trailing slash (ie. `~/.mycontribs/')
   dotspacemacs-configuration-layer-path '()
   ;; List of configuration layers to load. If it is the symbol `all' instead
   ;; of a list then all discovered layers will be installed.
   dotspacemacs-configuration-layers
   '(
     ;; --------------------------------------------------------
     ;; Example of useful layers you may want to use right away
     ;; Uncomment a layer name and press C-c C-c to install it
     ;; --------------------------------------------------------
     (auto-completion :variables
                      auto-completion-enable-company-yasnippet nil)
     ;; better-defaults
     (git :variables
          git-gutter-use-fringe t)
     markdown
     org
     syntax-checking

     ;; additional contrib layers
     emacs-lisp
     python
     smex
     themes-megapack
     perspectives
     php
     slime
     gtags

     ;; private layers
     my-python
     cscope
     ;; window-purpose
     )
   ;; A list of packages and/or extensions that will not be install and loaded.
   dotspacemacs-excluded-packages '(php-extras)
   ;; If non-nil spacemacs will delete any orphan packages, i.e. packages that
   ;; are declared in a layer which is not a member of
   ;; the list `dotspacemacs-configuration-layers'
   dotspacemacs-delete-orphan-packages nil))

(defun dotspacemacs/init ()
  "Initialization function.
This function is called at the very startup of Spacemacs initialization
before layers configuration."
  ;; This setq-default sexp is an exhaustive list of all the supported
  ;; spacemacs settings.
  (setq-default
   ;; Either `vim' or `emacs'. Evil is always enabled but if the variable
   ;; is `emacs' then the `holy-mode' is enabled at startup.
   dotspacemacs-editing-style 'vim
   ;; If non nil output loading progess in `*Messages*' buffer.
   dotspacemacs-verbose-loading nil
   ;; Specify the startup banner. Default value is `official', it displays
   ;; the official spacemacs logo. An integer value is the index of text
   ;; banner, `random' chooses a random text banner in `core/banners'
   ;; directory. A string value must be a path to a .PNG file.
   ;; If the value is nil then no banner is displayed.
   ;; dotspacemacs-startup-banner 'official
   dotspacemacs-startup-banner 'official
   ;; t if you always want to see the changelog at startup
   dotspacemacs-always-show-changelog t
   ;; List of items to show in the startup buffer. If nil it is disabled.
   ;; Possible values are: `recents' `bookmarks' `projects'."
   dotspacemacs-startup-lists '(recents projects bookmarks)
   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press <SPC> T n to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light)
   dotspacemacs-themes '(;; solarized-light
                         ;; solarized-dark
                         ;; leuven
                         ;; monokai
                         ;; zenburn
                         tangotango
                         solarized-light
                         )
   ;; If non nil the cursor color matches the state color.
   dotspacemacs-colorize-cursor-according-to-state t
   ;; Default font. `powerline-scale' allows to quickly tweak the mode-line
   ;; size to make separators look not too crappy.
   dotspacemacs-default-font '("Source Code Pro"
                               :size 13
                               :weight normal
                               :width normal
                               :powerline-scale 1.1)
   ;; The leader key
   dotspacemacs-leader-key "SPC"
   ;; The leader key accessible in `emacs state' and `insert state'
   dotspacemacs-emacs-leader-key "M-m"
   ;; Major mode leader key is a shortcut key which is the equivalent of
   ;; pressing `<leader> m`. Set it to `nil` to disable it.
   dotspacemacs-major-mode-leader-key ","
   ;; Major mode leader key accessible in `emacs state' and `insert state'
   dotspacemacs-major-mode-emacs-leader-key "C-M-m"
   ;; The command key used for Evil commands (ex-commands) and
   ;; Emacs commands (M-x).
   ;; By default the command key is `:' so ex-commands are executed like in Vim
   ;; with `:' and Emacs commands are executed with `<leader> :'.
   dotspacemacs-command-key ":"
   ;; If non nil the paste micro-state is enabled. While enabled pressing `p`
   ;; several times cycle between the kill ring content.
   dotspacemacs-enable-paste-micro-state t
   ;; Guide-key delay in seconds. The Guide-key is the popup buffer listing
   ;; the commands bound to the current keystrokes.
   dotspacemacs-guide-key-delay 0.4
   ;; If non nil a progress bar is displayed when spacemacs is loading. This
   ;; may increase the boot time on some systems and emacs builds, set it to
   ;; nil ;; to boost the loading time.
   dotspacemacs-loading-progress-bar t
   ;; If non nil the frame is fullscreen when Emacs starts up.
   ;; (Emacs 24.4+ only)
   dotspacemacs-fullscreen-at-startup nil
   ;; If non nil `spacemacs/toggle-fullscreen' will not use native fullscreen.
   ;; Use to disable fullscreen animations in OSX."
   dotspacemacs-fullscreen-use-non-native nil
   ;; If non nil the frame is maximized when Emacs starts up.
   ;; Takes effect only if `dotspacemacs-fullscreen-at-startup' is nil.
   ;; (Emacs 24.4+ only)
   dotspacemacs-maximized-at-startup t
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's active or selected.
   ;; Transparency can be toggled through `toggle-transparency'.
   dotspacemacs-active-transparency 90
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's inactive or deselected.
   ;; Transparency can be toggled through `toggle-transparency'.
   dotspacemacs-inactive-transparency 90
   ;; If non nil unicode symbols are displayed in the mode line.
   dotspacemacs-mode-line-unicode-symbols t
   ;; If non nil smooth scrolling (native-scrolling) is enabled. Smooth
   ;; scrolling overrides the default behavior of Emacs which recenters the
   ;; point when it reaches the top or bottom of the screen.
   dotspacemacs-smooth-scrolling t
   ;; If non-nil smartparens-strict-mode will be enabled in programming modes.
   dotspacemacs-smartparens-strict-mode nil
   ;; If non nil advises quit functions to keep server open when quitting.
   dotspacemacs-persistent-server nil
   ;; The default package repository used if no explicit repository has been
   ;; specified with an installed package.
   ;; Not used for now.
   dotspacemacs-default-package-repository nil
   )
  ;; User initialization goes here
  (setq-default git-magit-status-fullscreen t)
  (setq save-interprogram-paste-before-kill t)
  (if (fboundp 'advice-add)
      (advice-add 'spacemacs/post-theme-init :after 'my-post-theme-init)
    (defadvice spacemacs/post-theme-init (after my-post-theme-init-adv activate)
      "Call `my-post-theme-init'."
      (my-post-theme-init theme)))
  )

(defun dotspacemacs/config ()
  "Configuration function.
 This function is called at the very end of Spacemacs initialization after
layers configuration."
  ;; (setq browse-url-browser-function 'browse-url-firefox)
  ;; (dolist (mode-hook '(emacs-lisp-mode-hook python-mode-hook))
  ;;   (add-hook mode-hook 'paredit-mode))

  ;; (golden-ratio-mode)
  (with-eval-after-load 'comint
    (define-key comint-mode-map (kbd "M-p") #'comint-previous-matching-input-from-input)
    (define-key comint-mode-map (kbd "M-n") #'comint-next-matching-input-from-input)
    (define-key comint-mode-map (kbd "C-c M-r") #'comint-previous-input)
    (define-key comint-mode-map (kbd "C-c M-s") #'comint-previous-input))

  ;; (defvar work-purpose-conf
  ;;   (purpose-conf "work"
  ;;                 :mode-purposes '((conf-unix-mode . edit)
  ;;                                  (org-mode . org)
  ;;                                  (python-mode . py))
  ;;                 :regexp-purposes '(("\\.log$" . log))))
  ;; (purpose-set-extension-configuration :work work-purpose-conf)
  ;; (add-to-list 'purpose-user-name-purposes '("*Ilist*" . Ilist))
  ;; (purpose-compile-user-configuration)
  ;; (add-hook 'purpose-display-buffer-functions #'my-dedicate-repl)
  ;; (setq helm-display-function #'my-helm-display-buffer)
  (defun work-python-hook ()
    (setq-local indent-tabs-mode nil)
    (flycheck-mode -1))
  (defun make-work-settings ()
    (interactive)
    (setq-default python-indent-offset 4)
    (setq-default python-indent-guess-indent-offset nil)
    (add-hook 'python-mode-hook 'work-python-hook))
  (defun toggle-tabs-mode ()
    (interactive)
    (setq indent-tabs-mode (not indent-tabs-mode)))
  (evil-leader/set-key "ot" 'toggle-tabs-mode)
  (evil-leader/set-key-for-mode 'python-mode
    "mhj" 'jump-do-anaconda-view-doc
    "mhr" 'jump-do-anaconda-usages))

(defun my-post-theme-init (theme)
  "Personal additions to themes."
  (cond
   ((eql theme 'tangotango)
    (custom-theme-set-faces
     'tangotango
     '(spacemacs-mode-line-flycheck-info-face ((t (:foreground "dodger blue" :box (:line-width 1 :style released-button)))))
     '(spacemacs-mode-line-flycheck-warning-face ((t (:foreground "#edd400" :box (:line-width 1 :style released-button)))))
     '(spacemacs-mode-line-flycheck-error-face ((t (:foreground "tomato" :box (:line-width 1 :style released-button)))))
     '(rainbow-delimiters-depth-1-face ((t (:foreground "#729fcf"))))
     '(rainbow-delimiters-depth-2-face ((t (:foreground "sandy brown"))))
     '(rainbow-delimiters-depth-3-face ((t (:foreground "green yellow"))))
     '(rainbow-delimiters-depth-4-face ((t (:foreground "hot pink"))))
     '(rainbow-delimiters-depth-5-face ((t (:foreground "LightGoldenrod1"))))
     '(rainbow-delimiters-depth-6-face ((t (:foreground "light sky blue"))))
     '(rainbow-delimiters-depth-7-face ((t (:foreground "light green"))))
     '(rainbow-delimiters-depth-8-face ((t (:foreground "goldenrod"))))
     '(rainbow-delimiters-depth-9-face ((t (:foreground "orchid"))))
     '(evil-search-highlight-persist-highlight-face ((t (:background "orange3"))))
    ))))

;; taken from https://github.com/nex3/perspective-el/pull/43/files
(defun my-get-perspectives-for-buffer (buffer)
  "Get the names of all of the perspectives of which BUFFER is a member."
  (cl-loop for perspective being the hash-value of perspectives-hash
           if (member buffer (persp-buffers perspective))
           collect (persp-name perspective)))

;; taken from https://github.com/nex3/perspective-el/pull/43/files
(defun my-switch-buffer-and-perspective (buffer)
  "Switch to BUFFER and its perspective."
  (interactive "BBuffer: \n")
  (let* ((buffer (get-buffer-create buffer))
         (perspectives (my-get-perspectives-for-buffer buffer))
         (perspective (or (and (member (persp-name persp-curr) perspectives)
                               (persp-name persp-curr))
                          ;; perspectives' length is 1 (if length is 0 the result is nil)
                          (and (null (cdr perspectives))
                               (car perspectives))
                          (completing-read "Perspective: " perspectives))))
    (if (string= perspective (persp-name persp-curr))
        (switch-to-buffer buffer)
      (persp-switch perspective)
      (if (get-buffer-window buffer)
          (set-frame-selected-window nil (get-buffer-window buffer))
        (switch-to-buffer buffer)))))

(defun my-helm-display-buffer (buffer)
  (let ((window (or (purpose-display-reuse-window-buffer buffer nil)
                    (purpose-display-reuse-window-purpose buffer nil)
                    (purpose-display-at-bottom buffer nil 0.3))))
    (if window
        (progn
          (select-window window)
          (prog1
              ;; don't know why, but it doesn't work without `switch-to-buffer'
              (switch-to-buffer buffer t t)
            ;; prevent purpose from displaying another buffer in helm's window.
            ;; relevant for the persistent action of `helm-M-x' - without this
            ;; the help buffer may hide the helm buffer.
            (purpose-set-window-purpose-dedicated-p window t)))
      ;; in case the above methods weren't successful, fallback to default
      ;; helm display function
      (prog1
          (funcall #'helm-default-display-buffer buffer)
        (purpose-set-window-purpose-dedicated-p nil t)))))

(defvar jump-do-fn nil)
(defun jump-do-after-jump ()
  (remove-hook 'ace-jump-mode-end-hook #'jump-do-after-jump)
  (when (functionp jump-do-fn)
    (with-demoted-errors (funcall jump-do-fn))
    (setq jump-do-fn nil))
  (call-interactively #'ace-jump-mode-pop-mark))
(defun jump-do (fn)
  (setq jump-do-fn fn)
  (add-hook 'ace-jump-mode-end-hook #'jump-do-after-jump)
  (call-interactively #'ace-jump-char-mode))
(defmacro define-jump-do-command (name fn)
  `(defun ,name ()
     (interactive)
     (jump-do ,fn)))
(define-jump-do-command jump-do-anaconda-view-doc #'anaconda-mode-view-doc)
(define-jump-do-command jump-do-anaconda-usages #'anaconda-mode-usages)

(defun my-dedicate-repl (window)
  (when (eql (purpose-window-purpose window) 'repl)
    (purpose-set-window-purpose-dedicated-p window t)))

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
 '(paradox-github-token t)
 '(ring-bell-function (quote ignore) t)
 '(safe-local-variable-values
   (quote
    ((projectile-tags-file-name . "cscope.out")
     (cscope-option-do-not-update-database . t)))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-tooltip-common ((t (:inherit company-tooltip :weight bold :underline nil))))
 '(company-tooltip-common-selection ((t (:inherit company-tooltip-selection :weight bold :underline nil)))))
