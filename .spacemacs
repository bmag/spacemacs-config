;; -*- mode: dotspacemacs -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

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
    ))))

(defun dotspacemacs/layers ()
  "Configuration Layers declaration."
  (setq-default
   ;; List of additional paths where to look for configuration layers.
   ;; Paths must have a trailing slash (ie. `~/.mycontribs/')
   dotspacemacs-configuration-layer-path '()
   ;; List of configuration layers to load. If it is the symbol `all' instead
   ;; of a list then all discovered layers will be installed.
   dotspacemacs-configuration-layers '(auto-completion
                                       clojure
                                       git
                                       perspectives
                                       python
                                       smex
                                       themes-megapack
                                       window-purpose)
   ;; A list of packages and/or extensions that will not be install and loaded.
   dotspacemacs-excluded-packages '()
   ;; If non-nil spacemacs will delete any orphan packages, i.e. packages that
   ;; are declared in a layer which is not a member of
   ;; the list `dotspacemacs-configuration-layers'
   dotspacemacs-delete-orphan-packages t))

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
   ;; Specify the startup banner. Default value is `official', it displays
   ;; the official spacemacs logo. An integer value is the index of text
   ;; banner, `random' chooses a random text banner in `core/banners'
   ;; directory. A string value must be a path to a .PNG file.
   ;; If the value is nil then no banner is displayed.
   ;; dotspacemacs-startup-banner 'official
   dotspacemacs-startup-banner 'official
   ;; List of items to show in the startup buffer. If nil it is disabled.
   ;; Possible values are: `recents' `bookmarks' `projects'."
   dotspacemacs-startup-lists '(recents projects)
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
  (setq window-purpose-load-extensions t)
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
  (with-eval-after-load 'evil-leader
    (progn
      (evil-leader/set-key "xts" 'transpose-sexps)
      (evil-leader/set-key
        "ob" 'purpose-switch-buffer-with-purpose
        "oB" 'switch-buffer-without-purpose
        "od" 'purpose-toggle-window-purpose-dedicated
        "o;" 'purpose-delete-non-dedicated-windows
        "oo" 'window-purpose/other-buffer)))
  (define-key spacemacs-mode-map (kbd "<backtab>") 'widget-backward)
  ;; need anaconda-mode's fork for these to work:
  (evil-leader/set-key-for-mode 'python-mode
    "mgt" 'anaconda-mode-goto-top-level
    "mht" 'anaconda-mode-view-doc-top-level)


  (defvar persp-special-display-actions nil)
  (defun persp/remove-display-actions ()
    (dolist (action persp-special-display-actions)
      (setq purpose-special-action-sequences
            (cl-remove (car action)
                       purpose-special-action-sequences
                       :key 'car))))
  (defun persp/add-display-actions (actions)
    (dolist (action actions)
      (add-to-list 'purpose-special-action-sequences action)
      (add-to-list 'persp-special-display-actions action)))
  (defun persp/load-persp (persp-conf)
    (purpose-set-extension-configuration :perspective
                                         (plist-get persp-conf :purposes))
    (cond ((plist-get persp-conf :layout-object)
           (purpose-set-window-layout (plist-get persp-conf :layout-object)))
          ((plist-get persp-conf :layout-filename)
           (purpose-load-window-layout (plist-get persp-conf :layout-filename))))
    (persp/remove-display-actions)
    (persp/add-display-actions (plist-get persp-conf :special-display-actions)))

  (defvar sicp-persp
    (list :layout-filename "~/.emacs.d/private/layouts/sicp.window-layout"
          :layout-object nil
          :purposes (purpose-conf "sicp"
                                  :mode-purposes '((python-mode . py)
                                                   (inferior-python-mode . repl)))
          :special-display-actions
          `((repl purpose-display-reuse-window-buffer
                  purpose-display-reuse-window-purpose
                  ,(purpose-generate-display-and-dedicate 'purpose-display-at-bottom 8)))))
  (defun custom-persp/sicp ()
    (interactive)
    (custom-persp
     "sicp"
     (persp/load-persp sicp-persp)
     (find-file "~/Documents/python/sicp/a.py")
     (run-python (python-shell-parse-command))
     (display-buffer (python-shell-get-buffer))))
  (evil-leader/set-key "Los" 'custom-persp/sicp)
  )

;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.
