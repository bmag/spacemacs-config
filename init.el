;; -*- mode: emacs-lisp -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

(defun dotspacemacs/layers ()
  "Configuration Layers declaration.
You should not put any user code in this function besides modifying the variable
values."
  (setq-default
   ;; Base distribution to use. This is a layer contained in the directory
   ;; `+distribution'. For now available distributions are `spacemacs-base'
   ;; or `spacemacs'. (default 'spacemacs)
   dotspacemacs-distribution 'spacemacs
   ;; Lazy installation of layers (i.e. layers are installed only when a file
   ;; with a supported type is opened). Possible values are `all', `unused'
   ;; and `nil'. `unused' will lazy install only unused layers (i.e. layers
   ;; not listed in variable `dotspacemacs-configuration-layers'), `all' will
   ;; lazy install any layer that support lazy installation even the layers
   ;; listed in `dotspacemacs-configuration-layers'. `nil' disable the lazy
   ;; installation feature and you have to explicitly list a layer in the
   ;; variable `dotspacemacs-configuration-layers' to install it.
   ;; (default 'unused)
   dotspacemacs-enable-lazy-installation nil
   ;; If non-nil then Spacemacs will ask for confirmation before installing
   ;; a layer lazily. (default t)
   dotspacemacs-ask-for-lazy-installation t
   ;; If non-nil layers with lazy install support are lazy installed.
   ;; List of additional paths where to look for configuration layers.
   ;; Paths must have a trailing slash (i.e. `~/.mycontribs/')

   ;; gotcha: using "~/.spacemacs.d/" is wrong then trying to use a
   ;; different dotspacemacs directory (e.g. via SPACEMACSDIR)
   dotspacemacs-configuration-layer-path (list (expand-file-name "layers/" dotspacemacs-directory))
   ;; List of configuration layers to load.
   dotspacemacs-configuration-layers
   '(
     ;; completion layer
     ;; helm
     ivy ivy-tweaks

     ;; tools
     (auto-completion :variables
                      auto-completion-complete-with-key-sequence "jk"
                      auto-completion-complete-with-key-sequence-delay 0.5
                      auto-completion-enable-snippets-in-popup nil
                      auto-completion-enable-help-tooltip t
                      company-tooltip-align-annotations t)
     command-log
     evil-cleverparens
     (git :variables git-magit-status-fullscreen t)
     ;; gtags
     ibuffer
     imenu-list
     nlinum
     org
     pdf-tools
     ;; ranger
     (shell :variables
            shell-default-shell 'shell
            shell-default-height 30
            shell-default-position 'bottom)
     ;; semantic
     smex
     syntax-checking
     version-control

     ;; languages
     c-c++
     (clojure :variables
              clojure-enable-fancify-symbols nil)
     emacs-lisp
     (latex :variables
            latex-enable-auto-fill t
            latex-enable-folding t)
     markdown
     octave
     python
     ruby

     ;; private layers
     common-lisp
     ;; disabled nameless because it delays opening of elisp files significally
     ;; (~2-3 seconds)
     ;; nameless
     spaceline-tweaks
     window-purpose
     window-tweaks

     ;; themes
     themes-megapack
     (theming :variables
              theming-modifications
              '((spacemacs-dark (aw-leading-char-face :foreground "red" :height 3.0))
                (spacemacs-light (aw-leading-char-face :foreground "red" :height 3.0))
                (monokai (aw-leading-char-face :foreground "red" :height 3.0))
                (flatland (aw-leading-char-face :foreground "red" :height 3.0))))
     )
   ;; List of additional packages that will be installed without being
   ;; wrapped in a layer. If you need some configuration for these
   ;; packages, then consider creating a layer. You can also put the
   ;; configuration in `dotspacemacs/user-config'.
   dotspacemacs-additional-packages '(tabbar-ruler flycheck-package vimrc-mode buttercup el-mock)
   ;; A list of packages that cannot be updated.
   dotspacemacs-frozen-packages '()
   ;; A list of packages that will not be install and loaded.
   dotspacemacs-excluded-packages '()
   ;; Defines the behaviour of Spacemacs when downloading packages.
   ;; Possible values are `used', `used-but-keep-unused' and `all'. `used' will
   ;; download only explicitly used packages and remove any unused packages as
   ;; well as their dependencies. `used-but-keep-unused' will download only the
   ;; used packages but won't delete them if they become unused. `all' will
   ;; download all the packages regardless if they are used or not and packages
   ;; won't be deleted by Spacemacs. (default is `used')
   dotspacemacs-download-packages 'used-but-keep-unused))

(defun dotspacemacs/init ()
  "Initialization function.
This function is called at the very startup of Spacemacs initialization
before layers configuration.
You should not put any user code in there besides modifying the variable
values."
  ;; This setq-default sexp is an exhaustive list of all the supported
  ;; spacemacs settings.
  (setq-default
   ;; If non nil ELPA repositories are contacted via HTTPS whenever it's
   ;; possible. Set it to nil if you have no way to use HTTPS in your
   ;; environment, otherwise it is strongly recommended to let it set to t.
   ;; This variable has no effect if Emacs is launched with the parameter
   ;; `--insecure' which forces the value of this variable to nil.
   ;; (default t)
   dotspacemacs-elpa-https t
   ;; Maximum allowed time in seconds to contact an ELPA repository.
   dotspacemacs-elpa-timeout 5
   ;; If non nil then spacemacs will check for updates at startup
   ;; when the current branch is not `develop'. (default t)
   dotspacemacs-check-for-update nil

   ;; `dotspacemacs-enable-multiple-emacs-version' doesn't exist yet, it's just
   ;; part of a PR for now.
   ;; If non nil, different emacs versions have different package directories.
   ;; e.g. for Emacs 24.5, packages are stored in elpa/24.5/. Rollback
   ;; directories are also separated.
   dotspacemacs-enable-multiple-emacs-version t
   ;; One of `vim', `emacs' or `hybrid'.
   ;; `hybrid' is like `vim' except that `insert state' is replaced by the
   ;; `hybrid state' with `emacs' key bindings. The value can also be a list
   ;; with `:variables' keyword (similar to layers). Check the editing styles
   ;; section of the documentation for details on available variables.
   ;; (default 'vim)
   dotspacemacs-editing-style '(hybrid :variables
                                       hybrid-mode-default-state 'normal
                                       hybrid-mode-enable-evilified-state t
                                       hybrid-mode-enable-hjkl-bindings nil)
   ;; If non nil output loading progress in `*Messages*' buffer. (default nil)
   dotspacemacs-verbose-loading nil
   ;; Specify the startup banner. Default value is `official', it displays
   ;; the official spacemacs logo. An integer value is the index of text
   ;; banner, `random' chooses a random text banner in `core/banners'
   ;; directory. A string value must be a path to an image format supported
   ;; by your Emacs build.
   ;; If the value is nil then no banner is displayed. (default 'official)
   dotspacemacs-startup-banner 'official
   ;; List of items to show in startup buffer or an association list of of
   ;; the form `(list-type . list-size)`. If nil it is disabled.
   ;; Possible values for list-type are:
   ;; `recents' `bookmarks' `projects' `agenda' `todos'."
   dotspacemacs-startup-lists '((recents . 10)
                                (projects . 7))
   ;; Default major mode of the scratch buffer (default `text-mode')
   dotspacemacs-scratch-mode 'emacs-lisp-mode
   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press <SPC> T n to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light)
   dotspacemacs-themes '(spacemacs-dark
                         spacemacs-light
                         monokai
                         flatland)
   ;; If non nil the cursor color matches the state color in GUI Emacs.
   dotspacemacs-colorize-cursor-according-to-state t
   ;; Default font, or prioritized list of fonts. `powerline-scale' allows to
   ;; quickly tweak the mode-line size to make separators look not too crappy.
   dotspacemacs-default-font '("Source Code Pro"
                               :size 14
                               :weight normal
                               :width normal
                               :powerline-scale 1.1)
   ;; The leader key
   dotspacemacs-leader-key "SPC"
   ;; The leader key accessible in `emacs state' and `insert state'
   ;; (default "M-m")
   dotspacemacs-emacs-leader-key "M-m"
   ;; Major mode leader key is a shortcut key which is the equivalent of
   ;; pressing `<leader> m`. Set it to `nil` to disable it. (default ",")
   dotspacemacs-major-mode-leader-key ","
   ;; Major mode leader key accessible in `emacs state' and `insert state'.
   ;; (default "C-M-m)
   dotspacemacs-major-mode-emacs-leader-key "C-M-m"
   ;; The key used for Emacs commands (M-x) (after pressing on the leader key).
   ;; (default "SPC")
   dotspacemacs-emacs-command-key ":"
   ;; These variables control whether separate commands are bound in the GUI to
   ;; the key pairs C-i, TAB and C-m, RET.
   ;; Setting it to a non-nil value, allows for separate commands under <C-i>
   ;; and TAB or <C-m> and RET.
   ;; In the terminal, these pairs are generally indistinguishable, so this only
   ;; works in the GUI. (default nil)
   dotspacemacs-distinguish-gui-tab t
   ;; If non nil `Y' is remapped to `y$'. (default t)
   dotspacemacs-remap-Y-to-y$ t
   ;; If non-nil, the shift mappings `<' and `>' retain visual state if used
   ;; there. (default t)
   dotspacemacs-retain-visual-state-on-shift t
   ;; If non-nil, J and K move lines up and down when in visual mode.
   ;; (default nil)
   dotspacemacs-visual-line-move-text nil
   ;; If non nil, inverse the meaning of `g' in `:substitute' Evil ex-command.
   ;; (default nil)
   dotspacemacs-ex-substitute-global nil
   ;; Name of the default layout (default "Default")
   dotspacemacs-default-layout-name "Default"
   ;; If non nil the default layout name is displayed in the mode-line.
   ;; (default nil)
   dotspacemacs-display-default-layout nil
   ;; If non nil then the last auto saved layouts are resume automatically upon
   ;; start. (default nil)
   dotspacemacs-auto-resume-layouts nil
   ;; Size (in MB) above which spacemacs will prompt to open the large file
   ;; literally to avoid performance issues. Opening a file literally means that
   ;; no major mode or minor modes are active. (default is 1)
   dotspacemacs-large-file-size 1
   ;; Location where to auto-save files. Possible values are `original' to
   ;; auto-save the file in-place, `cache' to auto-save the file to another
   ;; file stored in the cache directory and `nil' to disable auto-saving.
   ;; (default 'cache)
   dotspacemacs-auto-save-file-location 'cache
   ;; Maximum number of rollback slots to keep in the cache. (default 5)
   dotspacemacs-max-rollback-slots 7
   ;; If non nil, `helm' will try to minimize the space it uses. (default nil)
   dotspacemacs-helm-resize nil
   ;; if non nil, the helm header is hidden when there is only one source.
   ;; (default nil)
   dotspacemacs-helm-no-header nil
   ;; define the position to display `helm', options are `bottom', `top',
   ;; `left', or `right'. (default 'bottom)
   dotspacemacs-helm-position 'bottom
   ;; If non nil the paste micro-state is enabled. When enabled pressing `p`
   ;; several times cycle between the kill ring content. (default nil)
   dotspacemacs-enable-paste-transient-state t
   ;; Which-key delay in seconds. The which-key buffer is the popup listing
   ;; the commands bound to the current keystroke sequence. (default 0.4)
   dotspacemacs-which-key-delay 0.4
   ;; Which-key frame position. Possible values are `right', `bottom' and
   ;; `right-then-bottom'. right-then-bottom tries to display the frame to the
   ;; right; if there is insufficient space it displays it at the bottom.
   ;; (default 'bottom)
   dotspacemacs-which-key-position 'bottom
   ;; If non nil a progress bar is displayed when spacemacs is loading. This
   ;; may increase the boot time on some systems and emacs builds, set it to
   ;; nil to boost the loading time. (default t)
   dotspacemacs-loading-progress-bar t
   ;; If non nil the frame is fullscreen when Emacs starts up. (default nil)
   ;; (Emacs 24.4+ only)
   dotspacemacs-fullscreen-at-startup nil
   ;; If non nil `spacemacs/toggle-fullscreen' will not use native fullscreen.
   ;; Use to disable fullscreen animations in OSX. (default nil)
   dotspacemacs-fullscreen-use-non-native nil
   ;; If non nil the frame is maximized when Emacs starts up.
   ;; Takes effect only if `dotspacemacs-fullscreen-at-startup' is nil.
   ;; (default nil) (Emacs 24.4+ only)
   dotspacemacs-maximized-at-startup nil
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's active or selected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-active-transparency 90
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's inactive or deselected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-inactive-transparency 90
   ;; If non nil show the titles of transient states. (default t)
   dotspacemacs-show-transient-state-title t
   ;; If non nil show the color guide hint for transient state keys. (default t)
   dotspacemacs-show-transient-state-color-guide t
   ;; If non nil unicode symbols are displayed in the mode line. (default t)
   dotspacemacs-mode-line-unicode-symbols t
   ;; If non nil smooth scrolling (native-scrolling) is enabled. Smooth
   ;; scrolling overrides the default behavior of Emacs which recenters point
   ;; when it reaches the top or bottom of the screen. (default t)
   dotspacemacs-smooth-scrolling t
   ;; If non nil line numbers are turned on in all `prog-mode' and `text-mode'
   ;; derivatives. If set to `relative', also turns on relative line numbers.
   ;; (default nil)
   dotspacemacs-line-numbers nil
   ;; Code folding method. Possible values are `evil' and `origami'.
   ;; (default 'evil)
   dotspacemacs-folding-method 'origami
   ;; If non-nil smartparens-strict-mode will be enabled in programming modes.
   ;; (default nil)
   dotspacemacs-smartparens-strict-mode t
   ;; If non-nil pressing the closing parenthesis `)' key in insert mode passes
   ;; over any automatically added closing parenthesis, bracket, quote, etc…
   ;; This can be temporary disabled by pressing `C-q' before `)'. (default nil)
   dotspacemacs-smart-closing-parenthesis nil
   ;; Select a scope to highlight delimiters. Possible values are `any',
   ;; `current', `all' or `nil'. Default is `all' (highlight any scope and
   ;; emphasis the current one). (default 'all)
   dotspacemacs-highlight-delimiters 'all
   ;; If non nil advises quit functions to keep server open when quitting.
   ;; (default nil)
   dotspacemacs-persistent-server nil
   ;; List of search tool executable names. Spacemacs uses the first installed
   ;; tool of the list. Supported tools are `ag', `pt', `ack' and `grep'.
   ;; (default '("ag" "pt" "ack" "grep"))
   dotspacemacs-search-tools '("ag" "pt" "ack" "grep")
   ;; The default package repository used if no explicit repository has been
   ;; specified with an installed package.
   ;; Not used for now. (default nil)
   dotspacemacs-default-package-repository nil
   ;; Delete whitespace while saving buffer. Possible values are `all'
   ;; to aggressively delete empty line and long sequences of whitespace,
   ;; `trailing' to delete only the whitespace at end of lines, `changed'to
   ;; delete only whitespace for changed lines or `nil' to disable cleanup.
   ;; (default nil)
   dotspacemacs-whitespace-cleanup nil
   ))

(defun dotspacemacs/user-init ()
  "Initialization function for user code.
It is called immediately after `dotspacemacs/init', before layer configuration
executes.
 This function is mostly useful for variables that need to be set
before packages are loaded. If you are unsure, you should try in setting them in
`dotspacemacs/user-config' first."
  (when (eq system-type 'gnu/linux)
    (let ((machine-home (expand-file-name "machines/linux/" dotspacemacs-directory)))
      ;; give machine its own cache
      (setq spacemacs-cache-directory (expand-file-name "cache/" machine-home))))

  (when (eq system-type 'windows-nt)
    (let ((machine-home (expand-file-name "machines/windows/" dotspacemacs-directory)))
      ;; give machine its own cache
      (setq spacemacs-cache-directory (expand-file-name "cache/" machine-home))
      (push (expand-file-name "bin/" machine-home) exec-path)
      (push (expand-file-name "bin/diffutils/" machine-home) exec-path)))

  ;; share packages between machines (`dotspacemacs-directory' and `package-user-dir' are shared)
  (setq custom-file (expand-file-name "mycustom.el" dotspacemacs-directory))
  (setq configuration-layer-rollback-directory (expand-file-name "rollback" dotspacemacs-directory))

  (load custom-file 'noerror)
  (setq paradox-github-token t)
  )

(defun dotspacemacs/user-config ()
  "Configuration function for user code.
This function is called at the very end of Spacemacs initialization after
layers configuration. You are free to put any user code."
  ;; small graphical changes
  (setq frame-title-format "Spacemacs")

  ;; recenter window-point after imenu jumps
  (add-hook 'imenu-after-jump-hook (lambda () (recenter 10)))

  ;; TODO: make a PR for evil-cp settings
  ;; properly enable/disable cleverparens
  (add-hook 'smartparens-enabled-hook #'spacemacs/toggle-evil-cleverparens-on)
  (add-hook 'smartparens-disabled-hook #'spacemacs/toggle-evil-cleverparens-off)
  ;; cleverparens works better with these settings
  (setq evil-move-cursor-back nil
        evil-move-beyond-eol t)

  ;; anzu commands provide a preview of matches
  (global-set-key [remap query-replace] #'anzu-query-replace)
  (global-set-key [remap query-replace-regexp] #'anzu-query-replace-regexp)

  (when (configuration-layer/layer-usedp 'helm)
    ;; un-fuzzy helm-locate
    (setq helm-locate-fuzzy-match nil)
    (spacemacs/set-leader-keys "oi" #'helm-comint-input-ring))

  (when (configuration-layer/layer-usedp 'ivy)
    (spacemacs/set-leader-keys "oi" #'counsel-shell-history))

  ;; TODO: make PR upstream?
  ;; automatically toggle emacs state with artist-mode
  (defun artist-mode-toggle-emacs-state ()
    (if artist-mode
        (unless (evil-emacs-state-p)
          (evil-emacs-state))
      (evil-exit-emacs-state)))
  (unless (eq dotspacemacs-editing-style 'emacs)
    (add-hook 'artist-mode-hook #'artist-mode-toggle-emacs-state))

  (when (configuration-layer/package-usedp 'avy)
    ;; (spacemacs/set-leader-keys "jj" #'evil-avy-goto-word-0)
    (spacemacs/set-leader-keys "SPC" #'evil-avy-goto-word-0)
    (with-eval-after-load 'avy
      (add-to-list 'avy-keys-alist
                   (cons 'avy-goto-word-0 (list ?a ?s ?d ?f ?l ?k ?j
                                                ?w ?e ?u ?i ?v ?n
                                                ?g ?h)))))

  (when (configuration-layer/package-usedp 'magit)
    (add-to-list 'spacemacs-useful-buffers-regexp "^\\*magit"))
  ;; (spacemacs/set-leader-keys
  ;;   "rsw" #'window-configuration-to-register
  ;;   "rsf" #'frame-configuration-to-register
  ;;   "rsp" #'point-to-register
  ;;   "rj" #'jump-to-register)
  ;; (spacemacs/declare-prefix "rs" "save register")
  ;; (spacemacs/declare-prefix "rj" "load register")

  ;; tabbar-ruler causes display issues for hydra-powered transient states :-(
  ;; (use-package tabbar-ruler
  ;;   :init
  ;;   (setq tabbar-ruler-global-tabbar t)
  ;;   :config
  ;;   (setq tabbar-buffer-groups-function 'tabbar-buffer-groups))

  ;; let emacs know where to find source files
  (when (and (version< emacs-version "25.0")
             (version<= "24.5" emacs-version))
    ;; this doesn't working...
    ;; (setq find-function-source-path '("~/Documents/emacs/24.5/lisp/"))
    (setq find-function-C-source-directory "~/Documents/emacs/24.5/src/"))

  (defun set-transperancy (&optional frame)
    (let ((frame (or frame (selected-frame))))
      (if (display-graphic-p frame)
          (set-frame-parameter frame 'alpha '(80 80))
        (set-face-background 'default "unspecified-bg" frame))))

  ;; TODO: move to a layer (or add upstream?)
  (when (and (configuration-layer/layer-usedp 'auto-complete)
             (configuration-layer/layer-usedp 'octave))
    (defun company-octave (command &optional arg &rest args)
      "`company-mode' backend using `completion-at-point-functions'."
      (interactive (list 'interactive))
      (if (eq command 'doc-buffer)
          ;; TODO `octave-help' displays the help buffer, we'd rather use a function
          ;; that doesn't display the help buffer
          (save-window-excursion
            (ignore-errors
              (octave-help arg)
              (get-buffer octave-help-buffer)))
        (apply #'company-capf command arg args)))

    (spacemacs|defvar-company-backends octave-mode)
    (spacemacs|defvar-company-backends inferior-octave-mode)
    (spacemacs|add-company-hook octave-mode)
    (spacemacs|add-company-hook inferior-octave-mode)
    (push 'company-octave company-backends-octave-mode)
    (push 'company-octave company-backends-inferior-octave-mode))

  (defun bm-add-project-buffers-to-layout ()
    "Add all of current project's buffers to current layout.
Handy after creating project layout with [SPC p l]."
    (interactive)
    (let ((persp-switch-to-added-buffer nil))
      (mapc #'persp-add-buffer (projectile-project-buffers))))
  )
