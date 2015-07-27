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
     ;; c-c++
     clojure
     emacs-lisp
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

     smex
     themes-megapack

     perspectives
     eyebrowse
     window-purpose
     (colors :variables colors-enable-nyan-cat-progress-bar ,(display-graphic-p))

     ;; private layers
     ;; sr-speedbar
     my-python
     helm-smex
     ;; winconf
     winconf2
     )
   dotspacemacs-additional-packages
   '(nlinum window-purpose imenu-list let-alist f tabbar tabbar-ruler which-key)
   dotspacemacs-excluded-packages '(php-extras)
   dotspacemacs-delete-orphan-packages t))

(defun dotspacemacs/init ()
  "Initialization function.
This function is called at the very startup of Spacemacs initialization
before layers configuration."
  (setq-default
   dotspacemacs-editing-style 'vim
   dotspacemacs-verbose-loading nil
   dotspacemacs-startup-banner 'official
   dotspacemacs-always-show-changelog t
   dotspacemacs-startup-lists '(recents projects)
   dotspacemacs-startup-recent-list-size 5
   dotspacemacs-themes '(spacemacs-dark
                         spacemacs-light
                         tangotango
                         leuven)
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

  ;; fix ffap pinging when trying to complete such as "a.is"
  (setq ffap-machine-p-known 'reject)

  ;; auto-completion
  (setq company-idle-delay 0.01)
  (setq company-minimum-prefix-length 1)

  ;; comint
  (with-eval-after-load 'comint
    (define-key comint-mode-map (kbd "M-p") #'comint-previous-matching-input-from-input)
    (define-key comint-mode-map (kbd "M-n") #'comint-next-matching-input-from-input)
    (define-key comint-mode-map (kbd "C-c M-r") #'comint-previous-input)
    (define-key comint-mode-map (kbd "C-c M-s") #'comint-next-input))

  ;; python
  (defvar remote-ipython-buffer nil)
  (defun open-remote-ipython ()
    (interactive)
    (if (buffer-live-p remote-ipython-buffer)
        (pop-to-buffer remote-ipython-buffer)
      (prog1
          (run-python "/usr/bin/ipython console --existing" t 0)
        (setq remote-ipython-buffer (current-buffer)))))
  (evil-leader/set-key "or" #'open-remote-ipython)

  ;; popup repls

  (with-eval-after-load 'window-purpose-x
    ;; (purpose-set-extension-configuration
    ;;  :popup-repls
    ;;  (purpose-conf "popup-repls"
    ;;                :mode-purposes '((inferior-python-mode . REPL)
    ;;                                 (inferior-emacs-lisp-mode . REPL))))
    ;; (purpose-x-popupify-purpose 'REPL)

    ;; (defun dedicate-repl (window)
    ;;   (when (eql (purpose-window-purpose window) 'REPL)
    ;;     (purpose-set-window-purpose-dedicated-p window t)))
    ;; (add-hook 'purpose-display-buffer-functions #'dedicate-repl)
    ;; (setq purpose-display-at-bottom-height 0.25)

    (defun open-recent-repl ()
      "Select open REPL window, or pop to recently used REPL."
      (interactive)
      (let ((window (car (purpose-windows-with-purpose 'REPL))))
        (if window
            (select-window window)
          (let ((buffer (car (purpose-buffers-with-purpose 'REPL))))
            (if buffer
                (pop-to-buffer buffer)
              (user-error "No REPL buffer exists"))))))

    (evil-leader/set-key-for-mode 'python-mode
      "m'" #'python-start-or-switch-repl)
    (evil-leader/set-key-for-mode 'emacs-lisp-mode
      "m'" #'ielm)
    (evil-leader/set-key-for-mode 'dired-mode
      "m'" #'open-recent-repl)
    (dolist (repl-mode '(inferior-emacs-lisp-mode inferior-python-mode))
      (evil-leader/set-key-for-mode repl-mode
        "m'" #'quit-window)))


  ;; ycmd
  ;; (setq ycmd-server-command `("python" ,(expand-file-name "~/src/ycmd/ycmd/__main__.py")))

  (setq frame-title-format "Spacemacs")

  ;; dired
  (setq dired-guess-shell-alist-user
        '(("\\.pdf\\'" "evince")
          ("\\.ods\\'\\|\\.xlsx?\\'\\|\\.docx?\\'\\|\\.csv\\'" "libreoffice")
          ("\\.jpg\\'" "gpicview")))

  ;; which-key
  ;; (which-key-mode)
  (which-key-setup-side-window-bottom)
  ;; (which-key-add-major-mode-key-based-replacements
  ;;  'python-mode
  ;;  ", g C" "find callers"     "SPC m g C" "find callers"
  ;;  ", g c" "find called"      "SPC m g c" "find called"
  ;;  ", g d" "find definition"  "SPC m g d" "find definition"
  ;;  ", g r" "find references"  "SPC m g r" "find references"
  ;;  ", g g" "goto symbol"      "SPC m g g" "goto symbol"
  ;;  ", g i" "goto ipc"         "SPC m g i" "goto ipc"
  ;;  )
  ;; (which-key-add-key-based-replacements
  ;;  "SPC /" "search in project"
  ;;  "SPC s f" "search in files"
  ;;  "SPC s b" "search in buffers"
  ;;  "SPC P" "holy-mode"
  ;;  "SPC m" "mode-specific"
  ;;  "SPC o" "personal"
  ;;  "SPC b Y" "copy whole buffer"
  ;;  "SPC b P" "paste whole buffer"
  ;;  "SPC w j" "move down"
  ;;  "SPC w k" "move up"
  ;;  "SPC w h" "move left"
  ;;  "SPC w l" "move right"
  ;;  "SPC w J" "move far down"
  ;;  "SPC w K" "move far up"
  ;;  "SPC w H" "move far left"
  ;;  "SPC w L" "move far right")
  ;; (cl-loop for entry in '(("spacemacs/\\(.+\\)" . "\\1")
  ;;                         ("select-window-\\([0-9]\\)" . "window \\1")
  ;;                         ("evil-ace-jump-line-mode" . "jump line")
  ;;                         ("evil-ace-jump-word-mode" . "jump word")
  ;;                         ("ace-jump-mode-pop-mark" . "jump back")
  ;;                         ("er/expand-region" . "expand region")
  ;;                         ("\\(.*\\)-micro-state\\(-?.*\\)" . "\\1-ms\\2"))
  ;;          do (cl-pushnew entry which-key-description-replacement-alist
  ;;                         :key #'car :test #'string=))

  ;; window-purpose
  (with-eval-after-load 'window-purpose
    (cl-pushnew '(conf-mode . edit) purpose-user-mode-purposes :key #'car)
    (cl-pushnew '("\\.log$" . log) purpose-user-regexp-purposes
                :key #'car :test #'equal)
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
  ;; (defun auto-fit-imenu-list (&optional window)
  ;;   (when (string= (buffer-name (window-buffer window)) "*Ilist*")
  ;;     (let ((fit-window-to-buffer-horizontally t))
  ;;       (fit-window-to-buffer window))))
  ;; (add-hook 'purpose-display-buffer-functions #'auto-fit-imenu-list)

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

  (setcdr evil-insert-state-map nil)
  (define-key evil-insert-state-map [escape] #'evil-normal-state)
  (define-key evil-insert-state-map (kbd "f") #'evil-escape-insert-state)
  (define-key evil-insert-state-map (kbd "M-m") evil-leader--default-map)

  (evil-leader/set-key
    "SPC" 'avy-goto-word-or-subword-1
    "l" 'avy-goto-line
    "`" 'pop-to-mark-command
    "~" 'pop-global-mark)
  (evil-leader/set-key "ot" 'toggle-tabs-mode)
  ;; (evil-leader/set-key-for-mode 'python-mode
  ;;   "mhj" 'jump-do-anaconda-view-doc
  ;;   "mhr" 'jump-do-anaconda-usages)
  (evil-leader/set-key "ob" 'my-switch-buffer)

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
    (let ((base "#b2b2b2")
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
       `(diff-hl-delete ((t (:foreground ,war :background "#361800"))))
       `(diff-hl-insert ((t (:foreground ,green :background "#003618"))))
       `(diff-hl-change ((t (:foreground ,inf :background "#001836"))))
       `(helm-visible-mark ((t (:foreground ,green :background ,bg4))))
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

(with-eval-after-load 'helm-source
  (with-eval-after-load 'smex
    (defvar helm-smex-source--cache (make-hash-table :test #'eq))
    (defvar helm-smex-source--candidates nil)
    (defun helm-smex-source--smex-score-no-cache (command)
      (or (cdr (car (cl-member (symbol-name command) smex-cache :key #'car :test #'string=)))
          0))
    (defun helm-smex-source--smex-score (command)
      (or (gethash command helm-smex-source--cache)
          (puthash command
                   (helm-smex-source--smex-score-no-cache command)
                   helm-smex-source--cache)))
    (defun helm-smex-source--sort-by-smex (cand1 cand2)
      (> (helm-smex-source--smex-score (intern-soft cand1))
         (helm-smex-source--smex-score (intern-soft cand2))))
    (defclass helm-smex-source (helm-source-sync)
      ((init :initform (lambda ()
                         ;; (smex-rebuild-cache)
                         (clrhash helm-smex-source--cache)
                         (setq helm-smex-source--candidates (smex-convert-for-ido smex-cache))))
       (candidates :initform 'helm-smex-source--candidates)
       (match :initform #'helm-fuzzy-match)
       (filtered-candidate-transformer
        :initform (lambda (candidates source)
                    (sort candidates #'helm-smex-source--sort-by-smex)))))
    (defun helm-smex ()
      (interactive)
      (let ((command-name (helm :buffer "*helm-smex*" :sources (helm-make-source "Smex" 'helm-smex-source))))
        (when command-name
          (unwind-protect
              (execute-extended-command current-prefix-arg command-name)
            (smex-rank (intern-soft command-name))))))
    ;; (evil-leader/set-key ":" #'helm-smex)
    ;; (global-set-key (kbd "M-x") #'helm-smex)
    ))
;; ;;; Source: https://github.com/wasamasa/dotemacs/blob/master/unpublished/helm-smex.el
;; (defun helm-smex-items ()
;;   (smex-rebuild-cache)
;;   (smex-convert-for-ido smex-cache))

;; (defun helm-smex-execute-command (command)
;;   (let ((prefix-arg current-prefix-arg))
;;     (command-execute command 'record)
;;     (smex-rank command)))

;; (setq helm-smex-source
;;       '((name . "M-x")
;;         (candidates . helm-smex-items)
;;         (coerce . intern)
;;         (action ("smex" . (helm-smex-execute-command)))))

;; (defun helm-smex ()
;;   (interactive)
;;   (helm :sources 'helm-smex-source :buffer "*helm-smex*"))
;; ;;; end of source


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
 ;; '(default ((t (:family "Source Code Pro" :foundry "adobe" :slant normal :weight normal :height 92 :width normal))))
 '(company-tooltip-common ((t (:inherit company-tooltip :weight bold :underline nil))))
 '(company-tooltip-common-selection ((t (:inherit company-tooltip-selection :weight bold :underline nil)))))
