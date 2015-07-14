;; -*- mode: emacs-lisp -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

(defun dotspacemacs/layers ()
  "Configuration Layers declaration."
  (setq-default
   dotspacemacs-configuration-layer-path '()
   dotspacemacs-configuration-layers
   `(
     auto-completion
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
     python
     php
     markdown
     org
     ;; sql

     slime
     (shell :variables shell-default-shell 'shell)
     gtags
     cscope

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
     )
   dotspacemacs-additional-packages '(sr-speedbar window-purpose imenu-list let-alist f)
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
   dotspacemacs-guide-key-delay 0.4
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

  ;; comint
  (with-eval-after-load 'comint
    (define-key comint-mode-map (kbd "M-p") #'comint-previous-matching-input-from-input)
    (define-key comint-mode-map (kbd "M-n") #'comint-next-matching-input-from-input)
    (define-key comint-mode-map (kbd "C-c M-r") #'comint-previous-input)
    (define-key comint-mode-map (kbd "C-c M-s") #'comint-previous-input))

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
    (purpose-set-extension-configuration
     :popup-repls
     (purpose-conf "popup-repls"
                   :mode-purposes '((inferior-python-mode . repl)
                                    (inferior-emacs-lisp-mode . repl))))
    (purpose-x-popupify-purpose 'repl)

    (defun dedicate-repl (window)
      (when (eql (purpose-window-purpose window) 'repl)
        (purpose-set-window-purpose-dedicated-p window t)))
    (add-hook 'purpose-display-buffer-functions #'dedicate-repl)
    (setq purpose-display-at-bottom-height 0.25)

    (defun open-recent-repl ()
      "Select open REPL window, or pop to recently used REPL."
      (interactive)
      (let ((window (car (purpose-windows-with-purpose 'repl))))
        (if window
            (select-window window)
          (let ((buffer (car (purpose-buffers-with-purpose 'repl))))
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
        "m'" #'delete-window)))


  ;; ycmd
  ;; (setq ycmd-server-command `("python" ,(expand-file-name "~/src/ycmd/ycmd/__main__.py")))

  (setq frame-title-format "Spacemacs")

  ;; dired
  (setq dired-guess-shell-alist-user
        '(("\\.pdf\\'" "evince")
          ("\\.ods\\'\\|\\.xlsx?\\'\\|\\.docx?\\'\\|\\.csv\\'" "libreoffice")
          ("\\.jpg\\'" "gpicview")))


  ;; window-purpose
  (with-eval-after-load 'window-purpose
    (cl-pushnew '(conf-mode . edit) purpose-user-mode-purposes :key #'car)
    (cl-pushnew '("\\.log$" . log) purpose-user-regexp-purposes
                :key #'car :test #'equal)
    (purpose-compile-user-configuration)

    (when (require 'window-purpose-x nil t)
      (purpose-x-magit-single-on)))

  ;; eyebrowse
  (with-eval-after-load 'eyebrowse
    (dolist (index (mapcar #'number-to-string '(0 1 2 3 4 5 6 7 8 9)))
      (define-key eyebrowse-mode-map
        (kbd (concat "M-" index))
        (intern-soft (concat "eyebrowse-switch-to-window-config-" index)))
      (define-key window-numbering-keymap
        (kbd (concat "M-" index)) nil)))

  ;; imenu-list
  (global-set-key (kbd "C-`") #'imenu-list-minor-mode)
  (add-to-list 'evil-motion-state-modes 'imenu-list-major-mode)
  (with-eval-after-load 'imenu-list
    (evil-define-key 'motion imenu-list-major-mode
      "s" #'hs-toggle-hiding
      "d" #'imenu-list-display-entry
      "q" #'imenu-list-minor-mode))

  ;; flycheck
  (add-to-list 'evil-motion-state-modes 'flycheck-error-list-mode)
  (with-eval-after-load 'flycheck
    (evil-define-key 'motion flycheck-error-list-mode-map
      "j" #'flycheck-error-list-next-error
      "k" #'flycheck-error-list-previous-error))

  ;; anaconda
  (add-to-list 'evil-motion-state-modes 'anaconda-nav-mode)
  (with-eval-after-load 'anaconda-mode
    (evil-define-key 'motion anaconda-nav-mode-map
      "j" #'next-error
      "k" #'previous-error
      "q" #'anaconda-nav-quit))

  (defun toggle-tabs-mode ()
    (interactive)
    (setq indent-tabs-mode (not indent-tabs-mode)))

  (with-eval-after-load 'helm
    (define-key helm-map (kbd "C-M-h") #'help-command))

  (setcdr evil-insert-state-map nil)
  (define-key evil-insert-state-map [escape] #'evil-normal-state)
  (define-key evil-insert-state-map (kbd "f") #'evil-escape-insert-state)
  (define-key evil-insert-state-map (kbd "M-m") evil-leader--default-map)

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
