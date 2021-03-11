;; A big contributor to startup times is garbage collection. We up the gc
;; threshold to temporarily prevent it from running, and then reset it later
;; using a hook.
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

;; Keep a ref to the actual file-name-handler
(defvar default-file-name-handler-alist file-name-handler-alist)

;; Set the file-name-handler to nil (because regexing is cpu intensive)
(setq file-name-handler-alist nil)

;; Reset file-name-handler-alist after initialization
(add-hook 'emacs-startup-hook
  (lambda ()
    (setq gc-cons-threshold 16777216
          gc-cons-percentage 0.1
          file-name-handler-alist default-file-name-handler-alist)))

(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

(defalias 'yes-or-no-p 'y-or-n-p)

(when window-system
  (menu-bar-mode -1)
  (toggle-scroll-bar -1)
  (tool-bar-mode -1)
  (blink-cursor-mode -1))

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(setq-default
 ;; General Defaults
 confirm-kill-emacs nil                 ; Don't have to confirm to kill emacs
 confirm-kill-processes t               ; Do have to confirm to kill running processes
 cursor-in-non-selected-windows nil     ; Hide the cursor in inactive windows
 delete-by-moving-to-trash nil          ; Delete files without moving to trash
 fill-column 80                         ; Set width for automatic line breaks
 indent-tabs-mode nil                   ; Stop using tabs to indent
 inhibit-startup-message t              ; Don't show the startup message
 inhibit-startup-screen t               ; inhibit useless and old-school startup screen
 initial-scratch-message nil            ; Empty scratch buffer
 line-spacing nil                       ; I sometimes like some line spacing
 ring-bell-function 'ignore             ; silent bell when you make a mistake
 sentence-end-double-space nil          ; End a sentence after a dot and a space
 show-trailing-whitespace t             ; Display trailing whitespaces
 tab-width 4                            ; Set width for tabs
 x-stretch-cursor t                     ; Stretch cursor to the width of the underlying glyph

 ;; Backup File Defaults
 auto-save-default nil                  ; stop creating #autosave# files
 create-lockfiles nil                   ; stop creating .# files
 make-backup-files nil                  ; stop creating backup~ files

 ;; Smooth Scrolling Defaults (from M-Emacs)
 scroll-step 1
 scroll-margin 1                        ; Add a line margin when scrolling vertically
 scroll-conservatively 101
 scroll-up-aggressively 0.01
 scroll-down-aggressively 0.01
 auto-window-vscroll nil
 fast-but-imprecise-scrolling nil
 mouse-wheel-scroll-amount '(1 ((shift) . 1))
 mouse-wheel-progressive-speed nil
 hscroll-step 1
 hscroll-margin 1
 )

;; Show full path of file in frame title
(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))

;; (set-frame-font "Fira Code 12" nil t)

;; Delete trailing whitepsace on save
(add-hook 'before-save-hook 'whitespace-cleanup)

;; Revert (update) buffers automatically when underlying files are changed externally
(global-auto-revert-mode t)

;; Enable subword movement and editing
(global-subword-mode t)

;; Highlight current line
(global-hl-line-mode t)

;; Show line and col numbers + size in modeline
(line-number-mode t)
(column-number-mode t)
(size-indication-mode t)

(require 'package)
(setq package-enable-at-startup nil)
(setq package-archives '(("org"   . "http://orgmode.org/elpa/")
                         ("gnu"   . "http://elpa.gnu.org/packages/")
                         ("melpa" . "http://melpa.org/packages/")))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-and-compile
  (setq use-package-always-ensure t)         ; Always assume the :ensure tag is set to t
  (setq use-package-expand-minimally t)      ; Make expanded code as minimal as possible
  (setq use-package-enable-imenu-support t)) ; Allow imenu to see use-package declarations

(eval-when-compile
  (require 'use-package))

(use-package diminish)

(use-package doom-themes
  :demand
  :config
  (doom-themes-org-config)
  (load-theme 'doom-monokai-pro t))

(use-package editorconfig
  :ensure t
  :config
  (editorconfig-mode 1))

(use-package undo-tree
  :config
  (global-undo-tree-mode))

;; load evil
(use-package evil
  :after undo-tree
  ;; install the evil package if not installed
  :init ;; tweak evil's configuration before loading it
  (setq evil-search-module 'evil-search)
  (setq evil-ex-search-case 'insensitive)
  (setq evil-ex-complete-emacs-commands nil)
  (setq evil-vsplit-window-right t)
  (setq evil-split-window-below t)
  (setq evil-shift-round nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-integration t) ;; This is optional since it's already set to t by default.
  (setq evil-want-keybinding nil)
  :config ;; tweak evil after loading it

  (setq evil-default-state 'normal)
  (add-hook 'with-editor-mode-hook 'evil-insert-state) ;; Magit git commit buffer
  ;; Use regular emacs keybindings for insert-mode (except for ESC-ESC-ESC,
  ;; because vim keybindings are still vim).
  (setq evil-insert-state-map (make-sparse-keymap))
  (define-key evil-insert-state-map (kbd "<escape>") 'evil-normal-state)

  (evil-set-undo-system 'undo-tree)

  (evil-mode))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

;; gc operator, like vim-commentary
(use-package evil-commentary
  :after evil
  :config
  (evil-commentary-mode))

;; like vim-surround
(use-package evil-surround
  :config
  (global-evil-surround-mode 1))

;; more text objectss
(use-package evil-args
  :after evil
  :init
  (progn
    ;; bind evil-args text objects
    (define-key evil-inner-text-objects-map "a" 'evil-inner-arg)
    (define-key evil-outer-text-objects-map "a" 'evil-outer-arg)))

(use-package yasnippet
  :defer 1
  :config (yas-global-mode))

(use-package yasnippet-snippets
  :after yasnippet
  :config (yasnippet-snippets-initialize))

(use-package ivy-yasnippet)

(use-package flycheck
  :diminish flycheck-mode
  :config
  (add-hook 'after-init-hook #'global-flycheck-mode)
  )

(use-package restart-emacs)

(use-package exec-path-from-shell
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

(use-package add-node-modules-path
  :config
  (add-hook 'js2-mode #'add-node-modules-path)
  (add-hook 'js2-jsx-mode #'add-node-modules-path))

(use-package company
  :diminish company-mode
  :config
  (define-key company-active-map (kbd "M-n") nil)
  (define-key company-active-map (kbd "M-p") nil)
  (define-key company-active-map (kbd "C-n") #'company-select-next)
  (define-key company-active-map (kbd "C-p") #'company-select-previous)
  (define-key company-active-map (kbd "C-j") #'company-select-next)
  (define-key company-active-map (kbd "C-k") #'company-select-previous)
  ;; Set delay in showing suggestions (0.5 by default)
  (setq company-idle-delay 0.35)

  ;; Start showing suggestions after just 1 character (3 by default)
  (setq company-minimum-prefix-length 1)

  (add-hook 'after-init-hook #'global-company-mode))

(use-package avy
  :commands (avy-goto-word-1))

(use-package ivy
  :diminish
  :init
  (use-package counsel :diminish :config (counsel-mode 1))
  (use-package swiper :defer t)
  (ivy-mode 1)
  :custom
  (ivy-use-virtual-buffers t)
  (ivy-height 10)
  (ivy-on-del-error-function nil)
  (ivy-magic-slash-non-match-action 'ivy-magic-slash-non-match-create)
  (ivy-count-format "【%d/%d】")
  (ivy-wrap t)
  :config
  (setq ivy-initial-inputs-alist nil)
  (defun counsel-goto-local-home ()
    "Go to the $HOME of the local machine."
    (interactive)
    (ivy--cd "~/")))

(use-package magit
  :bind ("C-x g" . magit-status))

(use-package diff-hl
  :config
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
  (global-diff-hl-mode))

(use-package which-key
  :diminish which-key-mode
  :config
  (which-key-mode))

(use-package writeroom-mode
  :config
  (setq writeroom-width 100)
  )

(use-package robot-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.robot\\'" . robot-mode)))

(defun edit-config-file ()
  "Open the init file."
  (interactive)
  (find-file user-init-file))

;; Helper function to center buffer after jump
(defun recenter-no-redraw (&optional arg)
  "Like `recenter', but no redrawing."
  (interactive "P")
  (let ((recenter-redisplay nil))
    (recenter arg)))

;; Center buffer after imenu jumps
(advice-add 'imenu :after (lambda (&rest args)
                                    (recenter-no-redraw)))

;; Not sure if this will work with js-mode
;; (use-package js2-refactor
;;   :config
;;   (add-hook 'js2-mode-hook #'js2-refactor-mode))

(use-package general
  :config
  (general-evil-setup)

  ;; swap ; and :
  (general-swap-key nil 'motion
    ";" ":")

  (defconst my-leader "SPC")
  (defconst my-local-leader ",")
  (general-create-definer my-leader-def
    :prefix my-leader)

  (general-create-definer my-local-leader-def
    :prefix my-local-leader)

  ;; Global Keybindings
  (my-leader-def
    :states '(normal visual motion)

    ;; This prevents leader key from ever being overriden (e.g. an evil
    ;; package may bind "SPC")
    :keymaps 'override

    ;; simple command
    "/"   'counsel-ag
    "'"   'vterm
    "="   'format-all-buffer
    "SPC" 'counsel-M-x

    ;; Buffers
    "b" '(:ignore t :which-key "buffers")
    "bb" 'counsel-switch-buffer
    "bp" 'evil-prev-buffer
    "bn" 'evil-next-buffer
    "bd" 'evil-delete-buffer

    ;; Documentation
    "dd" 'dash-at-point

    ;; Errors
    "e" '(:ignore t :which-key "errors")
    "el" 'flycheck-list-errors

    ;; Files
    "f" '(:ignore t :which-key "files")
    "fed" 'edit-config-file
    "fer" 'reload-emacs-configuration
    "ff" 'counsel-find-file
    "fg" 'counsel-git
    "fs" 'save-buffer
    "fr" 'counsel-recentf
    "ft" 'treemacs

    ;; Git
    "g" '(:ignore t :which-key "git")
    "gs" 'magit-status
    "gB" 'magit-blame

    ;; Help
    "h" '(:ignore t :which-key "help")
    "hf" 'describe-function
    "hg" 'general-describe-keybindings
    "hk" 'describe-key
    "hp" 'describe-package
    "hv" 'describe-variable
    "hm" 'describe-mode

    ;; Insert
    "i" '(:ignore t :which-key "insert")
    "is" 'ivy-yasnippet

    ;; Jump
    "j" '(:ignore t :which-key "jump")
    "ji" 'counsel-imenu
    "jl" 'evil-avy-goto-line
    "jw" 'evil-avy-goto-word-1
    "jj" 'evil-avy-goto-char-timer

    ;; Open
    "o" '(:ignore o :which-key "open")
    "od" 'docker
    "ol" 'leetcode

    ;; Projectile
    "p" '(:ignore t :which-key "project")
    "pp" 'project-switch-project
    "pf" 'project-find-file
    "pd" 'project-dired

    ;; Profiler
    "pb" 'profiler-start ;; Profile Begin
    "pr" 'profiler-report ;; Profile Report
    "pe" 'profiler-stop ;; Profile End

    ;; Quit
    "q" '(:ignore t :which-key "quit")
    "qq" '(save-buffers-kill-terminal :which-key "quit")
    "qr" '(restart-emacs :which-key "restart")

    ;; Search
    "s" '(:ignore t :which-key "search")
    "sl" 'swiper
    "sdg" 'deadgrep
    "sap" 'counsel-projectile-ag
    "srp" 'counsel-projectile-rg
    ;; "srg" 'projectile-ripgrep

    ;; Toggle 1
    "t" '(:ignore t :which-key "toggle")
    "tn" '(display-line-numbers-mode :which-key "line-numbers")
    "tz" 'writeroom-mode

    ;; Window
    "w" '(:ignore t :which-key "window")
    "wl" 'evil-window-right
    "wk" 'evil-window-up
    "wj" 'evil-window-down
    "wh" 'evil-window-left
    "wo" 'delete-other-windows
    "wm" 'delete-other-windows
    "wv" 'evil-window-vsplit
    "ws" 'evil-window-split
    "wq" 'evil-quit
    "wd" 'evil-quit
    "ww" 'ace-window
    )

  ;; TODO: Mode Keybindings
  (my-local-leader-def
    :states '(normal visual)
    :keymaps 'org-mode-map
    "sn" 'org-narrow-to-subtree
    "sN" 'widen
    )
  )

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(undo-tree evil-collection editorconfig robot-mode diminish writeroom-mode avy restart-emacs add-node-modules-path exec-path-from-shell flycheck ivy-yasnippet yasnippet-snippets yasnippet evil-args evil-surround evil-commentary which-key evil doom-modeline doom-themes use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
