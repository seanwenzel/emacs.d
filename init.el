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
 confirm-kill-processes nil             ; Don't have to confirm to kill running processes
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
 )

;; Set default font
(set-face-attribute 'default nil
                    :family "Fira Code"
                    :height 130
                    :weight 'normal
                    :width 'normal)

;; Show full path of file in frame title
(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))

;; Delete trailing whitepsace on save
(add-hook 'before-save-hook 'whitespace-cleanup)

;; Revert (update) buffers automatically when underlying files are changed externally
(global-auto-revert-mode t)

;; Enable subword movement and editing
(global-subword-mode t)

;; Highlight current line
(global-hl-line-mode t)

;; Always show matching parens
(show-paren-mode t)

(recentf-mode 1)
(setq recentf-max-saved-items 1000)

;; Show line and col numbers + size in modeline
(line-number-mode t)
(column-number-mode t)
(size-indication-mode t)

;; Interpret ansi escape sequences in log files
(require 'ansi-color)
(defun display-ansi-colors ()
  (interactive)
  (ansi-color-apply-on-region (point-min) (point-max)))
(add-to-list 'auto-mode-alist '("\\.log\\'" . display-ansi-colors))

(require 'package)
;; (setq package-enable-at-startup nil)
(setq package-archives '(("org"   . "http://orgmode.org/elpa/")
                         ("gnu"   . "http://elpa.gnu.org/packages/")
                         ("melpa" . "http://melpa.org/packages/")
                         ("melpa-stable" . "https://stable.melpa.org/packages/"))
      )
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-and-compile
  ;; (setq use-package-always-ensure t)         ; Always assume the :ensure tag is set to t
  (setq use-package-expand-minimally t)      ; Make expanded code as minimal as possible
  (setq use-package-enable-imenu-support t)) ; Allow imenu to see use-package declarations

(require 'use-package)

(use-package diminish
  :ensure t)

(use-package vlf
  :ensure t)

(use-package doom-themes
  :ensure t
  :demand
  :config
  (doom-themes-org-config)
  (load-theme 'doom-monokai-pro t))

(use-package all-the-icons
  :ensure t
  )
(use-package doom-modeline
  :ensure t
  :after all-the-icons
  :config
  (doom-modeline-mode))

(use-package editorconfig
  :ensure t
  :config
  (editorconfig-mode 1))

(use-package smooth-scrolling
  :ensure t
  :config
  (smooth-scrolling-mode 1)
  )

(use-package leetcode
  :ensure t
  :config
  (setq leetcode-prefer-language "python3")
  (setq leetcode-save-solutions t)
  (setq leetcode-directory "~/leetcode")
  )

;; load evil
(use-package evil
  :ensure t
  ;; install the evil package if not installed
  :init ;; tweak evil's configuration before loading it
  ;; (setq evil-search-module 'evil-search)
  (setq evil-ex-search-case 'insensitive)
  ;; (setq evil-ex-complete-emacs-commands nil)
  (setq evil-vsplit-window-right t)
  (setq evil-split-window-below t)
  ;; (setq evil-shift-round nil)
  (setq evil-want-C-u-scroll t)
  ;; (setq evil-want-integration t) ;; This is optional since it's already set to t by default.
  (setq evil-want-keybinding nil)
  :config ;; tweak evil after loading it

  (setq evil-default-state 'normal)

  ;; (evil-set-initial-state 'prog-mode 'normal)
  ;; (evil-set-initial-state 'text-mode 'normal)
  ;; (evil-set-initial-state 'fundamental-mode 'normal)

  (add-hook 'with-editor-mode-hook 'evil-insert-state) ;; Magit git commit buffer

  ;; Use regular emacs keybindings for insert-mode (except for ESC-ESC-ESC,
  ;; because vim keybindings are still vim).

  (setq evil-insert-state-map (make-sparse-keymap))
  (define-key evil-insert-state-map (kbd "<escape>") 'evil-normal-state)

  (evil-set-undo-system 'undo-redo)

  (evil-mode))

(use-package evil-collection
  :ensure t
  :after evil
  :config
  (evil-collection-init))

;; gc operator, like vim-commentary
(use-package evil-commentary
  :ensure t
  :after evil
  :config
  (evil-commentary-mode))

;; like vim-surround
(use-package evil-surround
  :ensure t
  :config
  (global-evil-surround-mode 1))

(use-package evil-snipe
  :ensure t
  :config
  (setq evil-snipe-scope 'buffer)
  (evil-snipe-mode +1)

  ;; This interferes with custom semi-colon mapping in general
  ;; Disable for now, revisit later
  ;; (evil-snipe-override-mode +1)
  )

;; more text objectss
(use-package evil-args
  :ensure t
  :after evil
  :init
  (progn
    ;; bind evil-args text objects
    (define-key evil-inner-text-objects-map "a" 'evil-inner-arg)
    (define-key evil-outer-text-objects-map "a" 'evil-outer-arg)))

(use-package rainbow-delimiters
  :ensure t
  :config
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
  )

(use-package yasnippet
  :ensure t
  :defer 1
  :config (yas-global-mode))

(use-package yasnippet-snippets
  :ensure t
  :after yasnippet
  :config (yasnippet-snippets-initialize))

(use-package ivy-yasnippet
  :ensure t
  )

(use-package flycheck
  :ensure t
  :diminish flycheck-mode
  ;; :config
  ;; (add-hook 'after-init-hook #'global-flycheck-mode)
  )

(use-package restart-emacs

  :ensure t
  )

(use-package exec-path-from-shell
  :ensure t
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)
    ))

(use-package add-node-modules-path
  :config
  (add-hook 'js-mode-hook #'add-node-modules-path))

(use-package projectile
  :ensure t
  :config
  (projectile-mode +1)
  (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))

(use-package company
  :ensure t
  :diminish company-mode
  :config
  (define-key company-active-map (kbd "M-n") nil)
  (define-key company-active-map (kbd "M-p") nil)
  (define-key company-active-map (kbd "C-n") #'company-select-next)
  (define-key company-active-map (kbd "C-p") #'company-select-previous)
  (define-key company-active-map (kbd "C-j") #'company-select-next)
  (define-key company-active-map (kbd "C-k") #'company-select-previous)
  ;; Set delay in showing suggestions (0.5 by default)
  (setq company-idle-delay 0.5)

  ;; Start showing suggestions after just 1 character (3 by default)
  (setq company-minimum-prefix-length 1)

  (add-hook 'after-init-hook #'global-company-mode))

(use-package avy
  :ensure t
  :commands (avy-goto-word-1))

(use-package ivy
  :ensure t
  :diminish
  :init
  (use-package counsel :diminish :config (counsel-mode 1))
  (use-package swiper :defer t)
  (use-package orderless)
  (ivy-mode 1)
  :custom
  (ivy-use-virtual-buffers t)
  (ivy-height 10)
  (ivy-on-del-error-function nil)
  (ivy-magic-slash-non-match-action 'ivy-magic-slash-non-match-create)
  (ivy-count-format "【%d/%d】")
  (ivy-wrap t)
  :config
  (setq ivy-re-builders-alist '((t . orderless-ivy-re-builder)))
  (setq ivy-initial-inputs-alist nil)
  (defun counsel-goto-local-home ()
    "Go to the $HOME of the local machine."
    (interactive)
    (ivy--cd "~/")))

(use-package magit
  :ensure t
  :bind ("C-x g" . magit-status))

;; Forge install fails for some reason
(use-package forge
  :ensure t
  :after magit)

(use-package diff-hl
  :ensure t
  :config
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
  (global-diff-hl-mode))

(use-package which-key
  :ensure t
  :diminish which-key-mode
  :config
  (which-key-mode))

(use-package writeroom-mode
  :ensure t
  :config
  (setq writeroom-width 100)
  )

(use-package prettier-js
  :ensure t
  :config
  (add-hook 'js-mode-hook 'prettier-js-mode))

(use-package lua-mode
  :ensure t)

;; (use-package elisp-mode
;;   ;;this is a built in package, so we don't want to try and install it
;;   :ensure nil
;;   :general
;;   (global-leader
;;     ;;specify the major modes these should apply to:
;;     :major-modes
;;     '(emacs-lisp-mode lisp-interaction-mode t)
;;     ;;and the keymaps:
;;     :keymaps
;;     '(emacs-lisp-mode-map lisp-interaction-mode-map)
;;     "e" '(:ignore t :which-key "eval")
;;     "eb" 'eval-buffer
;;     "ed" 'eval-defun
;;     "ee" 'eval-expression
;;     "ep" 'pp-eval-last-sexp
;;     "es" 'eval-last-sexp
;;     "i" 'elisp-index-search))

(use-package robot-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.robot\\'" . robot-mode)))

(use-package markdown-mode
  :ensure t
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

(use-package json-mode
  :ensure t
  )

(use-package python-black
  :ensure t
  :after python
  )

(use-package py-yapf
  :ensure t
  :after python)

(use-package docker
  :ensure t
  :bind ("C-c d" . docker))

(use-package dockerfile-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode))
  )

(use-package cmake-mode
  :ensure t
  )

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

;; toggle profiler (taken from Doom)
(defvar doom--profiler nil)
;;;###autoload
(defun doom/toggle-profiler ()
  "Toggle the Emacs profiler. Run it again to see the profiling report."
  (interactive)
  (if (not doom--profiler)
      (profiler-start 'cpu+mem)
    (profiler-report)
    (profiler-stop))
  (setq doom--profiler (not doom--profiler)))

(use-package general
  :ensure t
  :config
  (general-evil-setup)

  ;; from https://gist.github.com/progfolio/1c96a67fcec7584b31507ef664de36cc
  (defmacro +general-global-menu! (name infix-key &rest body)
    "Create a definer named +general-global-NAME wrapping global-definer.
Create prefix map: +general-global-NAME. Prefix bindings in BODY with INFIX-KEY."
    (declare (indent 2))
    `(progn
       (general-create-definer ,(intern (concat "+general-global-" name))
         :wrapping global-definer
         :prefix-map (quote ,(intern (concat "+general-global-" name "-map")))
         :infix ,infix-key
         :wk-full-keys nil
         "" '(:ignore t :which-key ,name))
       (,(intern (concat "+general-global-" name))
        ,@body)))

  ;; swap ; and :
  (general-swap-key nil 'motion
    ";" ":")

  ;; (defconst my-leader "SPC")
  ;; (defconst my-local-leader ",")

  ;; (general-create-definer my-leader-def
  ;;   :prefix my-leader)

  ;; (general-create-definer my-local-leader-def
  ;;   :prefix my-local-leader)

  (general-create-definer global-definer
    :keymaps 'override
    :states  '(insert emacs normal hybrid motion visual operator)
    :prefix  "SPC"
    :non-normal-prefix "M-m")

  ;; For per-major mode bindings
  (general-create-definer global-leader
  :keymaps 'override
  :states '(emacs normal hybrid motion visual operator)
  :prefix "SPC m"
  "" '(:ignore t :which-key (lambda (arg) `(,(cadr (split-string (car arg) " ")) . ,(replace-regexp-in-string "-mode$" "" (symbol-name major-mode))))))

  (+general-global-menu! "buffer" "b"
    "b"  'counsel-switch-buffer
    "d"  'kill-current-buffer
    "o" '((lambda () (interactive) (switch-to-buffer nil))
          :which-key "other-buffer")
    "p"  'previous-buffer
    "r"  'rename-buffer
    "M" '((lambda () (interactive) (switch-to-buffer "*Messages*"))
          :which-key "messages-buffer")
    "n"  'next-buffer
    "s" '((lambda () (interactive) (switch-to-buffer "*scratch*"))
          :which-key "scratch-buffer"))

  (+general-global-menu! "files" "f"
    "ed" 'edit-config-file
    "er" 'reload-emacs-configuration
    "f" 'counsel-find-file
    "g" 'counsel-git
    "s" 'save-buffer
    "r" 'counsel-recentf
    )

  (+general-global-menu! "git" "g"
    "s" 'magit-status
    "B" 'magit-blame
    "c" 'magit-clone
    )

  (+general-global-menu! "help" "h"
    "f" 'describe-function
    "g" 'general-describe-keybindings
    "k" 'describe-key
    "p" 'describe-package
    "v" 'describe-variable
    "m" 'describe-mode
    )

  (+general-global-menu! "jump" "j"
    "i" 'counsel-imenu
    "l" 'evil-avy-goto-line
    "w" 'evil-avy-goto-word-1
    "j" 'evil-avy-goto-char-timer
    )


  (+general-global-menu! "window" "w"
    "l" 'evil-window-right
    "k" 'evil-window-up
    "j" 'evil-window-down
    "h" 'evil-window-left
    "o" 'delete-other-windows
    "m" 'delete-other-windows
    "v" 'evil-window-vsplit
    "s" 'evil-window-split
    "q" 'evil-quit
    "d" 'evil-quit
    "w" 'ace-window)

  (+general-global-menu! "project" "p"
    "p" 'projectile-switch-project
    "f" 'projectile-find-file
    "d" 'projectile-dired
    "t" 'projectile-test-project
    )

  (+general-global-menu! "toggle" "t"
    "n" '(display-line-numbers-mode :which-key "line-numbers")
    "z" 'writeroom-mode
    "p" 'doom/toggle-profiler
    )


  ;; Global Keybindings
  (global-definer
    ;; simple command
    "/"   'counsel-ag
    "'"   'vterm
    "="   'format-all-buffer
    "SPC" 'counsel-M-x

    ;; Documentation
    "dd" 'dash-at-point

    ;; Errors
    "e" '(:ignore t :which-key "errors")
    "el" 'flycheck-list-errors

    ;; Insert
    "i" '(:ignore t :which-key "insert")
    "is" 'ivy-yasnippet

    ;; Open
    "o" '(:ignore o :which-key "open")
    "od" 'docker
    "ol" 'leetcode

    ;; Quit
    "q" '(:ignore t :which-key "quit")
    "qq" '(save-buffers-kill-terminal :which-key "quit")
    "qr" '(restart-emacs :which-key "restart")

    ;; Search
    "s" '(:ignore t :which-key "search")
    "sl" 'swiper
    )
  )



(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(prettier-js lua-mode cmake-mode dockerfile-mode dockerfile py-yapf python-black evil-snipe vlf docker projectile orderless forge json-mode markdown-mode js2-refactor rainbow-delimiters objed smooth-scrolling yasnippet-snippets writeroom-mode which-key use-package robot-mode restart-emacs magit leetcode ivy-yasnippet general flycheck exec-path-from-shell evil-surround evil-commentary evil-collection evil-args editorconfig doom-themes doom-modeline diminish diff-hl counsel company avy add-node-modules-path)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
