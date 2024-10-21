;; Interactively do things.
(fido-vertical-mode 0)
(ido-mode 0)

;; Complete pairs
(electric-pair-mode 0)

(blink-cursor-mode 0)

(pixel-scroll-precision-mode)

(global-visual-line-mode)

;; Show stray whitespace.
(setq-default indicate-empty-lines t)
(setq-default indicate-buffer-boundaries 'left)
(setq-default require-final-newline t)

(setq load-prefer-newer t)

;; Remove message in scratch buffer.
(setq-default initial-scratch-message nil)

;; Add a newline automatically at the end of a file while saving.
(setq-default require-final-newline t)

;; Consider a period followed by a single space to be end of sentence.
(setopt sentence-end-double-space nil)

(setq-default insert-directory-program "gls")

;; Use spaces, not tabs, for indentation.
(setq-default indent-tabs-mode nil)

;; Display the distance between two tab stops as 4 characters wide.
(setq-default tab-width 4)

(setq-default enable-recursive-minibuffers t)

(setq-default view-read-only t)

;; Indentation setting for various languages.
(setopt c-basic-offset 4)
(setopt js-indent-level 2)
(setopt typescript-indent-level 2)
(setopt css-indent-offset 2)

;; Highlight matching pairs of parentheses.
(setopt show-paren-delay 0)
(show-paren-mode)

;; Write auto-saves and backups to separate directory.
(make-directory "~/.tmp/emacs/auto-save/" t)
(setopt auto-save-file-name-transforms '((".*" "~/.tmp/emacs/auto-save/" t)))
(setopt backup-directory-alist '(("." . "~/.tmp/emacs/backup/")))

;; Do not move the current file while creating backup.
(setopt backup-by-copying t)

;; Disable lockfiles.
(setopt create-lockfiles nil)

(desktop-save-mode 1)
(setopt user-emacs-directory "~/.config/emacs/")
(setopt desktop-path '("~/.config/emacs/desktops/"))

(setopt nnrss-directory (expand-file-name "news/rss" user-emacs-directory))

(setopt shell-file-name "/opt/homebrew/bin/fish")
(setopt explicit-shell-file-name "/opt/homebrew/bin/fish")
(setopt vterm-shell "/opt/homebrew/bin/fish")

(setopt scroll-conservatively 10)
(setopt scroll-margin 1)

(tab-bar-mode 1)

(setopt diff-hl-show-staged-changes nil)

(setopt switch-to-buffer-obey-display-actions t)

;; Typed text replaces the selection if typed text replaces the
;; selection if the selection is active
(delete-selection-mode 1)

(setopt user-full-name       "Wadii Hajji"
        user-real-login-name "Wadii Hajji"
        user-login-name      "hwadii"
        user-mail-address    "hajji.wadii@yahoo.com")

(global-set-key [remap list-buffers] 'ibuffer)
(global-set-key [remap dabbrev-expand] 'hippie-expand)

;; Enable installation of packages from MELPA.
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(eval-and-compile
  (setopt use-package-expand-minimally t))

(use-package goto-addr
  :commands (goto-address-mode)
  :hook (prog-mode . goto-address-prog-mode))
(use-package xref
  :ensure nil
  :custom
  (xref-search-program 'ripgrep)
  ;; Use Consult to select xref locations with preview
  (xref-show-xrefs-function #'consult-xref)
  (xref-show-definitions-function #'consult-xref))
(use-package display-fill-column-indicator
  :ensure nil
  :hook ((text-mode prog-mode) . display-fill-column-indicator-mode))
(use-package isearch
  :ensure nil
  :custom
  (isearch-allow-motion t)
  (isearch-allow-scroll t)
  (isearch-repeat-on-direction-change t)
  (isearch-wrap-pause 'no)
  (isearch-lazy-count t))
(use-package ibuffer
  :ensure nil
  :bind ([remap list-buffers] . ibuffer))
(use-package hippie-expand
  :ensure nil
  :bind ([remap dabbrev-expand] . hippie-expand))
(use-package diminish
  :ensure t)
(use-package minions
  :ensure t
  :config
  (minions-mode)
  :custom
  (minions-mode-line-lighter "…")
  (minions-prominent-modes '(flymake-mode lsp-mode vterm-copy-mode)))
(use-package dired
  :ensure nil
  :bind (:map dired-mode-map
              ("_" . dired-create-empty-file))
  :custom
  (dired-dwim-target t)
  (dired-listing-switches "-vhal --group-directories-first")
  (dired-mouse-drag-files t)
  (dired-use-ls-dired t)
  (dired-recursive-copies 'always)
  (dired-recursive-deletes 'top)
  (dired-create-destination-dirs 'ask)
  (dired-kill-when-opening-new-dired-buffer t))
(use-package dired-x
  :ensure nil
  :after dired
  :hook ((dired-mode . dired-omit-mode)
         (dired-mode . dired-hide-details-mode)
         (dired-mode . hl-line-mode))
  :custom
  (dired-omit-mode nil t)
  (dired-omit-size-limit 60000)
  (dired-omit-verbose nil))
(use-package hl-line
  :ensure nil
  :hook ((text-mode prog-mode) . hl-line-mode))
(use-package dired-aux
  :ensure nil
  :custom
  (dired-vc-rename-file t))
(use-package nerd-icons
  :ensure t
  :custom
  (nerd-icons-font-family "Symbols Nerd Font Mono"))
(use-package nerd-icons-dired
  :ensure t
  :hook
  (dired-mode . nerd-icons-dired-mode))
(use-package nerd-icons-completion
  :ensure t
  :hook (marginalia-mode . nerd-icons-completion-marginalia-setup))
(use-package fish-completion
  :ensure t
  :hook (eshell-mode . fish-completion-mode))
(use-package rainbow-mode
  :ensure t
  :custom
  (rainbow-ansi-colors nil)
  (rainbow-x-colors nil)
  :bind (:map ctl-x-x-map
              ("c" . rainbow-mode)))
(use-package tab-bar
  :ensure nil
  :custom
  (tab-bar-auto-width nil)
  (tab-bar-new-button-show t)
  (tab-bar-close-button-show t))
(use-package winner
  :ensure nil
  :init (winner-mode))
(use-package windsize
  :ensure t
  :hook (after-init . windsize-default-keybindings))
(use-package windmove
  :ensure nil
  :config
  (windmove-swap-states-default-keybindings '(shift meta))
  :bind (("M-<down>" . windmove-display-down)
         ("M-<up>" . windmove-display-up)
         ("M-<left>" . windmove-display-left)
         ("M-<right>" . windmove-display-right)))
(use-package window
  :ensure nil
  :config
  (add-to-list 'display-buffer-alist
               '("\\*Help"
                 (display-buffer-same-window)))
  (add-to-list 'display-buffer-alist
               '("\\*info"
                 (display-buffer-same-window)))
  (add-to-list 'display-buffer-alist
               '("\\*helpful"
                 (display-buffer-same-window)))
  :custom
  (same-window-buffer-names nil)
  (same-window-regexps nil))
(use-package man
  :ensure nil
  :custom
  (Man-notify-method 'pushy))
(use-package repeat
  :ensure nil
  :hook (after-init . repeat-mode))
(use-package emacs
  :init
  (setopt tab-always-indent 'complete)
  :hook
  (text-mode . auto-fill-mode)
  :bind
  ("M-z" . zap-up-to-char)
  ("M-Z" . zap-to-char)
  ("C-M-j" . duplicate-dwim)
  ("C-s" . isearch-forward-regexp)
  ("C-r" . isearch-backward-regexp)
  ("C-M-s" . isearch-forward)
  ("C-M-r" . isearch-backward)
  ("C-x O" . (lambda ()
               (interactive)
               (setq repeat-map 'other-window-repeat-map)
               (other-window -1)))
  :custom
  (default-transient-input-method "latin-1-prefix")
  (text-mode-ispell-word-completion nil)
  (read-extended-command-predicate #'command-completion-default-include-p)
  (comment-fill-column 80)
  (x-underline-at-descent-line t)
  (auto-revert-avoid-polling t)
  (custom-safe-themes t)
  (set-mark-command-repeat-pop t)
  (save-interprogram-paste-before-kill t)
  (mouse-yank-at-point t))
(use-package whitespace
  :ensure nil
  :hook
  ((prog-mode text-mode) . whitespace-mode)
  :custom
  (whitespace-style '(face trailing)))
(use-package simple
  :ensure nil
  :custom
  (visual-line-fringe-indicators '(left-curly-arrow nil)))
(use-package async
  :ensure t)
(use-package which-func
  :ensure nil
  :custom
  (which-func-update-delay 1.0))
(use-package project
  :ensure nil
  :bind
  (:map project-prefix-map
        ("t" . eat-project))
  :config
  (add-to-list 'project-switch-commands '(eat-project "Eat" ?t) t)
  (add-to-list 'project-switch-commands '(magit-project-status "Magit" ?m) t))
(use-package savehist
  :ensure nil
  :init
  (setopt savehist-file "~/.cache/emacs/savehist"
          history-length 1000
          history-delete-duplicates t
          savehist-save-minibuffer-history t
          savehist-additional-variables '(kill-ring search-ring regexp-search-ring))
  (savehist-mode 1))
(use-package undo-fu-session
  :ensure t
  :config (undo-fu-session-global-mode))
(use-package minibuffer
  :ensure nil
  :custom
  (completion-cycle-threshold nil)
  (completion-ignore-case t)
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))
(use-package vertico
  :ensure t
  :init
  (setopt read-file-name-completion-ignore-case t
          read-buffer-completion-ignore-case t)
  (vertico-mode))
(use-package vertico-directory
  :ensure nil
  :after vertico
  :demand
  :bind (:map vertico-map
              ("RET"   . vertico-directory-enter)
              ("DEL"   . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word)))
(use-package vertico-repeat
  :after vertico
  :ensure nil
  :bind
  ("M-R" . vertico-repeat)
  :hook
  (minibuffer-setup . vertico-repeat-save))
(use-package tmm
  :ensure nil
  :config
  (advice-add #'tmm-add-prompt :after #'minibuffer-hide-completions))
(use-package ffap
  :ensure nil
  :config
  (advice-add #'ffap-menu-ask :around
              (lambda (&rest args)
                (cl-letf (((symbol-function #'minibuffer-completion-help)
                           #'ignore))
                  (apply args)))))
(use-package markdown-mode
  :ensure t
  :custom
  (markdown-fontify-code-blocks-natively t))
(use-package markdown-ts-mode
  :ensure t
  :disabled
  :config
  (add-to-list 'treesit-language-source-alist
               '(markdown "https://github.com/tree-sitter-grammars/tree-sitter-markdown" "split_parser" "tree-sitter-markdown/src"))
  (add-to-list 'treesit-language-source-alist
               '(markdown-inline "https://github.com/tree-sitter-grammars/tree-sitter-markdown" "split_parser" "tree-sitter-markdown-inline/src")))
(use-package rainbow-delimiters
  :ensure t
  :hook ((emacs-lisp-mode ielm-mode lisp-interaction-mode lisp-mode) . rainbow-delimiters-mode))
(use-package magit
  :ensure t
  :custom
  (magit-define-global-key-bindings 'recommended)
  (magit-display-buffer-function 'magit-display-buffer-fullframe-status-v1))
(use-package forge
  :ensure t
  :after magit
  :custom
  (forge-database-file "~/.config/forge/database.sqlite")
  (forge-owned-accounts '(("hwadii")))
  (magit-save-repository-buffers nil))
(use-package doc-view
  :custom
  doc-view-resolution 200)
(use-package org
  :ensure nil
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((shell . t)
     (emacs-lisp . t))))
(use-package diff-hl
  :ensure t
  :hook ((magit-post-refresh . diff-hl-magit-post-refresh)
         (magit-pre-refresh . diff-hl-magit-pre-refresh))
  :config (global-diff-hl-mode)
  :custom (diff-hl-flydiff-mode t))
(use-package ediff
  :ensure nil
  :init
  (defvar ue-ediff-window-config nil "Window config before ediffing")
  :hook
  ((ediff-before-setup . (lambda ()
                           (setq ue-ediff-window-config (current-window-configuration))))
   ((ediff-suspend ediff-quit) . (lambda () (set-window-configuration ue-ediff-window-config)))
   (ediff-cleanup . (lambda () (ediff-janitor t nil))))
  :custom
  (ediff-window-setup-function 'ediff-setup-windows-plain)
  (ediff-split-window-function 'split-window-horizontally))
(use-package tree-sitter-indent :ensure t)
(use-package tree-sitter
  :ensure nil
  :hook (((tree-sitter-after-on . tree-sitter-hl-mode))))
(use-package treesit-auto
  :ensure t
  :custom
  (treesit-auto-install 'prompt)
  (treesit-auto-langs '(ruby rust python go dockerfile org python c c++ zig))
  :config
  (global-treesit-auto-mode))
(use-package xml-mode
  :ensure nil
  :mode "\\.csproj\\'")
(use-package rust-mode :ensure t)
(use-package typescript-mode
  :ensure t
  :hook (typescript-mode . (lambda () (setq fill-column 120))))
(use-package json-mode :ensure t)
(use-package zig-mode :ensure t)
(use-package zig-ts-mode
  :ensure t
  :mode "\\.zig\\'")
(use-package ruby-mode
  :ensure t
  :config
  :hook (ruby-ts-mode . (lambda ()
                          (setq fill-column 140)
                          (remove-hook 'flymake-diagnostic-functions #'ruby-flymake-auto t)
                          (remove-hook 'flymake-diagnostic-functions #'t t)))
  :custom
  (ruby-method-call-indent nil)
  (ruby-method-params-indent nil)
  (ruby-bracketed-args-indent nil)
  (ruby-flymake-use-rubocop-if-available nil))
(use-package ruby-ts-mode
  :ensure nil
  :mode "\\.rb\\'"
  :mode "Rakefile\\'"
  :mode "Gemfile\\'")
(use-package csharp-ts-mode
  :ensure nil
  :mode "\\.cs\\'")
(use-package apheleia
  :ensure t
  :config
  (add-to-list 'apheleia-formatters '(csharpier "dotnet" "csharpier" "--write-stdout"))
  :bind ("C-c l f a" . apheleia-format-buffer))
(use-package csharp-mode
  :ensure nil
  :hook ((csharp-mode csharp-ts-mode) . (lambda () (setq fill-column 120)))
  :config
  (require 'init-csharp)
  (reapply-csharp-ts-mode-font-lock-settings)) ; to remove when csharp-ts-mode gets updated
(use-package corfu
  :ensure t
  :init
  (global-corfu-mode)
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-file)
  :bind
  (:map corfu-map
        ("C-n" . corfu-next)
        ("C-p" . corfu-previous))
  :custom
  (corfu-auto nil)
  (corfu-cycle t))
(use-package corfu-popupinfo
  :ensure nil
  :after corfu
  :hook (corfu-mode . corfu-popupinfo-mode)
  :custom
  (corfu-popupinfo-delay '(0.25 . 0.1))
  (corfu-popupinfo-hide nil)
  :config
  (corfu-popupinfo-mode))
(use-package cape
  :ensure t
  :bind ("C-c p" . cape-prefix-map))
(use-package vterm
  :ensure t
  :bind (("C-c t" . vterm)
         :map vterm-mode-map
         ("C-q" . vterm-send-next-key))
  :custom
  (vterm-tramp-shells '(("docker" "/bin/sh") ("ssh" "/usr/bin/fish"))))
(use-package which-key
  :ensure nil
  :custom
  (which-key-show-early-on-C-h t)
  (which-key-idle-delay 10000)
  (which-key-idle-secondary-delay 0.05)
  :init (which-key-mode))
(use-package sudo-utils
  :ensure t
  :bind ("C-M-!" . sudo-utils-shell-command))
(use-package eglot
  :disabled
  :ensure nil
  :custom
  (eglot-autoshutdown t)
  (eglot-sync-connect 3)
  (eglot-stay-out-of '(flymake))
  (eglot-send-changes-idle-time 0.5)
  (eglot-events-buffer-config :size 0)
  :bind (("C-c l c" . eglot-reconnect)
         ("C-c l d" . flymake-show-buffer-diagnostics)
         ("C-c l f f" . eglot-format)
         ("C-c l f b" . eglot-format-buffer)
         ("C-c l l" . eglot)
         ("C-c l r n" . eglot-rename)
         ("C-c l s" . eglot-shutdown)
         ("C-c l i" . eglot-inlay-hints-mode)
         ("C-c l a" . eglot-code-actions))
  :config
  (add-to-list 'eglot-server-programs '((ruby-mode ruby-ts-mode) . ("ruby-lsp")) t)
  (add-to-list 'eglot-server-programs '((ruby-mode ruby-ts-mode) . ("bundle" "exec" "rubocop" "--lsp")) t)
  :hook
  (eglot-managed-mode . (lambda () (eglot-inlay-hints-mode -1)))
  ((ruby-mode ruby-ts-mode) . eglot-ensure)
  ((csharp-mode csharp-ts-mode) . eglot-ensure))
(use-package flymake
  :hook (prog-mode . flymake-mode)
  :custom
  (flymake-show-diagnostics-at-end-of-line 'short)
  :bind (:map flymake-mode-map
	      ("M-n" . flymake-goto-next-error)
	      ("M-p" . flymake-goto-prev-error)))
(use-package fish-mode
  :ensure t)
(use-package eat
  :ensure t
  :hook (eshell-mode . eat-eshell-mode))
(use-package ispell
  :ensure nil
  :custom
  (ispell-program-name "/opt/homebrew/bin/hunspell"))
(use-package flyspell
  :ensure nil
  :after ispell
  :hook ((markdown-mode org-mode) . flyspell-mode))
(use-package password-store-menu
  :ensure t)
(use-package rg
  :ensure t
  :config (rg-enable-default-bindings)
  :custom
  (rg-executable "rg"))
(use-package rg-isearch
  :ensure nil
  :after rg
  :bind (
         :map isearch-mode-map
         ("M-s R" . rg-isearch-menu)))
(use-package eshell
  :ensure nil
  :bind (("C-x C-z" . eshell)))
(use-package ligature
  :disabled
  :ensure t
  :hook (prog-mode . ligature-mode)
  :config
  (ligature-set-ligatures 'prog-mode '("==" "!=")))
(use-package marginalia
  :ensure t
  :custom (marginalia-mode 1))
(use-package inf-ruby :ensure t)
(use-package orderless
  :ensure t
  :after vertico
  :custom
  (orderless-matching-styles '(orderless-literal orderless-regexp)))
(use-package casual-editkit
  :ensure t)
(use-package ef-themes
  :ensure t
  :config
  (ef-themes-select 'ef-owl)
  :custom
  (ef-themes-variable-pitch-ui t)
  (ef-themes-mixed-fonts t))
(use-package mise
  :ensure t
  :hook ((prog-mode magit-mode) . mise-mode))
(use-package no-littering
  :ensure t
  :config
  ;; Write customizations to a separate file instead of this file.
  (setq custom-file (no-littering-expand-etc-file-name "custom.el")))
(use-package helpful :ensure t)
(use-package operate-on-number
  :ensure t
  :config
  (defvar-keymap operate-on-number-repeat-map
    :repeat t
    "+" #'apply-operation-to-number-at-point
    "-" #'apply-operation-to-number-at-point)
  (define-key global-map (kbd "C-c n") operate-on-number-repeat-map))
(use-package embark
  :ensure t
  :bind (("C-." . embark-act)
         ("M-." . embark-dwim)
         ("C-h B" . embark-bindings) ;; alternative for `describe-bindings'
         :map minibuffer-mode-map
         ("C-c C-e" . embark-export)
         ("C-c C-c" . embark-collect)
         :map embark-general-map
         ("w" . dictionary-search)
         :map embark-become-file+buffer-map
         ("t f" . find-file-other-tab))
  :init
  ;; Optionally replace the key help with a completing-read interface
  ;; (setq prefix-help-command #'embark-prefix-help-command)
  :custom
  (embark-indicators
   '(embark-minimal-indicator  ; default is embark-mixed-indicator
     embark-highlight-indicator
     embark-isearch-highlight-indicator))
  ;; Show the Embark target at point via Eldoc.  You may adjust the Eldoc
  ;; strategy, if you want to see the documentation from multiple providers.
  ;; (add-hook 'eldoc-documentation-functions #'embark-eldoc-first-target)
  ;; (setq eldoc-documentation-strategy #'eldoc-documentation-compose-eagerly)
  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))
(use-package dictionary
  :ensure nil
  :custom
  (dictionary-server "dict.org"))
(use-package server
  :ensure nil
  :defer 1
  :config
  (setq server-client-instructions nil)
  (unless (server-running-p)
    ;; Start server.
    (server-start)))
(use-package so-long
  :ensure nil
  :config (global-so-long-mode))
(use-package auto-compile
  :ensure t
  :config (auto-compile-on-load-mode)
  :custom
  (auto-compile-display-buffer nil)
  (auto-compile-mode-line-counter t))
(use-package eshell
  :ensure nil
  :custom
  (eshell-banner-message ""))
(use-package avy
  :ensure t
  :bind
  (("C-:" . avy-goto-char-timer)))
(use-package calendar
  :ensure nil
  :custom
  (calendar-week-start-day 1)
  (calendar-latitude [48 51 24 north])
  (calendar-longitude [2 21 07 east])
  (calendar-location-name "Paris, FR")
  (calendar-mark-holidays t)
  (calendar-mark-diary-flags t))
;; Example configuration for Consult
(use-package consult
  ;; Replace bindings. Lazily loaded by `use-package'.
  :bind (;; C-c bindings in `mode-specific-map'
         ("C-c M-x" . consult-mode-command)
         ("C-c h" . consult-history)
         ("C-c k" . consult-kmacro)
         ("C-c m" . consult-man)
         ("C-c i" . consult-info)
         ([remap Info-search] . consult-info)
         ;; C-x bindings in `ctl-x-map'
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ("C-x t b" . consult-buffer-other-tab)    ;; orig. switch-to-buffer-other-tab
         ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
         ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ;; M-g bindings in `goto-map'
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings in `search-map'
         ("M-s d" . consult-fd)                  ;; Alternative: consult-fd
         ("M-s c" . consult-locate)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
         ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)                 ;; orig. next-matching-history-element
         ("M-r" . consult-history))                ;; orig. previous-matching-history-element

  ;; The :init configuration is always executed (Not lazy)
  :init

  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
  :config

  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key "M-.")
  ;; (setq consult-preview-key '("S-<down>" "S-<up>"))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   ;; :preview-key "M-."
   :preview-key '(:debounce 0.4 any))

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  :custom
  (consult-narrow-key "<") ;; "C-+"
  (consult-man-args "man -k")

  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  ;; (keymap-set consult-narrow-map (concat consult-narrow-key " ?") #'consult-narrow-help)
  )
(use-package consult-gh
  :ensure t
  :after consult
  :config
  (setq consult-gh-default-orgs-list
        (append consult-gh-default-orgs-list
                (remove "" (split-string (or (consult-gh--command-to-string "org" "list") "") "\n")))))
(use-package consult-gh-embark
  :ensure t
  :config
  (consult-gh-embark-mode))
(use-package consult-gh-forge
  :disabled)
(use-package embark-consult
  :ensure t
  :after embark)
(use-package mouse
  :ensure nil
  :config (context-menu-mode))
(use-package combobulate
  :disabled
  :ensure t
  :vc (:url "https://github.com/mickeynp/combobulate" :branch "main")
  :preface
  ;; You can customize Combobulate's key prefix here.
  ;; Note that you may have to restart Emacs for this to take effect!
  (setq combobulate-key-prefix "C-c o"))
(use-package standard-themes
  :disabled
  :ensure t
  :custom
  (standard-themes-variable-pitch-ui t)
  (standard-themes-bold-constructs t)
  (standard-themes-italic-constructs t)
  (standard-themes-mixed-fonts t))
(use-package yaml-ts-mode
  :ensure nil
  :mode "\\.ya?ml\\'")
(use-package go-ts-mode
  :ensure nil
  :mode "\\.go\\'"
  :custom
  (go-ts-mode-indent-offset 4))
(use-package git-link
  :ensure t)
(use-package modus-themes
  :ensure t
  :config
  :custom
  (modus-themes-mixed-fonts t)
  (modus-themes-variable-pitch-ui t)
  (modus-themes-italic-constructs t)
  (modus-themes-bold-constructs nil)
  (modus-themes-to-toggle '(modus-operandi-tinted modus-vivendi-tinted))
  :hook
  (modus-themes-after-load-theme . (lambda ()
                                     (modus-themes-with-colors
                                       (custom-set-faces
                                        `(eglot-highlight-symbol-face ((t :background ,bg-ochre :extend nil)))
                                        `(forge-pullreq-open ((t :foreground ,green-intense :extend nil)))))
                                     (custom-set-faces
                                      '(lsp-face-highlight-read ((t (:inherit highlight :extend nil))))))))
(use-package smtpmail
  :ensure nil
  :custom
  (smtpmail-debug-info t)
  (smtpmail-smtp-server "smtp.mail.yahoo.com")
  (smtpmail-smtp-service 465))
(use-package eldoc-box
  :ensure t
  :bind (("C-h ." . eldoc-box-help-at-point))
  :config
  (setopt eldoc-echo-area-prefer-doc-buffer t)
  (setopt eldoc-echo-area-use-multiline-p nil))
(use-package transpose-frame
  :ensure t)
(use-package editorconfig
  :ensure nil
  :defer t
  :init
  (editorconfig-mode 1))
(use-package eglot-booster
  :disabled
  :ensure t
  :vc (:url "https://github.com/jdtsmith/eglot-booster" :rev :newest)
  :after eglot
  :config (eglot-booster-mode))
(use-package lsp-mode
  :ensure t
  :diminish (lsp-mode . "LSP")
  :custom
  (lsp-headerline-breadcrumb-enable nil)
  (lsp-modeline-code-actions-enable nil)
  (lsp-modeline-diagnostics-enable nil)
  (lsp-enable-snippet nil)
  (lsp-ruby-lsp-use-bundler t)
  (lsp-solargraph-use-bundler nil)
  (lsp-completion-provider :none)
  (lsp-keymap-prefix "C-c l")
  (lsp-enable-suggest-server-download nil)
  (lsp-auto-guess-root t)
  (lsp-csharp-server-install-dir "/Users/wadii/.config/emacs/var/lsp/server/omnisharp-roslyn/")
  (lsp-csharp-omnisharp-enable-decompilation-support t)
  (lsp-progress-prefix nil)
  :hook
  (lsp-mode . lsp-enable-which-key-integration)
  ((csharp-mode csharp-ts-mode) . lsp)
  ((ruby-mode ruby-ts-mode) . lsp))
(use-package exec-path-from-shell
  :if (memq window-system '(mac ns))
  :ensure t
  :config
  (exec-path-from-shell-initialize))
(use-package solarized-theme
  :disabled
  :ensure t
  :custom
  (solarized-use-less-bold t)
  (solarized-use-more-italic t)
  (solarized-distinct-doc-face t)
  (solarized-height-minus-1 1.0)
  (solarized-height-plus-1 1.0)
  (solarized-height-plus-2 1.0)
  (solarized-height-plus-3 1.0)
  (solarized-height-plus-4 1.0))
(use-package jq-mode
  :ensure t
  :commands jq-interactively
  :bind (:map json-mode-map
              ("C-c C-j" . jq-interactively)))
(use-package surround
  :ensure t
  :bind-keymap ("M-+" . surround-keymap))
(use-package doom-themes
  :disabled
  :ensure t
  :custom
  (doom-themes-enable-bold t)    ; if nil, bold is universally disabled
  (doom-themes-enable-italic t)) ; if nil, italics is universally disabled
(use-package terraform-mode
  :ensure t)
(use-package csv-mode
  :ensure t)
(use-package d2-mode
  :ensure t
  :mode "\\.d2\\'"
  :custom (d2-flags '("-t" "0"))
  :init
  (unbind-key "C-x C-o" d2-mode-map)
  :hook (d2-mode . (lambda () (setq-local indent-line-function #'indent-relative))))
(use-package just-ts-mode
  :ensure t)
(set-face-attribute 'default nil :family "Berkeley Mono" :height 170)
(set-face-attribute 'fixed-pitch nil :family "Berkeley Mono" :height 170)
(set-face-attribute 'variable-pitch nil :family "Atkinson Hyperlegible" :height 170)

(put 'narrow-to-region 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'scroll-left 'disabled nil)
(put 'set-goal-column 'disabled nil)
(put 'list-timers 'disabled nil)

(load custom-file t)
(require 'init-browse)
(require 'init-insert)
