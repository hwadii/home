;; Interactively do things.
(fido-vertical-mode 0)
(ido-mode 0)

;; Complete pairs
(electric-pair-mode 0)

(column-number-mode 1)
(line-number-mode 1)

(blink-cursor-mode 1)
(pixel-scroll-precision-mode 1)

(global-visual-line-mode)
(global-visual-wrap-prefix-mode)

;; Show stray whitespace.
(setq-default indicate-empty-lines t)
(setq-default indicate-buffer-boundaries 'left)
(setq-default require-final-newline t)

(setopt load-prefer-newer t)

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; Remove message in scratch buffer.
(setq-default initial-scratch-message nil)

;; Add a newline automatically at the end of a file while saving.
(setq-default require-final-newline t)

;; Consider a period followed by a single space to be end of sentence.
(setopt sentence-end-double-space nil)

;; Use spaces, not tabs, for indentation.
(setq-default indent-tabs-mode nil)

;; Display the distance between two tab stops as 4 characters wide.
(setq-default tab-width 4)

(setopt enable-recursive-minibuffers t)

(setopt view-read-only t)

;; Indentation setting for various languages.
(setopt c-basic-offset 4)
(setopt js-indent-level 2)
(setopt typescript-indent-level 2)
(setopt css-indent-offset 2)

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

(setopt nnrss-directory (expand-file-name "news/rss" user-emacs-directory))

(setopt shell-file-name "/opt/homebrew/bin/fish")
(setopt explicit-shell-file-name "/opt/homebrew/bin/bash")

(setopt scroll-conservatively 10)
(setopt scroll-margin 5)
(setopt scroll-preserve-screen-position t)

(tab-bar-mode 1)

;; Typed text replaces the selection if typed text replaces the
;; selection if the selection is active
(delete-selection-mode 1)

(setopt user-full-name       "Wadii Hajji"
        user-real-login-name "Wadii Hajji"
        user-login-name      "hwadii"
        user-mail-address    "hajji.wadii@yahoo.com")

(global-set-key [remap list-buffers] 'ibuffer)
(global-set-key [remap dabbrev-expand] 'hippie-expand)

(add-to-list 'trusted-content (concat user-emacs-directory "lisp/wh-browse.el"))
(add-to-list 'trusted-content (concat user-emacs-directory "lisp/wh-insert.el"))
(add-to-list 'trusted-content (concat user-emacs-directory "lisp/wh-eshell-prompt.el"))
(add-to-list 'trusted-content (concat user-emacs-directory "early-init.el"))

(defvar-keymap wh-prefix-map
  :doc "Keymap for my commands."
  :prefix 'wh-prefix-map)

;; Enable installation of packages from MELPA.
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))
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
  :bind
  ("C-s" . isearch-forward-regexp)
  ("C-r" . isearch-backward-regexp)
  ("C-M-s" . isearch-forward)
  ("C-M-r" . isearch-backward)
  :custom
  (isearch-allow-motion t)
  (isearch-allow-scroll 'unlimited)
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
  :ensure t
  :pin gnu)
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
  (dired-recursive-deletes 'always)
  (dired-create-destination-dirs 'ask)
  (dired-kill-when-opening-new-dired-buffer t)
  (dired-auto-revert-buffer t)
  (delete-by-moving-to-trash nil))
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
(use-package dired-filter
  :after dired
  :ensure t
  :bind (:map dired-mode-map ("/" . dired-filter-map)))
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
(use-package paren
  :ensure nil
  :custom
  (show-paren-delay 0.125)
  :config
  (show-paren-mode))
(use-package windsize
  :ensure t
  :hook (after-init . windsize-default-keybindings))
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
  :bind
  (:map window-prefix-map
        ("R" . unbury-buffer)
        ("t" . transpose-frame)
        ("r" . rotate-frame))
  :custom
  (same-window-buffer-names nil)
  (same-window-regexps nil)
  (switch-to-buffer-obey-display-actions t))
(use-package man
  :ensure nil
  :custom
  (Man-notify-method 'pushy))
(use-package repeat
  :ensure nil
  :hook (after-init . repeat-mode))
(use-package emacs
  :init
  (defun crm-indicator (args)
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string
                   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                   crm-separator)
                  (car args))
          (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)
  :bind
  ("M-z" . zap-up-to-char)
  ("M-Z" . zap-to-char)
  ("C-M-j" . duplicate-dwim)
  ("C-x O" . (lambda ()
               (interactive)
               (setq repeat-map 'other-window-repeat-map)
               (other-window -1)))
  ("M-g M-c" . switch-to-minibuffer)
  ("C-x C-#" . server-edit-abort)
  :bind-keymap ("C-c w" . wh-prefix-map)
  :custom
  (tab-always-indent 'complete)
  (default-transient-input-method "latin-1-prefix")
  (text-mode-ispell-word-completion nil)
  (read-extended-command-predicate #'command-completion-default-include-p)
  (comment-fill-column 80)
  (x-underline-at-descent-line t)
  (auto-revert-avoid-polling t)
  (custom-safe-themes t)
  (set-mark-command-repeat-pop t)
  (save-interprogram-paste-before-kill t)
  (mouse-yank-at-point t)
  (compilation-max-output-line-length nil)
  (yank-excluded-properties t)
  (insert-directory-program "gls"))
(use-package autorevert
  :ensure nil
  :custom
  (global-auto-revert-non-file-buffers t))
(use-package whitespace
  :ensure nil
  :hook
  ((prog-mode text-mode) . whitespace-mode)
  :custom
  (whitespace-style '(face trailing)))
(use-package simple
  :ensure nil
  :custom
  (visual-line-fringe-indicators '(left-curly-arrow nil))
  (visual-wrap-extra-indent 2))
(use-package async
  :ensure t)
(use-package which-func
  :ensure nil
  :custom
  (which-func-update-delay 1.0))
(use-package project
  :ensure nil)
(use-package consult-project-extra
  :ensure t
  :after consult
  :bind
  (("s-p" . project-find-file)
   (:map project-prefix-map
         ("f" . project-find-file)
         ("F" . consult-project-extra-find)))
  :custom
  (project-switch-commands '((project-find-file "Find" ?f)
                            (consult-project-extra-find "Find extra" ?F)
                            (project-find-dir "Directory" ?d)
                            (consult-ripgrep "Ripgrep" ?r)
                            (magit-project-status "Magit" ?m)
                            (project-eshell "Eshell" ?e)
                            (consult-project-buffer "Buffers" ?b)
                            (vterm "Terminal" ?t)
                            (project-any-command "Other" ?o))))
(use-package savehist
  :ensure nil
  :custom
  (history-length 1000)
  (history-delete-duplicates t)
  (savehist-save-minibuffer-history t)
  (savehist-additional-variables '(kill-ring search-ring regexp-search-ring))
  :init
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
  :custom
  (vertico-cycle t)
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
  ("M-R" . vertico-repeat-select)
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
  (magit-display-buffer-function 'magit-display-buffer-fullframe-status-v1)
  (magit-format-file-function 'magit-format-file-nerd-icons)
  (magit-save-repository-buffers nil)
  (magit-process-finish-apply-ansi-colors t)
  (magit-repository-directories '(("~/code/cardiologs". 1))))
(use-package forge
  :ensure t
  :after magit
  :custom
  (forge-database-file "~/.config/forge/database.sqlite")
  (forge-owned-accounts '(("hwadii"))))
(use-package doc-view
  :custom
  (doc-view-resolution 300))
(use-package pdf-tools
  :ensure t
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :custom
  (pdf-view-display-size 'fit-page))
(use-package org
  :ensure nil
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((shell . t)
     (emacs-lisp . t)))
  :bind
  ("C-h ." . display-local-help)
  :custom
  (org-hide-emphasis-markers t))
(use-package org-modern
  :ensure t
  :hook
  (org-mode . org-modern-mode)
  (org-agenda-finalize-hook . org-modern-agenda))
(use-package verb
  :ensure t
  :after org
  :config (define-key org-mode-map (kbd "C-c C-r") verb-command-map))
(use-package diff-hl
  :ensure t
  :hook ((magit-post-refresh . diff-hl-magit-post-refresh)
         (magit-pre-refresh . diff-hl-magit-pre-refresh))
  :config (global-diff-hl-mode)
  :custom
  (diff-hl-flydiff-mode t)
  (diff-hl-draw-borders nil)
  (diff-hl-show-staged-changes nil))
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
(use-package diff-mode
  :ensure nil
  :custom
  (diff-font-lock-prettify t))
(use-package tree-sitter-indent :ensure t)
(use-package tree-sitter
  :ensure nil
  :hook (((tree-sitter-after-on . tree-sitter-hl-mode))))
(use-package treesit-auto
  :ensure t
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
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
  (ruby-flymake-use-rubocop-if-available t))
(use-package ruby-ts-mode
  :ensure nil
  :mode "\\.rb\\'"
  :mode "Rakefile\\'"
  :mode "Gemfile\\'")
(use-package csharp-ts-mode
  :ensure nil
  :mode "\\.cs\\'")
(use-package scala-ts-mode
  :ensure t
  :mode "\\.scala'")
(use-package apheleia
  :ensure t
  :config
  (add-to-list 'apheleia-formatters '(csharpier "dotnet" "csharpier" "--write-stdout"))
  :bind ("C-c l f a" . apheleia-format-buffer))
(use-package csharp-mode
  :ensure nil
  :hook ((csharp-mode csharp-ts-mode) . (lambda () (setq fill-column 120))))
(use-package remember
  :ensure nil
  :config
  (defun wh-find-notes-file ()
    "Find notes file from notes directory."
    (interactive)
    (find-file "~/code/notes/notes"))
  :bind (:map wh-prefix-map
              ("r r" . remember)
              ("r o" . remember-notes)
              ("r n" . wh-find-notes-file))
  :custom
  (remember-notes-initial-major-mode 'org-mode)
  (remember-in-new-frame t)
  (remember-data-file "~/code/notes/remember"))
(use-package corfu
  :ensure t
  :after orderless
  :init
  (global-corfu-mode)
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-file)
  :custom
  (corfu-auto nil)
  (corfu-cycle t)
  :hook
  (corfu-mode . (lambda ()
                  (setq-local completion-styles '(orderless-literal-only basic)
                              completion-category-overrides nil
                              completion-category-defaults nil))))
(use-package corfu-popupinfo
  :ensure nil
  :after corfu
  :custom
  (corfu-popupinfo-delay '(1.25 . 0.5))
  (corfu-popupinfo-hide t)
  (corfu-preview-current nil)
  :config
  (corfu-popupinfo-mode 1))
(use-package cape
  :ensure t
  :bind ("C-c p" . cape-prefix-map))
(use-package vterm
  :ensure t
  :bind (("C-c t" . vterm)
         :map vterm-mode-map
         ("C-q" . vterm-send-next-key))
  :custom
  (vterm-tramp-shells '(("docker" "/bin/sh") ("ssh" "/usr/bin/fish")))
  (vterm-shell "/opt/homebrew/bin/fish"))
(use-package which-key
  :ensure nil
  :pin gnu
  :custom
  (which-key-show-early-on-C-h t)
  (which-key-idle-delay 10000.1)
  (which-key-idle-secondary-delay 0.05)
  :config (which-key-mode))
(use-package sudo-utils
  :ensure t
  :bind ("C-M-!" . sudo-utils-shell-command))
(use-package ansi-color
  :hook (compilation-filter . ansi-color-compilation-filter))
(use-package eglot
  :ensure nil
  :pin gnu
  :custom
  (eglot-autoshutdown t)
  (eglot-sync-connect 3)
  (eglot-stay-out-of nil)
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
  (add-to-list 'eglot-server-programs '((ruby-mode ruby-ts-mode) . ("ruby-lsp")))
  (add-to-list 'eglot-server-programs '(scala-ts-mode . ("metals")) t)
  (add-to-list 'eglot-server-programs '(zig-ts-mode . ("zls")) t)
  (add-to-list 'eglot-server-programs '(nix-ts-mode . ("nil")) t)
  :hook
  (eglot-managed-mode . (lambda () (eglot-inlay-hints-mode -1))))
(use-package flymake
  :hook (prog-mode . flymake-mode)
  :custom
  (flymake-show-diagnostics-at-end-of-line nil)
  (flymake-no-changes-timeout 0.5)
  :bind (:map flymake-mode-map
	          ("M-n" . flymake-goto-next-error)
	          ("M-p" . flymake-goto-prev-error)))
(use-package fish-mode
  :ensure t)
(use-package eat
  :ensure t
  :bind
  (:map eat-mode-map ("s-v" . eat-yank))
  :hook
  (eshell-mode . eat-eshell-mode)
  (eshell-mode . eat-eshell-visual-command-mode)
  :custom
  (eat-term-scrollback-size 262144)
  :config
  (defalias 'eshell/v #'eat--eshell-exec-visual))
(use-package ispell
  :ensure nil
  :custom
  (ispell-program-name "/opt/homebrew/bin/hunspell"))
(use-package flyspell
  :ensure nil
  :after ispell
  :hook ((markdown-mode org-mode) . flyspell-mode))
(use-package password-store-menu
  :ensure t
  :bind (:map wh-prefix-map ("p" . password-store-menu))
  :custom
  (password-store-menu-key nil))
(use-package rg
  :ensure t
  :config (rg-enable-default-bindings)
  :custom
  (rg-executable "rg"))
(use-package rg-isearch
  :ensure nil
  :after rg
  :bind (:map isearch-mode-map ("M-s R" . rg-isearch-menu)))
(use-package eshell
  :ensure nil
  :bind (("C-x C-z" . eshell))
  :hook
  (eshell-mode . abbrev-mode)
  (eshell-mode . goto-address-mode)
  :config
  (defun wh-pwd-replace-home (pwd)
    "Replace home in PWD with tilde (~) character."
    (let* ((home (expand-file-name (getenv "HOME")))
           (home-len (length home)))
      (if (and
           (>= (length pwd) home-len)
           (equal home (substring pwd 0 home-len)))
          (concat "~" (substring pwd home-len))
        pwd)))
  (defun wh-pwd-shorten-dirs (pwd n)
    "Shorten all directory names in PWD except the last N."
    (let ((p-lst (split-string pwd "/")))
      (if (> (length p-lst) n)
          (concat
           (mapconcat (lambda (elm) (if (zerop (length elm)) ""
                                      (substring elm 0 1)))
                      (butlast p-lst n)
                      "/")
           "/"
           (mapconcat (lambda (elm) elm)
                      (last p-lst n)
                      "/"))
        pwd)))  ;; Otherwise, we just return the PWD
  :custom
  (eshell-prefer-lisp-functions t)
  (eshell-scroll-show-maximum-output t)
  (eshell-banner-message "")
  (eshell-history-size (* 1024 256))
  (eshell-history-append t)
  (eshell-hist-ignoredups t)
  (eshell-buffer-maximum-lines 4096)
  (eshell-prompt-function #'wh-eshell-prompt-fn)
  (eshell-visual-subcommands '(("nix" "shell") ("kubectl" "exec") ("tsh" "ssh")))
  (eshell-visual-commands '("nvim" "tmux" "top" "htop" "less" "newsboat" "nu")))
(use-package ligature
  :ensure t
  :init
  (global-ligature-mode)
  :config
  (ligature-set-ligatures 't '("=>" "->" "<-" "<->" "<=>" "==>" "<==>" "<==" "==" "!=" "===" "!==" ">=" "<=" "::" "?." "??")))
(use-package marginalia
  :ensure t
  :custom (marginalia-mode 1))
(use-package inf-ruby :ensure t)
(use-package orderless
  :ensure t
  :after vertico
  :config
  (orderless-define-completion-style orderless-literal-only
    (orderless-style-dispatchers nil)
    (orderless-matching-styles '(orderless-literal)))
  :custom
  (orderless-matching-styles '(orderless-literal orderless-regexp)))
(use-package casual
  :ensure t
  :init (require 'casual-image)
  :after (calc dired ibuffer image)
  :bind
  (:map calc-mode-map ("?" . casual-calc-tmenu))
  (:map ibuffer-mode-map ("?" . casual-ibuffer-tmenu))
  (:map dired-mode-map ("?" . casual-dired-tmenu))
  (:map image-mode-map ("?" . casual-image-tmenu))
  (:map calendar-mode-map ("?" . casual-calendar-tmenu))
  (:map reb-mode-map ("C-c C-/" . casual-re-builder-tmenu)))
(use-package ef-themes
  :ensure t
  :bind
  ("<f6>" . ef-themes-toggle)
  :custom
  (ef-themes-variable-pitch-ui t)
  (ef-themes-mixed-fonts t)
  (ef-themes-to-toggle '(ef-reverie ef-dream)))
(use-package mise
  :ensure t
  :hook (after-init . global-mise-mode)
  :custom
  (mise-debug t))
(use-package envrc
  :ensure t
  :bind (:map envrc-mode-map ("C-c e" . envrc-command-map)))
(use-package no-littering
  :ensure t
  :config
  ;; Write customizations to a separate file instead of this file.
  (setopt custom-file (no-littering-expand-etc-file-name "custom.el")))
(use-package helpful
  :ensure t
  :bind (([remap describe-command] . helpful-command)
         ([remap describe-function] . helpful-callable)
         ([remap describe-key] . helpful-key)
         ([remap describe-symbol] . helpful-symbol)
         ([remap describe-variable] . helpful-variable)
         ("C-h F" . helpful-function)
         ("C-h K" . describe-keymap)
         :map helpful-mode-map
         ([remap revert-buffer] . helpful-update)))
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
         ("W" . dictionary-search)
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
  :custom
  (server-client-instructions nil)
  :config
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
(use-package avy
  :ensure t
  :config
  (defun avy-action-embark (pt)
    (unwind-protect
        (save-excursion
          (goto-char pt)
          (embark-act))
      (select-window
       (cdr (ring-ref avy-ring 0))))
    t)
  (setf (alist-get ?. avy-dispatch-alist) 'avy-action-embark)
  :bind
  (("C-:" . avy-goto-char-timer)
   ("M-g w" . avy-goto-word-1)
   :map isearch-mode-map
   ("C-'" . avy-isearch)))
(use-package ace-window
  :ensure t
  :custom
  (aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  :bind
  (("M-o" . ace-window)
   :map eat-mode-map
   ("M-o" . ace-window)
   :map eat-semi-char-mode-map
   ("M-o" . ace-window)))
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
         ("C-c K" . consult-kmacro)
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
         ("M-e" . isearch-edit-string)         ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
         ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)                 ;; orig. next-matching-history-element
         ("M-r" . consult-history)
         :map project-prefix-map
         ("r" . consult-ripgrep))

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
   ;; :preview-key "M-.")
   :preview-key '(:debounce 0.4 any))

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  :custom
  (consult-narrow-key "<") ;; "C-+"
  (consult-man-args "gman -k")
  (consult-preview-key 'any)

  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  ;; (keymap-set consult-narrow-map (concat consult-narrow-key " ?") #'consult-narrow-help)
  )
(use-package embark-consult
  :ensure t
  :after (embark consult))
(use-package affe
  :ensure t
  :after consult
  :config
  (defun affe-orderless-regexp-compiler (input _type _ignorecase)
    (setq input (cdr (orderless-compile input)))
    (cons input (apply-partially #'orderless--highlight input t)))
  (setq affe-regexp-compiler #'affe-orderless-regexp-compiler)
  ;; Manual preview key for `affe-grep'
  (consult-customize affe-grep :preview-key '(:debounce 0.4 any)))
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
(use-package nushell-ts-mode
  :ensure t)
(use-package git-link
  :ensure t
  :bind (:map wh-prefix-map ("g" . git-link-dispatch)))
(use-package modus-themes
  :ensure t
  :config
  :custom
  (modus-themes-mixed-fonts t)
  (modus-themes-variable-pitch-ui t)
  (modus-themes-italic-constructs t)
  (modus-themes-bold-constructs nil)
  (modus-themes-to-toggle '(modus-operandi-tinted modus-vivendi-tinted)))
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
  :pin gnu
  :init
  (editorconfig-mode 1))
(use-package eglot-booster
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
  (lsp-ruby-lsp-use-bundler nil)
  (lsp-solargraph-use-bundler nil)
  (lsp-completion-provider :none)
  (lsp-keymap-prefix "C-c l")
  (lsp-enable-suggest-server-download nil)
  (lsp-auto-guess-root t)
  (lsp-csharp-server-install-dir "/Users/wadii/.config/emacs/var/lsp/server/omnisharp-roslyn/")
  (lsp-csharp-omnisharp-enable-decompilation-support t)
  (lsp-progress-prefix nil)
  (lsp-disabled-clients '(ruby-ls rubocop-ls angular-ls))
  (lsp-enable-indentation nil)
  :hook
  (lsp-mode . lsp-enable-which-key-integration))
(use-package exec-path-from-shell
  :ensure t
  :init
  (exec-path-from-shell-copy-envs '("PASSWORD_STORE_DIR" "BROWSER" "COMPOSE_BAKE" "XDG_CONFIG_HOME" "RIPGREP_CONFIG_PATH"
                                    "EDITOR" "VISUAL" "PRE_COMMIT_COLOR" "LSP_USE_PLISTS" "LESS" "LS_COLORS" "LANG" "LC_ALL"
                                    "LANGUAGE" "HOMEBREW_NO_EMOJI")))
(use-package jq-mode
  :ensure t
  :commands jq-interactively
  :bind (:map json-mode-map
              ("C-c C-j" . jq-interactively)))
(use-package expand-region
  :ensure t
  :bind ("C-=" . er/expand-region))
(use-package surround
  :ensure t
  :bind-keymap ("M-+" . surround-keymap))
(use-package terraform-mode
  :ensure t)
(use-package csv-mode
  :ensure t)
(use-package just-ts-mode
  :ensure t)
(use-package nix-ts-mode
  :mode "\\.nix\\'"
  :ensure t)
(use-package visual-replace
  :ensure t
  :custom
  (visual-replace-default-to-full-scope t))
(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :custom
  (doom-modeline-minor-modes t)
  (doom-modeline-vcs-max-length 15)
  (doom-modeline-workspace-name nil)
  (doom-modeline-height 22)
  (doom-modeline-column-zero-based nil)
  (doom-modeline-total-line-number t)
  (doom-modeline-env-enable-ruby nil)
  (doom-modeline-icon t))
(use-package kubed
  :if (memq window-system '(mac ns))
  :ensure t
  :bind-keymap ("C-c k" . kubed-prefix-map)
  :bind (:map kubed-prefix-map ("k" . kubed-transient)))
(use-package man
  :ensure nil
  :custom
  (manual-program "gman"))
(use-package stillness-mode
  :ensure t
  :init (stillness-mode))
(use-package request
  :ensure t)
(use-package nov
  :commands nov-mode
  :ensure t)
(use-package eshell-vterm
  :ensure t)

(setopt wh-font-family "Adwaita Mono"
        wh-font-size 140)
(set-face-attribute 'default nil :font wh-font-family :height wh-font-size :width 'normal :weight 'regular)
(set-face-attribute 'fixed-pitch nil :font wh-font-family :height wh-font-size :width 'normal :weight 'regular)
(set-face-attribute 'variable-pitch nil :font "Adwaita Sans" :height 140 :width 'regular :weight 'regular)

(put 'narrow-to-region 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'scroll-left 'disabled nil)
(put 'set-goal-column 'disabled nil)
(put 'list-timers 'disabled nil)

(load custom-file t)
(require 'wh-browse)
(require 'wh-insert)
(require 'wh-eshell-prompt)
