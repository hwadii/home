;; Interactively do things.
(fido-vertical-mode 0)
(ido-mode 0)

;; Complete pairs
(electric-pair-mode 0)

(pixel-scroll-precision-mode)

(global-visual-line-mode)

;; Show stray whitespace.
(setq-default indicate-empty-lines t)
(setq-default indicate-buffer-boundaries 'left)
(setq-default require-final-newline t)

(setq load-prefer-newer t)

(setq auth-sources '("~/.authinfo"))

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
;; Write customizations to a separate file instead of this file.
(setq custom-file (no-littering-expand-etc-file-name "custom.el"))
(load custom-file t)

(setopt nnrss-directory (expand-file-name "news/rss" user-emacs-directory))

(setopt shell-file-name "/opt/homebrew/bin/fish")
(setopt explicit-shell-file-name "/opt/homebrew/bin/fish")
(setopt vterm-shell "/opt/homebrew/bin/fish")

(setopt scroll-conservatively 101)
(setopt scroll-margin 1)

(tab-bar-mode 1)

;; Show directories first in dired.
(setopt insert-directory-program "gls")

(setopt xref-search-program 'ripgrep)

(setopt diff-hl-show-staged-changes nil)

(setopt switch-to-buffer-obey-display-actions t)

;; Typed text replaces the selection if typed text replaces the
;; selection if the selection is active
(delete-selection-mode 1)

(setopt user-full-name       "Wadii Hajji"
        user-real-login-name "Wadii Hajji"
        user-login-name      "hwadii"
        user-mail-address    "wadii@cardiologs.com")

(setopt tab-bar-new-button-show t
        tab-bar-close-button-show t)

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
  :hook (((prog-mode text-mode) . goto-address-mode)))
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
  :bind ("C-x C-b" . ibuffer))
(use-package hippie-expand
  :ensure nil
  :bind ("M-/" . hippie-expand))
(use-package diminish
  :ensure t
  :config
  (diminish 'visual-line-mode))
(use-package dired
  :ensure nil
  :custom
  (dired-dwim-target t)
  (dired-listing-switches "-vhal --group-directories-first")
  (dired-mouse-drag-files t))
(use-package dired-x
  :ensure nil
  :after dired
  :diminish dired-omit-mode
  :hook ((dired-mode . dired-omit-mode)
         (dired-mode . dired-hide-details-mode)
         (dired-mode . hl-line-mode))
  :custom
  (dired-omit-mode nil t)
  (dired-omit-size-limit 60000))
(use-package dired-aux
  :ensure nil
  :custom
  (dired-vc-rename-file t))
(use-package nerd-icons
  :ensure t
  :custom
  (nerd-icons-font-family "Symbols Nerd Font Mono"))
(use-package nerd-icons-dired
  :diminish
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
  :diminish
  :ensure t
  :custom
  (rainbow-ansi-colors nil)
  (rainbow-x-colors nil)
  :bind (:map ctl-x-x-map
          ("c" . rainbow-mode)))
(use-package tab-bar
  :ensure nil
  :bind ("C-x t (" . tab-bar-mode))
(use-package windsize
  :ensure t
  :hook (after-init . windsize-default-keybindings))
(use-package windmove
  :ensure nil
  :hook ((after-init . windmove-default-keybindings)
         (after-init . windmove-swap-states-default-keybindings))
  :bind (("M-S-<down>" . windmove-display-down)
         ("M-S-<up>" . windmove-display-up)
         ("M-S-<left>" . windmove-display-left)
         ("M-S-<right>" . windmove-display-right)
         ("M-T" . windmove-display-new-tab)))
(use-package repeat
  :ensure nil
  :hook (after-init . repeat-mode))
(use-package emacs
  :init
  (setopt tab-always-indent 'complete)
  :hook ((text-mode . auto-fill-mode)
         ((text-mode prog-mode) . (lambda () (setq-local show-trailing-whitespace t))))
  :bind (
         ("M-Z" . zap-up-to-char)
         ("C-M-j" . duplicate-dwim)))
(use-package simple
  :ensure nil
  :custom
  (visual-line-fringe-indicators '(nil right-curly-arrow)))
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
  (completion-cycle-threshold 3)
  (completion-ignore-case t)
  (completion-styles '(orderless basic)))
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
(use-package rainbow-delimiters
  :ensure t
  :hook ((emacs-lisp-mode ielm-mode lisp-interaction-mode lisp-mode) . rainbow-delimiters-mode))
(use-package magit
  :demand
  :ensure t
  :custom
  (magit-define-global-key-bindings 'recommended))
(use-package forge
  :ensure t
  :after magit
  :custom
  (forge-database-file "~/.config/forge/database.sqlite")
  (forge-owned-accounts '(("hwadii"))))
(use-package diff-hl
  :ensure t
  :hook (magit-post-refresh . diff-hl-magit-post-refresh)
  :config (global-diff-hl-mode)
  :custom (diff-hl-flydiff-mode t))
(use-package tree-sitter-indent :ensure t)
(use-package tree-sitter
  :ensure nil
  :hook ((after-init . global-tree-sitter-mode)
         (tree-sitter-after-on . tree-sitter-hl-mode)))
(use-package treesit-auto
  :ensure t
  :custom
  (treesit-auto-install 'prompt)
  (treesit-auto-langs '(ruby rust python go))
  :config
  (global-treesit-auto-mode))
(use-package rust-mode :ensure t)
(use-package typescript-mode
  :ensure t
  :hook (typescript-mode . (lambda () (setq fill-column 120))))
(use-package json-mode :ensure t)
(use-package zig-mode :ensure t)
(use-package ruby-mode
  :ensure t
  :config
  :hook (ruby-ts-mode . (lambda () (setq fill-column 140))))
(use-package corfu
  :ensure t
  :init
  (global-corfu-mode)
  :bind
  (:map corfu-map
        ("C-n" . corfu-next)
        ("C-p" . corfu-previous)))
(use-package corfu-popupinfo
  :ensure nil
  :after corfu
  :hook (corfu-mode . corfu-popupinfo-mode)
  :custom
  (corfu-popupinfo-delay '(0.25 . 0.1))
  (corfu-popupinfo-hide nil)
  :config
  (corfu-popupinfo-mode))
(use-package vterm
  :ensure t
  :bind ("C-c t" . vterm))
(use-package which-key
  :ensure nil
  :diminish
  :custom
  (which-key-show-early-on-C-h t)
  (which-key-idle-delay 10000)
  (which-key-idle-secondary-delay 0.05)
  :init (which-key-mode))
(use-package auto-package-update
  :ensure t
  :custom
  (auto-package-update-delete-old-versions t)
  (auto-package-update-hide-results t)
  :config
  (auto-package-update-maybe))
(use-package sudo-utils
  :ensure t
  :bind ("C-M-!" . sudo-utils-shell-command))
(use-package eglot
  :ensure nil
  :config
  (setq eldoc-echo-area-use-multiline-p nil
        eglot-autoshutdown t
        eglot-sync-connect 1
        eglot-stay-out-of '(flymake)
        eglot-send-changes-idle-time 0.1))
(use-package flycheck
  :ensure t
  :diminish flycheck-mode
  :hook (prog-mode . flycheck-mode)
  :bind (:map flycheck-mode-map
              ("M-n" . flycheck-next-error)
              ("M-p" . flycheck-previous-error))
  :custom
  (flycheck-check-syntax-automatically '(save)))
(use-package flycheck-rust
  :ensure t
  :after flycheck
  :hook (rust-mode . flycheck-rust-setup))
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
  :diminish flyspell-mode
  :hook (text-mode . flyspell-mode))
(use-package password-store :ensure t)
(use-package rg
  :ensure t
  :config (rg-enable-default-bindings))
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
  :ensure t
  :config
  (ligature-set-ligatures 't '("|||>" "<|||" "<==>" "<!--" "####" "~~>" "||=" "||>"
                               ":::" "::=" "=:=" "===" "==>" "=!=" "=>>" "=<<" "=/=" "!=="
                               "!!." ">=>" ">>=" ">>>" ">>-" ">->" "->>" "-->" "---" "-<<"
                               "<~~" "<~>" "<*>" "<||" "<|>" "<$>" "<==" "<=>" "<=<" "<->"
                               "<--" "<-<" "<<=" "<<-" "<<<" "<+>" "</>" "###" "#_(" "..<"
                               "..." "+++" "/==" "///" "_|_" "www" "&&" "^=" "~~" "~@" "~="
                               "~>" "~-" "**" "*>" "*/" "||" "|}" "|]" "|=" "|>" "|-" "{|"
                               "[|" "]#" "::" ":=" ":>" ":<" "$>" "==" "=>" "!=" "!!" ">:"
                               ">=" ">>" ">-" "-~" "-|" "->" "--" "-<" "<~" "<*" "<|" "<:"
                               "<$" "<=" "<>" "<-" "<<" "<+" "</" "#{" "#[" "#:" "#=" "#!"
                               "##" "#(" "#?" "#_" "%%" ".=" ".-" ".." ".?" "+>" "++" "?:"
                               "?=" "?." "??" ";;" "/*" "/=" "/>" "//" "__" "~~" "(*" "*)"
                               "\\\\" "://"))
  :hook (prog-mode text-mode))
(use-package marginalia
  :ensure t
  :custom (marginalia-mode 1)
  :bind (
         ("C-c )" . marginalia-mode)
         :map minibuffer-mode-map
         ("M-A" . marginalia-cycle)))
(use-package inf-ruby :ensure t)
(use-package orderless
  :ensure t
  :after vertico
  :custom
  (orderless-matching-styles '(orderless-flex)))
(use-package ef-themes
  :ensure t
  :config
  (ef-themes-select 'ef-duo-light))
(use-package mise
  :ensure t
  :diminish mise-mode
  :hook prog-mode)
(use-package no-littering :ensure t)
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
  :diminish
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
  (calendar-week-start-day 1))
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
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ;; M-g bindings in `goto-map'
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flycheck)               ;; Alternative: consult-flycheck
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

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

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

  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  ;; (keymap-set consult-narrow-map (concat consult-narrow-key " ?") #'consult-narrow-help)
  )
(use-package consult-flycheck
  :ensure t
  :after consult)
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

(set-face-attribute 'default nil :family "Iosevka Comfy" :height 145)
(set-face-attribute 'fixed-pitch nil :family "Iosevka Comfy" :height 145)
(set-face-attribute 'variable-pitch nil :family "Iosevka Aile" :height 145)

(put 'narrow-to-region 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'scroll-left 'disabled nil)
(put 'set-goal-column 'disabled nil)

(require 'init-browse)
(require 'init-insert)
