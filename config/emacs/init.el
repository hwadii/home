;; Interactively do things.
(fido-vertical-mode 0)
(ido-mode 0)

;; Complete pairs
(electric-pair-mode 0)

(pixel-scroll-precision-mode -1)

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
(setopt user-emacs-directory "~/.cache/emacs")
(setopt desktop-path '("~/.cache/emacs/desktops/"))
;; Write customizations to a separate file instead of this file.
(setopt custom-file (expand-file-name "custom.el" user-emacs-directory))
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

(setopt calendar-week-start-day 1)

;; Enable installation of packages from MELPA.
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

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
         ("C-c i d" . wadii/insert-date)
         ("C-c i t" . wadii/insert-time)
         ("C-c i u" . wadii/insert-uuid)))
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
  :ensure t
  :bind (("C-x g s" . magit-status)
         ("C-x g l" . magit-log-all)))
(use-package forge
  :ensure t
  :after magit
  :custom
  (forge-database-file "~/.config/forge/database.sqlite")
  (forge-owned-accounts '(("hwadii"))))
(use-package diff-hl
  :ensure t
  :hook (magit-post-refresh . diff-hl-magit-post-refresh)
  :custom (diff-hl-flydiff-mode t))
(use-package expand-region
  :bind ("C-=" . er/expand-region))
(use-package multiple-cursors
  :ensure t
  :custom
  (mc/always-run-for-all t)
  :bind (("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C-<" . mc/mark-all-like-this)
         ("C-S-c C-S-c" . mc/edit-lines)))
(use-package tree-sitter-langs :ensure t)
(use-package tree-sitter-indent :ensure t)
(use-package tree-sitter
  :diminish
  :ensure nil
  :init
  (setq major-mode-remap-alist
        '((yaml-mode . yaml-ts-mode)
          (bash-mode . bash-ts-mode)
          (js2-mode . js-ts-mode)
          (typescript-mode . typescript-ts-mode)
          (json-mode . json-ts-mode)
          (css-mode . css-ts-mode)
          (python-mode . python-ts-mode)
          (csharp-mode . csharp-ts-mode)
          (ruby-mode . ruby-ts-mode)
          (rust-mode . rust-ts-mode)))
  :after tree-sitter-langs
  :hook ((after-init . global-tree-sitter-mode)
         (tree-sitter-after-on . tree-sitter-hl-mode)))
(use-package treesit-auto
  :ensure t
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))
(use-package rust-mode :ensure t)
(use-package typescript-mode
  :ensure t
  :hook (typescript-mode . (lambda () (setq-local fill-column 120))))
(use-package json-mode :ensure t)
(use-package zig-mode :ensure t)
(use-package ruby-mode
  :ensure t
  :config
  :hook (ruby-mode . (lambda () (setq-local fill-column 140))))
(use-package corfu
  :ensure t
  :init
  (global-corfu-mode)
  :bind
  (:map corfu-map
        ("SPC" . corfu-insert-separator)
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
  :ensure t
  :diminish
  :config
  (which-key-mode))
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
  :hook (
         (javascript-mode . eglot-ensure)
         (ruby-mode . eglot-ensure)
         (zig-mode . eglot-ensure)
         (typescript-mode . eglot-ensure)
         (csharp-mode . eglot-ensure)
         (rust-mode . eglot-ensure))
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
         ("M-s r" . rg-isearch-menu)))
(use-package eshell
  :ensure nil
  :bind (("C-x C-z" . eshell)))
(use-package fd-dired :ensure t)
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
  :bind (("C-c +" . apply-operation-to-number-at-point)
         ("C-c -" . apply-operation-to-number-at-point)))
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

(set-face-attribute 'default nil :family "Iosevka Comfy" :height 150)
(set-face-attribute 'fixed-pitch nil :family "Iosevka Comfy" :height 150)
(set-face-attribute 'variable-pitch nil :family "Iosevka Aile" :height 140)

(put 'narrow-to-region 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'scroll-left 'disabled nil)
(put 'set-goal-column 'disabled nil)

(defun wadii/insert-date ()
  (interactive)
  (insert (format-time-string "%F")))
(defun wadii/insert-date-s ()
  (interactive)
  (insert (format-time-string "%Y%m%d")))
(defun wadii/insert-time ()
  (interactive)
  (insert (format-time-string "%FT%T%z")))
(defun wadii/insert-uuid ()
  (interactive)
  (insert (string-trim (shell-command-to-string "uuid"))))
(defconst wadii/streams
  '("39daph" "EnglishBen" "English_Ben" "TheGreatReview" "ThePrimeagen" "dmmulroy" "louispilfold" "lpil" "papesan" "teej_dv" "theprimeagen" "tigerbeetle" "tsoding" "untangledco" "lcolonq" "sphaerophoria" "etoiles"))
(defun wadii/open-stream ()
  "Open stream in external player."
  (interactive)
  (let ((stream (completing-read "Channel: " wadii/streams nil t)))
    (shell-command (format "iina https://twitch.tv/%s" stream))))
(defun wadii/browse-video ()
  (call-process "iina" nil 0 nil (read-string "URL: ")))
