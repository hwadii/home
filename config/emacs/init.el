;;; Emfy 0.3.0-dev <https://github.com/susam/emfy>

;; Interactively do things.
(fido-vertical-mode 0)
(ido-mode 0)

;; Complete pairs
(electric-pair-mode 1)

(pixel-scroll-precision-mode 1)

(setq frame-title-format '("%b"))
;; Show stray whitespace.
(setq-default show-trailing-whitespace t)
(setq-default indicate-empty-lines t)
(setq-default indicate-buffer-boundaries 'left)
(setq-default require-final-newline t)

;; Remove message in scratch buffer.
(setq-default initial-scratch-message nil)

;; Add a newline automatically at the end of a file while saving.
(setq-default require-final-newline t)

;; Consider a period followed by a single space to be end of sentence.
(setq sentence-end-double-space nil)

;; Use spaces, not tabs, for indentation.
(setq-default indent-tabs-mode nil)

;; Display the distance between two tab stops as 4 characters wide.
(setq-default tab-width 4)

;; Indentation setting for various languages.
(setq c-basic-offset 4)
(setq js-indent-level 2)
(setq typescript-indent-level 2)
(setq css-indent-offset 2)

;; Highlight matching pairs of parentheses.
(setq show-paren-delay 0)
(show-paren-mode)

;; Write auto-saves and backups to separate directory.
(make-directory "~/.tmp/emacs/auto-save/" t)
(setq auto-save-file-name-transforms '((".*" "~/.tmp/emacs/auto-save/" t)))
(setq backup-directory-alist '(("." . "~/.tmp/emacs/backup/")))

;; Do not move the current file while creating backup.
(setq backup-by-copying t)

;; Disable lockfiles.
(setq create-lockfiles nil)

(setq desktop-path '("~/.config/emacs/desktops/"))
(desktop-save-mode 1)
;; Write customizations to a separate file instead of this file.
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file t)

(setq nnrss-directory (expand-file-name "news/rss" user-emacs-directory))

(setq shell-file-name "/bin/fish")
(setq explicit-shell-file-name "/bin/fish")
(setq vterm-shell "/bin/fish")

(setq scroll-conservatively 101)
(setq scroll-margin 1)

(tab-bar-mode 1)

;; Show directories first in dired.
(setq insert-directory-program "ls")
(setq dired-listing-switches "-vhal --group-directories-first")

(setq xref-search-program 'ripgrep)

(setq diff-hl-show-staged-changes nil)

(setq switch-to-buffer-obey-display-actions t)

;; Typed text replaces the selection if typed text replaces the
;; selection if the selection is active
(delete-selection-mode 1)

(setq user-full-name       "Wadii Hajji"
      user-real-login-name "Wadii Hajji"
      user-login-name      "hwadii"
      user-mail-address    "wadii@cardiologs.com")

(global-set-key [remap list-buffers] 'ibuffer)
(global-set-key [remap dabbrev-expand] 'hippie-expand)

(setq tab-bar-new-button-show nil
      tab-bar-close-button-show nil)

(setq calendar-week-start-day 1)

;; Enable installation of packages from MELPA.
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

(eval-and-compile
  (setq use-package-always-ensure t
        use-package-expand-minimally t))

(use-package org
  :ensure nil
  :config
  (setq org-directory "~/code/notes"
        org-default-notes-file (concat org-directory "/notes.org")
        org-default-jounral-file (concat org-directory "/journal.org")
        org-default-reading-file (concat org-directory "/reading.org")
        org-todo-keywords (quote ((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!/!)")))
        org-todo-keyword-faces (quote (("NEXT" :inherit warning
                                        ("PROJECT" :inherit font-lock-string-face))))
        org-goto-interface 'outline-path-completion
        org-outline-path-complete-in-steps nil
        org-goto-max-level 5))
(use-package org-capture
  :ensure nil
  :after org
  :config
  (setq org-capture-templates
        '(("t" "Todo" entry (file+headline org-default-notes-file "Tasks")
           "* TODO %?\n  %i\n  %a")
          ("j" "Journal" entry (file+datetree org-default-jounral-file)
           "* %?\n  %i\n  %a")
          ))
  :bind ("C-c c" . org-capture))
(use-package tab-bar
  :ensure nil
  :bind ("C-x t (" . tab-bar-mode))
(use-package emacs
  :init
  (setq modus-themes-italic-constructs t
        modus-themes-bold-constructs t
        completion-cycle-threshold 3
        tab-always-indent 'complete)
  (global-display-fill-column-indicator-mode)
  :config
  (adwaita-dark-theme-arrow-fringe-bmp-enable)
  :hook ((after-init . windmove-default-keybindings)
         (completion-list-mode . wadii/term-mode)
         (prog-mode . display-line-numbers-mode))
  :bind (
         ("<f5>" . modus-themes-toggle)
         ("C-c o" . find-file-at-point)
         ("M-i" . imenu)
         ("M-Z" . zap-up-to-char)
         ("C-c i d" . wadii/insert-date)
         ("C-c i t" . wadii/insert-time)
         ("C-c i u" . wadii/insert-uuid)))
(use-package calculator
  :ensure nil
  :bind (("C-c =" . calculator)))
(use-package savehist
  :ensure nil
  :init
  (setq savehist-file "~/.config/emacs/savehist"
        history-length 1000
        history-delete-duplicates t
        savehist-save-minibuffer-history t
        savehist-additional-variables '(kill-ring search-ring regexp-search-ring))
  (savehist-mode 1))
(use-package vertico
  :init
  (setq read-file-name-completion-ignore-case t
        read-buffer-completion-ignore-case t
        completion-ignore-case t
        completion-styles '(orderless basic))
  (vertico-mode))
(use-package vertico-directory
  :ensure nil
  :after vertico
  :demand
  :bind (:map vertico-map
              ("RET"   . vertico-directory-enter)
              ("DEL"   . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word)))
(use-package crux
  :ensure t
  :commands crux-open-with
  :bind
  (("C-c d" . crux-duplicate-current-line-or-region)
   ("C-S-<return>" . crux-smart-open-line-above)
   ("S-<return>" . crux-smart-open-line)
   ("C-c n" . crux-cleanup-buffer-or-region)))
(use-package markdown-mode
  :custom
  (markdown-fontify-code-blocks-natively t))
(use-package paredit
  :config
  ;; Enable Paredit.
  (add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode)
  (add-hook 'eval-expression-minibuffer-setup-hook 'enable-paredit-mode)
  (add-hook 'ielm-mode-hook 'enable-paredit-mode)
  (add-hook 'lisp-interaction-mode-hook 'enable-paredit-mode)
  (add-hook 'lisp-mode-hook 'enable-paredit-mode)
  (add-hook 'minibuffer-setup-hook 'disable-paredit-mode))
(use-package rainbow-delimiters
  :config
  ;; Enable Rainbow Delimiters.
  (add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode)
  (add-hook 'ielm-mode-hook 'rainbow-delimiters-mode)
  (add-hook 'lisp-interaction-mode-hook 'rainbow-delimiters-mode)
  (add-hook 'lisp-mode-hook 'rainbow-delimiters-mode)
)
(use-package magit
  :commands magit-get-top-dir
  :bind (("C-c g" . magit-status)))
(use-package diff-hl
  :commands global-diff-hl-mode
  :init
  (add-hook 'after-init-hook #'global-diff-hl-mode)
  :config
  (diff-hl-flydiff-mode)
  (adwaita-dark-theme-diff-hl-fringe-bmp-enable)
  :hook (magit-post-refresh . diff-hl-magit-post-refresh))
(use-package elfeed
  :bind (
         ("C-c w" . elfeed)
         :map elfeed-search-mode-map
         ("f" . elfeed-update)))
(use-package expand-region
  :bind ("C-=" . er/expand-region))
(use-package tree-sitter-langs)
(use-package tree-sitter-indent)
(use-package tree-sitter
  :config
  (require 'tree-sitter-langs)
  (global-tree-sitter-mode)
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))
(use-package rust-mode)
(use-package typescript-mode
  :config
  (add-hook 'typescript-mode-hook (lambda () (setq-local fill-column 120))))
(use-package json-mode)
(use-package zig-mode)
(use-package ruby-mode
  :config
  (add-hook 'ruby-mode-hook (lambda () (setq-local fill-column 140))))
(use-package corfu
  :custom
  (corfu-auto nil)
  :init
  (global-corfu-mode))
(use-package vterm
  :bind ("C-c t" . vterm)
  :hook (vterm-mode . wadii/term-mode))
(use-package which-key
  :disabled)
(use-package auto-package-update
  :config
  (setq auto-package-update-delete-old-versions t)
  (setq auto-package-update-hide-results t)
  (auto-package-update-maybe))
(use-package sudo-utils
  :bind (
         ("C-M-!" . sudo-utils-shell-command)))
(use-package eglot
  :hook (
         (javascript-mode . eglot-ensure)
         (ruby-mode . eglot-ensure)
         (zig-mode . eglot-ensure)
         (typescript-mode . eglot-ensure)
         (rust-mode . eglot-ensure))
  :commands (eglot-ensure)
  :config
  (setq eldoc-echo-area-use-multiline-p nil
        eglot-autoshutdown t
        eglot-sync-connect 1
        eglot-connect-timeout 10
        eglot-stay-out-of '(flymake)
        eglot-send-changes-idle-time 0.1))
  (fset #'jsonrpc--log-event #'ignore)
(use-package flycheck
  :hook
  (prog-mode . flycheck-mode)
  :bind (:map flycheck-mode-map
         ("M-n" . flycheck-next-error)
         ("M-p" . flycheck-previous-error))
  :custom
  (flycheck-check-syntax-automatically '(save mode-enabled)))
(use-package tide
  :after (typescript-mode flycheck)
  :hook ((typescript-mode . tide-setup)))
(use-package password-store)
(use-package rg
  :hook (after-init . rg-enable-default-bindings)
  :bind (
         :map isearch-mode-map
         ("M-s r" . rg-isearch-menu)))
(use-package forge
  :after magit)
(use-package eshell
  :ensure nil
  :hook (eshell-mode . wadii/term-mode)
  :bind (("C-z" . eshell)))
(use-package fd-dired)
(use-package ligature
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
  :defer t
  :bind (
         ("C-c )" . marginalia-mode)
         :map minibuffer-mode-map
         ("M-A" . marginalia-cycle)))
(use-package inf-ruby)
(use-package doom-themes)
(use-package dired
  :ensure nil
  :bind (:map dired-mode-map
              ("z" . dired-start-process)
              ("r" . dired-xdg-open)))
(use-package adwaita-dark-theme)
(use-package orderless
  :after vertico
  :custom
  (orderless-matching-styles '(orderless-flex))
  (completion-category-overrides '((file (styles basic partial-completion)))))
(use-package ef-themes)
(use-package standard-themes)

(set-face-attribute 'default nil :font "BerkeleyMono Nerd Font-11:hintstyle=3:hinting=true:lcdfilter=3:antialias=true:weight=normal")
(set-face-attribute 'variable-pitch nil :font "Source Sans Pro-11:hintstyle=3:hinting=true:lcdfilter=3:antialias=true:weight=normal")

;; Start server.
(require 'server)
(unless (server-running-p)
  (server-start))
(put 'narrow-to-region 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

(defun wadii/show-buffer-file-name ()
  "Show the full path to the current file in the minibuffer."
  (interactive)
  (let ((file-name (buffer-file-name)))
    (if file-name
        (progn
          (message file-name)
          (kill-new file-name))
      (error "Buffer not visiting a file"))))

(defun dired-start-process (cmd &optional file-list)
  (interactive
   (let ((files (dired-get-marked-files
                 t current-prefix-arg)))
     (list
      (dired-read-shell-command "& on %s: "
                                current-prefix-arg files)
      files)))
  (let (list-switch)
    (start-process
     cmd nil shell-file-name
     shell-command-switch
     (format
      "nohup &>/dev/null %s \"%s\""
      (if (and (> (length file-list) 1)
               (setq list-switch
                     (cadr (assoc cmd dired-filelist-cmd))))
          (format "%s %s" cmd list-switch)
        cmd)
      (mapconcat #'expand-file-name file-list "\" \"")))))

(defun dired-xdg-open ()
  (interactive)
  (browse-url-xdg-open (car (dired-get-marked-files))))

(defun wadii/term-mode ()
  (setq-local show-trailing-whitespace nil)
  (display-fill-column-indicator-mode -1))
(defun wadii/insert-date ()
  (interactive)
  (insert (format-time-string "%F")))
(defun wadii/insert-time ()
  (interactive)
  (insert (format-time-string "%FT%T%z")))
(defun wadii/insert-uuid ()
  (interactive)
  (insert (string-trim (shell-command-to-string "uuid"))))
