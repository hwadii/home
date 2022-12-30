;;; Emfy 0.3.0-dev <https://github.com/susam/emfy>

;; Customize user interface.
(menu-bar-mode 0)
(when (display-graphic-p)
  (tool-bar-mode 0)
  (scroll-bar-mode 0))
(setq inhibit-startup-screen t)
(column-number-mode)

(setq frame-title-format '("%b"))
(setq ring-bell-function 'ignore)
(setq use-short-answers t)
(setq read-process-output-max (* 1024 1024)) ;; 1mb
(setq gc-cons-threshold 100000000)

;; Interactively do things.
(fido-vertical-mode 0)
(ido-mode 0)

;; Complete pairs
(electric-pair-mode 1)

;; Show stray whitespace.
(setq-default show-trailing-whitespace t)
(setq-default indicate-empty-lines t)
(setq-default indicate-buffer-boundaries 'left)

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

(recentf-mode 1)
(setq recentf-max-menu-items 25)
(setq recentf-max-saved-items 25)
(global-set-key (kbd "C-x C-r") 'recentf-open-files)

(tab-bar-mode 1)

;; Show directories first in dired.
(setq ls-lisp-dirs-first t)
(setq ls-lisp-use-insert-directory-program nil)

(setq xref-search-program 'ripgrep)

(setq diff-hl-show-staged-changes nil)

;; Typed text replaces the selection if typed text replaces the
;; selection if the selection is active
(delete-selection-mode 1)

(global-set-key [remap list-buffers] 'ibuffer)
(global-set-key [remap dabbrev-expand] 'hippie-expand)
(global-set-key (kbd "C-z") 'eshell)
(global-set-key (kbd "C-c o") 'find-file-at-point)
(global-set-key (kbd "C-c d") 'delete-trailing-whitespace)
(global-set-key (kbd "C-c =") 'calculator)
(global-set-key (kbd "M-i") 'imenu)
(global-set-key (kbd "C-c C-/") #'company-other-backend)
(global-set-key (kbd "M-Z") 'zap-up-to-char)

(defun wadii/term-mode ()
  (setq-local show-trailing-whitespace nil)
  (display-line-numbers-mode -1)
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
(defun sudo ()
  "Use TRAMP to `sudo' the current buffer."
  (interactive)
  (when buffer-file-name
    (find-alternate-file
     (concat "/sudo:root@localhost:"
             buffer-file-name))))

(global-set-key (kbd "C-c i d") 'wadii/insert-date)
(global-set-key (kbd "C-c i t") 'wadii/insert-time)
(global-set-key (kbd "C-c i u") 'wadii/insert-uuid)

(setq tab-bar-new-button-show nil
      tab-bar-close-button-show nil)

(setq calendar-week-start-day 1)

;; Enable installation of packages from MELPA.
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

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
                                        ("PROJECT" :inherit font-lock-string-face))))))
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
(use-package emacs
  :init
  (setq modus-themes-italic-constructs t
        modus-themes-bold-constructs t
        completion-cycle-threshold 3
        tab-always-indent 'complete)
  :config
  (adwaita-dark-theme-arrow-fringe-bmp-enable)
  :hook ((after-init . windmove-default-keybindings)
         (completion-list-mode . wadii/term-mode))
  ;; Load the theme of your choice:
  :bind ("<f5>" . modus-themes-toggle))
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
  (setq completion-styles '(basic partial-completion emacs22 flex))
  (vertico-mode))
(use-package markdown-mode)
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
  (corfu-auto t)
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
         (ruby-mode . eglot-ensure)
         (zig-mode . eglot-ensure)
         (typescript-mode . eglot-ensure)
         (rust-mode . eglot-ensure))
  :commands (eglot-ensure)
  :config
  (setq eldoc-echo-area-use-multiline-p nil))
(use-package flymake
  :config
  (define-key flymake-mode-map (kbd "M-n") 'flymake-goto-next-error)
  (define-key flymake-mode-map (kbd "M-p") 'flymake-goto-prev-error))
(use-package password-store)
(use-package solarized-theme
  :init
  (setq solarized-use-more-italic t)
  (setq solarized-use-less-bold t))
(use-package rg
  :hook (after-init . rg-enable-default-bindings)
  :bind (
         :map isearch-mode-map
         ("M-s r" . rg-isearch-menu)))
(use-package forge
  :after magit)
(use-package eshell
  :ensure nil
  :hook (eshell-mode . wadii/term-mode))
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
(use-package color-theme-sanityinc-tomorrow)
(use-package dired
  :ensure nil
  :bind (:map dired-mode-map
              ("r" . dired-start-process)))
(use-package adwaita-dark-theme)

(global-display-line-numbers-mode)
(global-display-fill-column-indicator-mode)

(set-face-attribute 'default nil :family "Source Code Pro" :height 105 :weight 'normal :width 'expanded)

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
