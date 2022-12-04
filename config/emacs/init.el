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
(fido-mode 1)

;; Complete pairs
(electric-pair-mode 11)

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

(setq savehist-file "~/.config/emacs/savehist"
      history-length 1000
      history-delete-duplicates t
      savehist-save-minibuffer-history t
      savehist-additional-variables '(kill-ring search-ring regexp-search-ring))
(savehist-mode 1)

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
(global-set-key (kbd "C-z") 'eshell)
(global-set-key (kbd "C-c o") 'find-file-at-point)
(global-set-key (kbd "C-c d") 'delete-trailing-whitespace)
(global-set-key (kbd "C-c =") 'calculator)

(setq tab-bar-new-button-show nil
      tab-bar-close-button-show nil)

(setq org-directory "~/code/notes")
(setq org-default-notes-file (concat org-directory "/notes.org"))
;;; Capturing
(global-set-key (kbd "C-c c") 'org-capture)

(setq org-capture-templates
      `(("t" "todo" entry (file "")  ; "" => `org-default-notes-file'
         "* NEXT %?\n%U\n")
        ("n" "note" entry (file "")
         "* %? :NOTE:\n%U\n%a\n")
        ))
(setq org-todo-keywords
      (quote ((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!/!)")
              (sequence "PROJECT(p)" "|" "DONE(d!/!)" "CANCELLED(c@/!)")
              (sequence "WAITING(w@/!)" "DELEGATED(e!)" "HOLD(h)" "|" "CANCELLED(c@/!)")))
      org-todo-repeat-to-state "NEXT")
(setq org-todo-keyword-faces
      (quote (("NEXT" :inherit warning)
              ("PROJECT" :inherit font-lock-string-face))))

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

;; Install packages.
(use-package emacs
  :init
  (setq modus-themes-italic-constructs t
        modus-themes-bold-constructs t)
  :config
  ;; Load the theme of your choice:
  :bind ("<f5>" . modus-themes-toggle))
(use-package markdown-mode)
(use-package paredit)
(use-package rainbow-delimiters)
(use-package magit
  :commands magit-get-top-dir
  :bind (("C-c g" . magit-status)))
(use-package diff-hl
  :after magit
  :hook (magit-post-refresh . diff-hl-magit-post-refresh)
  :config
  (add-hook 'after-init-hook 'global-diff-hl-mode)
  (diff-hl-flydiff-mode))
(use-package elfeed
  :init
  (global-set-key (kbd "C-x w") 'elfeed)
  :bind (:map elfeed-search-mode-map
              ("f" . elfeed-update)))
(use-package expand-region
  :bind ("C-=" . er/expand-region))
(use-package tree-sitter-langs)
(use-package tree-sitter
  :config
  (require 'tree-sitter-langs)
  (global-tree-sitter-mode)
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))
(use-package rust-mode)
(use-package typescript-mode)
(use-package zig-mode)
(use-package company
  :config
  (add-hook 'after-init-hook 'global-company-mode)
  (setq company-idle-delay 0
        company-minimum-prefix-length 4
        company-selection-wrap-around t))
(use-package vterm
  :bind ("C-c t" . vterm))
(use-package which-key
  :disabled)
(use-package auto-package-update
  :config
  (setq auto-package-update-delete-old-versions t)
  (setq auto-package-update-hide-results t)
  (auto-package-update-maybe))
(use-package sudo-utils)
(use-package eglot
  :hook (
         (ruby-mode . eglot-ensure)
         (zig-mode . eglot-ensure)
         (typescript-mode . eglot-ensure))
  :commands (eglot-ensure)
  :config
  (setq eldoc-echo-area-use-multiline-p nil))
(use-package flymake
  :config
  (define-key flymake-mode-map (kbd "M-n") 'flymake-goto-next-error)
  (define-key flymake-mode-map (kbd "M-p") 'flymake-goto-prev-error))
(use-package json-mode)
(use-package password-store)
(use-package solarized-theme
  :init
  (setq solarized-use-more-italic t)
  (setq solarized-use-less-bold t))
(use-package ef-themes)

;; Enable Paredit.
(add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode)
(add-hook 'eval-expression-minibuffer-setup-hook 'enable-paredit-mode)
(add-hook 'ielm-mode-hook 'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook 'enable-paredit-mode)
(add-hook 'lisp-mode-hook 'enable-paredit-mode)
(add-hook 'minibuffer-setup-hook 'disable-paredit-mode)

;; Enable Rainbow Delimiters.
(add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode)
(add-hook 'ielm-mode-hook 'rainbow-delimiters-mode)
(add-hook 'lisp-interaction-mode-hook 'rainbow-delimiters-mode)
(add-hook 'lisp-mode-hook 'rainbow-delimiters-mode)

;; Language major modes hooks
(add-hook 'ruby-mode-hook (lambda () (setq-local fill-column 140)))
(add-hook 'typescript-mode-hook (lambda () (setq-local fill-column 120)))

(defun wadii/term-mode ()
  (setq-local show-trailing-whitespace nil)
  (display-line-numbers-mode -1)
  (display-fill-column-indicator-mode -1))

(add-hook 'vterm-mode-hook 'wadii/term-mode)
(add-hook 'eshell-mode-hook 'wadii/term-mode)

(global-display-line-numbers-mode)
(global-display-fill-column-indicator-mode)

;; Customize Rainbow Delimiters.
(set-face-foreground 'rainbow-delimiters-depth-1-face "#c66")  ; red
(set-face-foreground 'rainbow-delimiters-depth-2-face "#6c6")  ; green
(set-face-foreground 'rainbow-delimiters-depth-3-face "#69f")  ; blue
(set-face-foreground 'rainbow-delimiters-depth-4-face "#cc6")  ; yellow
(set-face-foreground 'rainbow-delimiters-depth-5-face "#6cc")  ; cyan
(set-face-foreground 'rainbow-delimiters-depth-6-face "#c6c")  ; magenta
(set-face-foreground 'rainbow-delimiters-depth-7-face "#ccc")  ; light gray
(set-face-foreground 'rainbow-delimiters-depth-8-face "#999")  ; medium gray

(add-to-list 'default-frame-alist '(font . "Berkeley Mono-10.5"))

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
