(defvar enable-debug-p nil
  "Non-nil to enable debug.")
(setq gc-cons-threshold most-positive-fixnum)
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 16 1024 1024))))
(setq byte-compile-warnings '(not obsolete))
(setq warning-suppress-log-types '((comp) (bytecomp)))
(setq initial-major-mode 'fundamental-mode)
(setq display-time-default-load-average nil)
(setq ring-bell-function 'ignore)
(setq use-short-answers t)
(setq read-process-output-max (* 1024 1024)) ;; 1mb
(setq frame-inhibit-implied-resize t
      frame-resize-pixelwise t  ; fine resize
      package-native-compile t) ; native compile packages
(setq native-comp-async-report-warnings-errors 'silent)
(setq native-comp-warning-on-missing-source enable-debug-p)

(setq auto-revert-check-vc-info nil)

(setq inhibit-startup-echo-area-message (user-login-name))
(setq inhibit-startup-screen t
      inhibit-startup-echo-area-message user-login-name)
(setq initial-buffer-choice nil
      inhibit-startup-buffer-menu t)
(setq package-install-upgrade-built-in t)

(tool-bar-mode -1)
(scroll-bar-mode -1)
(column-number-mode 1)
(line-number-mode 1)
(menu-bar-mode 1)
(setq default-frame-alist '((fullscreen . maximized)))
(setq ns-use-thin-smoothing t)
