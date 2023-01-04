(setq gc-cons-threshold 100000000)
(setq byte-compile-warnings '(not obsolete))
(setq warning-suppress-log-types '((comp) (bytecomp)))
(setq initial-major-mode 'fundamental-mode)
(setq inhibit-startup-screen t)
(setq display-time-default-load-average nil)
(setq frame-title-format '("%b"))
(setq ring-bell-function 'ignore)
(setq use-short-answers t)
(setq read-process-output-max (* 1024 1024)) ;; 1mb

(setq auto-revert-check-vc-info t)

(setq inhibit-startup-echo-area-message (user-login-name))

;; Customize user interface.
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(column-number-mode)
