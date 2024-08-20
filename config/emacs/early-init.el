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
(setq inhibit-startup-message t
      frame-resize-pixelwise t  ; fine resize
      package-native-compile t) ; native compile packages

(setq auto-revert-check-vc-info nil)

(setq inhibit-startup-echo-area-message (user-login-name))

(setq package-install-upgrade-built-in t)

;; Customize user interface.
(push '(menu-bar-lines . 0)   default-frame-alist)
(push '(tool-bar-lines . 0)   default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(push '(horizontal-scroll-bars) default-frame-alist)

(setq tool-bar-mode nil
      scroll-bar-mode nil
      column-number-mode 1
      menu-bar-mode 1)

