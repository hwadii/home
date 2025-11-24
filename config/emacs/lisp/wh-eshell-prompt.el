;;; wh-eshell-prompt.el --- -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(defun wh-eshell-prompt-fn ()
  "Eshell prompt with colors from the current (modus-themes based) enabled theme."
  (modus-themes-with-colors
    (let ((cwd (wh-pwd-shorten-dirs (wh-pwd-replace-home (eshell/pwd)) 1))
          (branch (magit-get-current-branch))
          (stat (magit-file-status))
          (suffix (if (= (file-user-uid) 0) "#" ">"))
          (nix-shell? (getenv "IN_NIX_SHELL")))
      (format "%s%s%s%s "
              (if nix-shell?
                  (propertize "<nix> " 'face `(:foreground ,cyan))
                "")
              (propertize cwd 'face `(:weight bold :foreground ,blue-warmer))
              (if branch
                  (format "%s%s%s"
                          (propertize "❙" 'face `(:foreground ,blue))
                          (propertize (format "%s" branch) 'face `(:foreground ,blue))
                          (propertize (if (length> stat 0) "*" "") 'face `(:weight bold :foreground ,yellow)))
                "")
              (if (eshell-exit-success-p)
                  (propertize suffix 'face `(:weight bold :foreground ,yellow))
                (propertize suffix 'face `(:weight bold :foreground ,red-cooler)))))))

(provide 'wh-eshell-prompt)

;;; wh-eshell-prompt.el ends here
