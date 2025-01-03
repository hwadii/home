;;; init-browse.el --- -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(defgroup browse-streams nil
  "Browse videos and things."
  :group 'emacs)
(defcustom browse-streams-streams nil
  "Favorite streams."
  :group 'browse-streams
  :type '(repeat string))
(defun browse-url-video-player (url)
  "Browse URL in appropriate video player."
  (let ((program (if (eq system-type 'darwin) "iina" "mpv")))
    (start-process program nil program (shell-quote-wildcard-pattern url) "--no-stdin")))
(defun browse-stream ()
  "Open stream in external player."
  (interactive)
  (browse-url-video-player (format "https://twitch.tv/%s"
                                   (completing-read "Channel: " browse-streams-streams nil 'confirm))))
(defun browse-video ()
  "Open video in external player."
  (interactive)
  (browse-url-video-player (read-string "URL: ")))

(provide 'init-browse)

;;; init-browse.el ends here
