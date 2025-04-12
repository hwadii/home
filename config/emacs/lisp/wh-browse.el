;;; wh-browse.el --- -*- lexical-binding: t -*-

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
    (start-process program nil program (shell-quote-wildcard-pattern url))))
(defun browse-stream (stream)
  "Open STREAM in external player."
  (interactive "sChannel: ")
  (browse-url-video-player (format "https://twitch.tv/%s" stream)))
(defun browse-video (video)
  "Open VIDEO in external player."
  (interactive "sURL: ")
  (browse-url-video-player video))

(provide 'wh-browse)

;;; wh-browse.el ends here
