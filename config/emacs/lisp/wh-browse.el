;;; wh-browse.el --- -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(defgroup browse-streams nil
  "Browse videos and things."
  :group 'emacs)
(defconst browse-streams-player (if (eq system-type 'darwin) "iina" "mpv"))
(defcustom browse-streams-streams nil
  "Favorite streams."
  :group 'browse-streams
  :type '(repeat string))
(defun browse-url-video-player (url &optional timecode)
  "Browse URL in appropriate video player at the given TIMECODE."
  (let ((start (if timecode timecode "0")))
    (start-process browse-streams-player nil browse-streams-player "--mpv-start" start (shell-quote-wildcard-pattern url))))
(defun browse-stream (stream)
  "Open STREAM in external player."
  (interactive "sChannel: ")
  (browse-url-video-player (format "https://twitch.tv/%s" stream)))
(defun browse-video (video)
  "Open VIDEO in external player."
  (interactive "sURL: ")
  (browse-url-video-player video))
(defun browse-video-at (video timecode)
  "Open VIDEO in external player at TIMECODE."
  (interactive "sURL: \nsTimecode: ")
  (browse-url-video-player video timecode))

(provide 'wh-browse)

;;; wh-browse.el ends here
