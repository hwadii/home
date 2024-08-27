;;; init-browse.el --- -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(defconst browse-streams
  '("39daph"
    "EnglishBen"
    "English_Ben"
    "TheGreatReview"
    "ThePrimeagen"
    "dmmulroy"
    "louispilfold"
    "lpil"
    "papesan"
    "teej_dv"
    "theprimeagen"
    "tigerbeetle"
    "tsoding"
    "untangledco"
    "lcolonq"
    "sphaerophoria"
    "etoiles"
    "prodzpod"
    "pushcx"))
(defun browse-url-video-player (url)
  "Browse URL in appropriate video player."
  (let ((program (if (eq system-type 'darwin) "iina" "mpv")))
    (start-process program nil program (shell-quote-wildcard-pattern url) "--no-stdin")))
(defun browse-stream ()
  "Open stream in external player."
  (interactive)
  (browse-url-video-player (format "https://twitch.tv/%s" (completing-read "Channel: " browse-streams nil 'confirm))))
(defun browse-video ()
  "Open video in external player."
  (interactive)
  (browse-url-video-player (read-string "URL: ")))

(provide 'init-browse)

;;; init-browse.el ends here
