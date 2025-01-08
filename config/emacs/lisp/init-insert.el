;;; init-insert.el --- -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'org-macs)

(defun insert-date ()
  "Insert a date."
  (interactive)
  (insert (format-time-string "%F")))
(defun insert-date-s ()
  "Insert a date without dashes."
  (interactive)
  (insert (format-time-string "%Y%m%d")))
(defun insert-time ()
  "Insert a timestamp."
  (interactive)
  (insert (format-time-string "%FT%T%z")))
(defun insert-uuid ()
  "Insert a uuidv4."
  (interactive)
  (insert (org-id-uuid)))

(define-key wadii-map (kbd "d") 'insert-date-s)

(provide 'init-insert)

;;; init-insert.el ends here
