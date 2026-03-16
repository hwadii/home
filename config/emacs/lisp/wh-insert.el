;;; wh-insert.el --- -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'org-macs)

(defun wh-insert-date ()
  "Insert a date."
  (interactive)
  (insert (format-time-string "%F")))
(defun wh-insert-date-s ()
  "Insert a date without dashes."
  (interactive)
  (insert (format-time-string "%Y%m%d")))
(defun wh-insert-time ()
  "Insert a timestamp."
  (interactive)
  (insert (format-time-string "%FT%T%z")))
(defun wh-insert-uuid ()
  "Insert a uuidv4."
  (interactive)
  (insert (org-id-uuid)))

(bind-key "d" 'wh-insert-date wh-map)

(provide 'wh-insert)

;;; wh-insert.el ends here
