;;; wh-eglot.el --- -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:
;;;###autoload
(defun wh-eglot-setup-deno ()
  "Setup deno lsp for eglot when editing typescript."
  (interactive)
  (add-to-list 'eglot-server-programs '((typescript-mode typescript-ts-mode) . (eglot-deno "deno" "lsp")))

  (defclass eglot-deno (eglot-lsp-server) ()
    :documentation "A custom class for deno lsp.")

  (cl-defmethod eglot-initialization-options ((server eglot-deno))
    "Passes through required deno initialization options"
    (list
     :enable t
     :enablePaths t
     :lint t)))

;;;###autoload
(defun wh-eglot-unset-deno ()
  "Unset deno lsp eglot setup. Use tsserver when editing typescript."
  (interactive)
  (setq eglot-server-programs
        (seq-remove (lambda (elt) (equal (cdr elt) '(eglot-deno "deno" "lsp")))
                    eglot-server-programs)))

(provide 'wh-eglot)
;;; wh-eglot.el ends here
