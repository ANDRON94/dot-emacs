;;; dired-follow-subdir.el --- auto open/close subdirs on movement -*- lexical-binding: t; -*-

(require 'dired)

(defvar-local dired-follow-subdir--last-post-command-pos nil
  "Last saved position after command is exectuted.")

(defun dired-follow-subdir-mode--post-command ()
  (let ((curr-pos (point)))
    (unless (equal (line-number-at-pos curr-pos)
                   (line-number-at-pos dired-follow-subdir--last-post-command-pos))
      (when (> (length dired-subdir-alist) 1)
        (save-excursion
          (goto-char (point-max))
          (cl-symbol-macrolet ((curr-subdir-pos (cdr (car dired-subdir-alist))))
            (while (>  curr-subdir-pos curr-pos)
              (dired-kill-subdir)))))
      (when-let* ((curr-filename (ignore-errors (dired-get-filename))))
        (setq curr-filename (file-name-as-directory (expand-file-name curr-filename)))
        (when (and (not (eq (find-file-name-handler curr-filename 'file-directory-p)
                            #'tramp-archive-file-name-handler))
                   (file-directory-p curr-filename))
          (save-excursion (dired-maybe-insert-subdir curr-filename))))
      (setq dired-follow-subdir--last-post-command-pos curr-pos))))

(define-minor-mode dired-follow-subdir-mode
  "Auto open/close subdirs on movement."
  :global nil
  :init-value nil
  :interactive (dired)
  (if dired-follow-subdir-mode
      (progn
        (setq dired-follow-subdir--last-post-command-pos (point))
        (add-hook 'post-command-hook #'dired-follow-subdir-mode--post-command 0 t))
    (remove-hook 'post-command-hook #'dired-follow-subdir-mode--post-command t)
    (setq dired-follow-subdir--last-post-command-pos nil)))

(provide 'dired-follow-subdir)
