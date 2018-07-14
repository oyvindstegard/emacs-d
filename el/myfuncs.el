;; Misc functions and macros (non-interactive)

(defmacro one-shot-hook (name hook &rest body)
  "Define a one shot function named NAME on the fly and add it to HOOK.
The first thing the function will do when called is to remove
itself from the hook and execute the rest of its BODY."
  `(progn
     (defun ,name ()
       (remove-hook ',hook ',name)
       ,@body)
     (add-hook ',hook ',name)))

(defun select-frame-set-input-focus-by-name (name)
  "Attempts to locate a frame with NAME and focus it.
Used by ec (my custom emacsclient wrapper)."
  (let ((fl (frames-on-display-list)) (f nil))
  (while (and fl (not f))
    (when (equal name (frame-parameter (car fl) 'name))
      (setq f (car fl))
      (select-frame-set-input-focus f))
    (setq fl (cdr fl)))))

(defun linux-os-release-field (field)
  "Extract a single field value from the /etc/os-release file.
FIELD should be a string containing the field name.

The function will return `nil` if the file does not exist, is not
readable or does not contain the requested field. Otherwise a
string value is returned. The string may contain shell style escaped chars.

See:
https://www.freedesktop.org/software/systemd/man/os-release.html"
  (with-temp-buffer
    (when (file-readable-p "/etc/os-release")
      (insert-file-contents "/etc/os-release")
      (goto-char (point-min))
      (when (re-search-forward (format "^%s=\\([^\n]+\\)" field) nil t)
        (let ((raw-value (match-string 1)))
          (when (string-match "^[\"']" raw-value)
            (setq raw-value (substring raw-value 1)))
          (when (string-match "[^\\][\"']$" raw-value)
            (setq raw-value (substring raw-value 0 -1)))
          raw-value)))))

(provide 'myfuncs)
