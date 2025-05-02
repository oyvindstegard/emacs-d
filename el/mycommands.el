;; Misc interactive functions (commands)

(defun insert-date-string (time)
  "Insert current date as string. With prefix argument, insert
time as well. Format: %Y-%m-%d [%H:%M]."
  (interactive "P")
  (insert (if time (format-time-string "%Y-%m-%d %H:%M")
            (format-time-string "%Y-%m-%d"))))

(defun show-buffer-file-name()
  "Shows the complete path and name of the file that the current buffer is visiting."
  (interactive)
  (let ((minibuffer-message-timeout 10))
    (minibuffer-message (or (buffer-file-name) "No file for current buffer."))))

(defun kill-buffer-other-window (arg)
"Kill the buffer in the other window, and make the current buffer full size.
 If no other window, kills current buffer."
  (interactive "p")
  (let ((buf (save-window-excursion
               (other-window arg)
               (current-buffer))))
    (delete-windows-on buf)
    (kill-buffer buf)))

(defun window-dedicated-toggle ()
  "Toggle whether window should be dedicated to its current
buffer or not."
  (interactive)
  (let ((window (selected-window)))
    (if (window-dedicated-p window)
        (progn
          (set-window-dedicated-p window nil)
          (message "Window no longer dedicated: %s" window))
      (progn
        (set-window-dedicated-p window t)
        (message "Window dedicated: %s" window)))))

(defun unfill-region (start end)
  "Unfills region into one line."
  (interactive "r")
  (save-excursion
    (save-restriction
      (let ((fill-column (point-max)))
        (fill-region start end nil t)
        (goto-char start)
        (while (search-forward "\n\n" end t)
          (replace-match "\n"))))))

(defun extract-regexp-occurrences (regexp)
  "Extract all occurrences of a regular expression REGEXP in the current buffer.
Put result into a new buffer, each occurrence on its own line.

A regexp group can be specified via numeric prefix argument. The group is
selected for extraction. Default is to extract all occurrences of the entire regular
expression (group 0)."
  (interactive
   (list (read-regexp "Regexp")))
  (let ((group (if current-prefix-arg (prefix-numeric-value current-prefix-arg) 0))
        occurrences)

    (when (< group 0) (setq group 0))

    (save-excursion
      (while (re-search-forward regexp nil t)
        (setq occurrences (cons (match-string-no-properties group) occurrences))))

    (if occurrences
        (progn
          (setq occurrences (nreverse occurrences))
          (pop-to-buffer "*Extracted occurrences*")
          (delete-region (point-min) (point-max))
          (while occurrences
            (insert (concat (car occurrences) "\n"))
            (setq occurrences (cdr occurrences))
            ))
      (error "No occurrences found in current buffer"))
    ))

(defun xml-pretty-print-region (begin end)
  "Pretty format XML markup in region. The function inserts
linebreaks to separate tags that have nothing but whitespace
between them. It then indents the markup by using nxml's
indentation rules."
  (interactive "r")
  (when (>= begin end) (error "Region is empty."))
  (let* ((input (buffer-substring begin end))
         (output (with-temp-buffer
                   (delete-region (point-min) (point-max))
                   (insert input)
                   (nxml-mode)
                   (goto-char (point-min))
                   (while (search-forward-regexp "\>[ \\t]*\<" nil t)
                     (backward-char) (insert "\n"))
                   (indent-region (point-min) (point-max))
                   (delete-trailing-whitespace)
                   (buffer-string))))
    (delete-region begin end)
    (insert output)))

(defun uniquify-region ()
  "Remove duplicate adjacent lines in the given region, like unix command 'uniq'."
  (interactive)
  (narrow-to-region (region-beginning) (region-end))
  (beginning-of-buffer)
  (while (re-search-forward "\\(.*\n\\)\\1+" nil t)
    (replace-match "\\1" nil nil))
  (widen)
  nil)

(defun base64-encode-region-no-break()
  "Call `base64-encode-region' with optional arg no-break set to t."
  (interactive)
  (base64-encode-region (mark) (point) t))

(defun b64dec (beg end)
  "Base64-decode region, ignoring any errors with padding."
  (interactive "r")
  (base64-decode-region beg end nil t))

(defun tail-view-mode ()
  "Toggle `auto-revert-tail-mode' and `view-mode' for current buffer."
  (interactive)
  (call-interactively 'auto-revert-tail-mode)
  (if auto-revert-tail-mode
      (progn (goto-char (point-max)) (view-mode 1) (message "Auto revert tail mode and view mode enabled."))
    (progn (view-mode -1) (message "Auto revert tail mode and view mode disabled."))))

(provide 'mycommands)
