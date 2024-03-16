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

(defun e28-p () "Are we running Emacs 28 or newer ?"
       (eval-when-compile (not (version< emacs-version "28"))))
(defun e29-p () "Are we running Emacs 29 or newer ?"
       (eval-when-compile (not (version< emacs-version "29"))))

(defun ubuntu-p () "Are we running on Ubuntu ?"
       (and (eq system-type 'gnu/linux) (equal "ubuntu" (linux-os-release-field "ID"))))

(defun wsl-p () "Are we running in WSL ?"
       (and (file-readable-p "/proc/version")
            (with-temp-buffer (insert-file-contents "/proc/version")
                              (goto-char (point-min))
                              (search-forward "Microsoft" nil t))
            t))

;; clock-jump
(defvar clock-jump-detector-hook nil
  "Functions to run when a system clock jump is detected.")
(defvar clock-jump-detector-threshold (* 5 60)
  "Minimum time skip (in seconds) to consider it a system clock
jump. When it is detected that the system clock jumps with more
than this number of seconds, then hooks in
`clock-jump-detector-hook' are run.")
(setq clock-jump-detector-time (current-time))
(defun clock-jump-detector ()
  (let ((time-passed (float-time (time-since clock-jump-detector-time))))
    (when (> time-passed clock-jump-detector-threshold)
      (message "Clock jump of %f seconds detected, running hooks .." time-passed)
      (run-hooks 'clock-jump-detector-hook))
    (setq clock-jump-detector-time (current-time))))
(run-at-time t 15 'clock-jump-detector)
;; end clock-jump-detector


(provide 'myfuncs)
