;;; mailto-url-gnus.el --- Handle mailto: links with Gnus+Message-mode

;; For Gnus+Message-mode-combo, only tested on GNU Emacs24.
;; Author: Øyvind Stegard <oyvind.stegard@ifi.uio.no>
;; License: Public domain, use at own risk, no warranties of any kind.
(require 'gnus)

;; Where to store temporary mail attachment files from mailto:-links
(defvar mailto-url-gnus-attachment-tmp-dir
  (expand-file-name "mail-attachments" user-emacs-directory)
  "Path to directory where temporary mail attachment files should be stored.")

;; mailto-handling helper functions:
(defun mailto-url-gnus-store-temporary-attachment-file (filepath)
  "Stores a copy of file in temporary attachment directory. Returns new temporary file path."
  (let* ((attachments-path 
          (if (and (boundp 'mailto-url-gnus-attachment-tmp-dir) (stringp mailto-url-gnus-attachment-tmp-dir))
              (expand-file-name "" mailto-url-gnus-attachment-tmp-dir)
            (expand-file-name "mail-attachments" user-emacs-directory)))
        (newfilepath (expand-file-name (file-name-nondirectory filepath) attachments-path)))
    (unless (file-directory-p attachments-path)
      (make-directory attachments-path t)
      (set-file-modes attachments-path ?\700))

    (when (file-exists-p newfilepath)
        (setq newfilepath (make-temp-file (concat attachments-path "/") nil (file-name-nondirectory filepath))))
    
    (copy-file filepath newfilepath t nil nil)
    newfilepath))

(defun mailto-url-gnus-cleanup-old-attachments()
  "Clean up old temporary files in attachment directory. Returns number of old files deleted."
  (let ((seconds-in-day (* 60 60 24))
        file-age-in-seconds
        (cleanup-count 0)
        (attachments-path 
         (if (and (boundp 'mailto-url-gnus-attachment-tmp-dir) (stringp mailto-url-gnus-attachment-tmp-dir))
             (expand-file-name "" mailto-url-gnus-attachment-tmp-dir)
           (expand-file-name "mail-attachments" user-emacs-directory))))
    (when (file-directory-p attachments-path)
      (dolist (filepath (directory-files attachments-path t))
        ;; TODO: `time-to-seconds' is obsolete, replace with `float-time'.
        (setq file-age-in-seconds (time-to-seconds (time-subtract (current-time) (nth 5 (file-attributes filepath)))))
        (when (and (file-regular-p filepath) (> file-age-in-seconds seconds-in-day))
          (delete-file filepath)
          (setq cleanup-count (+ 1 cleanup-count)))))
    cleanup-count))

;; Handle mailto: links
;; Supports everything including file attachment references.
;; For Gnus+Message-mode-combo, only tested on GNU Emacs 24.

(defun mailto-url-gnus (url &optional make-tmp-attachment-copies delete-frame-on-exit)
  "Compose a new message from a 'mailto:...' URL. If
`make-tmp-attachment-copies' is set, then copy any referenced attachment files
to a private temporary location. If `delete-frame-on-exit' is t, then the
message composition frame will be deleted after the message has been sent or
aborted. This is suitable if you prefer to compose messages from mailto: links
in new and dedicated Emacs frames."
  (interactive "i")
  ;; Start up Gnus if necessary:
  (when (or (not (fboundp 'gnus-alive-p))
          (not (gnus-alive-p)))
    (gnus 1))

  (if (not url) (setq url "")) ;; Accept nil as URL

  (message "mailto-url-gnus called with url '%s'" url)

  ;; Let's strip any 'mailto:' at beginning of URL first.
  (setq url (if (string-match "^mailto:" url)
                (substring url (match-end 0)) url))

  ;; Parse interesting bits and invoke gnus-msg-mail
  (let (query-string
        query-string-alist
        subject
        cc
        bcc
        body
        attachments
        extra-headers
        (to (decode-coding-string (url-unhex-string 
             (replace-regexp-in-string "\\+" "%20" (substring url 0 (string-match "\\?" url)))) 'utf-8)))

    (when (string-match "\\?" url)
        (setq query-string (replace-regexp-in-string "\\+" "%20"
                                                     (substring url (+ 1 (match-beginning 0))))
              query-string-alist (url-parse-query-string query-string t)
              subject (cadr (assoc-string "subject" query-string-alist))
              cc (cadr (assoc-string "cc" query-string-alist))
              bcc (cadr (assoc-string "bcc" query-string-alist))
              body (cadr (assoc-string "body" query-string-alist))
              attachments (cdr (assoc-string "attach" query-string-alist))
              attachments (append (cdr (assoc-string "attachment" query-string-alist)) attachments)))

    ;; UTF-8 decode (not done by url-parse-query-string, which provides raw bytes)
    (setq subject (and subject (decode-coding-string subject 'utf-8))
          cc (and cc (decode-coding-string cc 'utf-8))
          bcc (and bcc (decode-coding-string bcc 'utf-8))
          body (and body (decode-coding-string body 'utf-8)))
    
    ;; Add Cc or Bcc, if any.
    (when cc
        (setq extra-headers (cons (list "Cc" cc) extra-headers)))
    (when bcc
        (setq extra-headers (cons (list "Bcc" bcc) extra-headers)))

    ;; Create new message buffer and setup exit actions
    (gnus-msg-mail to subject extra-headers
                   nil 'switch-to-buffer nil nil)
    (select-frame-set-input-focus (selected-frame))

    (when delete-frame-on-exit
      (make-local-variable 'message-kill-actions)
      (make-local-variable 'message-exit-actions)
      (make-local-variable 'message-postpone-actions)
      (setq message-kill-actions '(delete-frame))
      (setq message-exit-actions '(delete-frame))
      (setq message-postpone-actions '(delete-frame)))
    
    ;; If any attachments, then attach them all into new message buffer as MIME parts.
    ;; Create a private temporary copy of the files.
    (while attachments
      (let ((file (decode-coding-string (replace-regexp-in-string "^file://" "" (car attachments)) 'utf-8)))
        (when make-tmp-attachment-copies
          (setq file (mailto-url-gnus-store-temporary-attachment-file file)))
        (save-excursion
          (message-goto-body)
          (mml-attach-file file)))
      (setq attachments (cdr attachments)))

    ;; Do some housekeeping of old temporary attachment files:
    (mailto-url-gnus-cleanup-old-attachments)

    ;; If body, then we insert that directly into new message buffer
    (when body
      (save-excursion
        (message-goto-body)
        (insert (concat body "\n\n"))))

    ;; If To and Subect are provided, then jump to start of body.
    (when (and (> (length to) 0) (> (length subject) 0))
      (message-goto-body)
      (insert "\n")
      (message-goto-body)))
  
  ;; Auto-generated message contents should not set buffer to modified state:
  (set-buffer-modified-p nil))

(provide 'mailto-url-gnus)
