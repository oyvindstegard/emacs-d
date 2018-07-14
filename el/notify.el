;; -*- coding: utf-8; mode: emacs-lisp -*-
;; Using Freedesktop Notifications service over D-Bus ..
;; Call notify-show-message to trigger desktop notifications..

;; Author: Øyvind Stegard <oyvind.stegard@ifi.uio.no>
;; License: Public domain, use at own risk, no warranties of any kind.

;; DEPRECATED: Use Emacs' built-in support for notifications (in recent
;; versions) instead.

(require 'dbus)

(defvar notify-icon-alist
  '((mail . "notification-message-email")
    (appt . "/usr/share/icons/gnome/scalable/status/appointment-soon.svg")))

(defun notify-show-message(summary message &optional icon)
  "Show message MESSAGE using the org.freedesktop.Notifications Dbus API.
An optional icon reference can be provided, and available icons
can be seen in variable `notify-icon-alist'."
  ;; Encode summary and message string to utf-8
  (setq summary (encode-coding-string summary 'utf-8)
        message (encode-coding-string message 'utf-8))

  (ignore-errors
    (if (assoc icon notify-icon-alist)
        (dbus-call-method
         :session
         "org.freedesktop.Notifications" "/org/freedesktop/Notifications"
         "org.freedesktop.Notifications" "Notify" "GNU Emacs"
         0 (cdr (assoc icon notify-icon-alist)) summary message
         '(:array) '(:array :signature "{sv}") -1)

      (dbus-call-method
       :session "org.freedesktop.Notifications" "/org/freedesktop/Notifications"
       "org.freedesktop.Notifications" "Notify" "GNU Emacs"
       0 "" summary message '(:array) '(:array :signature "{sv}") -1)
      )))

(provide 'notify)
