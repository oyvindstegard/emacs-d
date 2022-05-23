;; -*- coding: utf-8 -*-
;; Integration with UPower daemon over D-Bus.
;; Listens for suspend and resume signals and runs corresponding
;; hook variables.

;; Author: Øyvind Stegard <oyvind@stegard.net>
;; License: Public domain, use at own risk, no warranties of any kind.

;; Requires Emacs23 or better with D-Bus bindings and a UPower daemon running.
;;
;; You will need to call the function `upower-enable' for things to start
;; happening. Functions you would like to run when machine is suspended:
;; (add-hook 'upower-sleep-hook (lambda() (save-some-buffers t)))
;; Note the time constraint of about a second before machine is actually suspended.
;; So make your hook functions complete quickly.
;;
;; Functions you would like to run when machine is resumed:
;; (add-hook 'upower-resume-hook (lambda() (message "Yawn, wake up already ?")))

(require 'dbus)

(defvar upower-sleep-hook nil
  "Functions called when machine is about to sleep (suspend or hibernate).
   Machine will suspend in approximately one second from the time
   hooks in this variable are called.")
(defvar upower-resume-hook nil
  "Functions called when machine is resumed (from suspend or
  hibernate)")

(defun upower-sleep-signal-handler()
  (message "upower: received sleep signal, running sleep hooks ..")
  (run-hooks 'upower-sleep-hook)
  (message "upower: received sleep signal, running sleep hooks ..done"))

(defun upower-resume-signal-handler()
  (message "upower: received resume signal, running resume hooks ..")
  (run-hooks 'upower-resume-hook)
  (message "upower: received resume signal, running resume hooks ..done"))

(defun upower-register()
  "Register signal handlers for sleep/resume. Return list of
signal registration objects."
  (if (member "PrepareForSleep" (dbus-introspect-get-signal-names :system
                       "org.freedesktop.login1" "/org/freedesktop/login1" "org.freedesktop.login1.Manager"))
      ;; logind Manager interface available, prefer that instead of UPower:
      (list
       (dbus-register-signal :system "org.freedesktop.login1" "/org/freedesktop/login1"
                             "org.freedesktop.login1.Manager" "PrepareForSleep"
                             (lambda(sleep) (if sleep (upower-sleep-signal-handler) (upower-resume-signal-handler)))))
    ;; else Register directly for UPower signals:
    (list
     (dbus-register-signal
      :system
      "org.freedesktop.UPower" "/org/freedesktop/UPower"
      "org.freedesktop.UPower" "Sleeping"
      'upower-sleep-signal-handler)
     (dbus-register-signal
      :system
      "org.freedesktop.UPower" "/org/freedesktop/UPower"
      "org.freedesktop.UPower" "Resuming"
      'upower-resume-signal-handler))
    ))
  
(defvar upower-dbus-registration nil
  "List holding registered dbus signals")

(defun upower-enable()
  "Enable integration with UPower. Does nothing if already enabled."
  (interactive)
  (when (not upower-dbus-registration)
    (when (setq upower-dbus-registration
                (condition-case nil (upower-register)
                  (dbus-error (message "upower: warn: failed to enable dbus integration") nil)))
      (message "upower: enabled dbus integration."))))

(defun upower-disable()
  "Disable integration with UPower daemon. Does nothing if already disabled."
  (interactive)
  (while upower-dbus-registration
    (dbus-unregister-object (car upower-dbus-registration))
    (setq upower-dbus-registration (cdr upower-dbus-registration)))
  (message "upower: disabled dbus integration."))

(provide 'upower)
