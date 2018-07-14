;; -*- coding: utf-8 -*-
;; Integration with Gnome screensaver interface over D-Bus.

;; Author: Øyvind Stegard <oyvind.stegard@ifi.uio.no>
;; License: Public domain, use at own risk, no warranties of any kind.

;; Requires Emacs23 or better with D-Bus bindings and a Gnome session running.
;;

(require 'dbus)

(defvar gnome-screensaver-activated-hooks nil
  "Functions called when screensaver is activated.")
(defvar gnome-screensaver-deactivated-hooks nil
  "Functions called when screensaver is deactivated.")
(defvar gnome-screensaver--dbus-registration nil
  "Holds dbus signal registration object")

(defun gnome-screensaver-active-p ()
  "Returns t if screen saver is currently active, nil otherwise."
  (dbus-call-method :session "org.gnome.ScreenSaver"
                    "/org/gnome/ScreenSaver"
                    "org.gnome.ScreenSaver" "GetActive"))

(defun gnome-screensaver--ActiveChanged-signal-handler(active)
  (if active
      (run-hooks 'gnome-screensaver-activated-hooks)
    (run-hooks 'gnome-screensaver-deactivated-hooks)))

(setq gnome-screensaver--dbus-registration
      (dbus-register-signal
       :session "org.gnome.ScreenSaver" "/org/gnome/ScreenSaver"
       "org.gnome.ScreenSaver" "ActiveChanged" 'gnome-screensaver--ActiveChanged-signal-handler))

(provide 'gnome-screensaver)
