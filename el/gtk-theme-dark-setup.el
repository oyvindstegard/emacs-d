;; Set dark theme variant for GTK menus and window decorations

(defun set-dark-wm-theme (frame)
  (select-frame frame) ;; this is important!
  (when (display-graphic-p)
    (progn
      (when (file-exists-p "/usr/bin/xprop")
    (progn
      (defvar winid nil)
      (setq winid (frame-parameter frame 'outer-window-id))
      (call-process "/usr/bin/xprop" nil nil nil "-f" "_GTK_THEME_VARIANT" "8u" "-set" "_GTK_THEME_VARIANT" "dark" "-id" winid))))))

(defun gtk-theme-set-wm-theme ()
  (set-dark-wm-theme (selected-frame)))

(add-hook 'window-setup-hook 'gtk-theme-set-wm-theme)
(add-hook 'after-make-frame-functions 'set-dark-wm-theme)

