;; -*- coding: utf-8; mode: emacs-lisp -*-
;;
;; Init speed hack
(setq gc-cons-threshold 128000000)
(add-hook 'after-init-hook #'(lambda () (setq gc-cons-threshold 800000)))

;; Basic boot strapping settings
(setq user-full-name "Øyvind Stegard"
      user-mail-address "oyvind@stegard.net"
      inhibit-startup-screen t
      initial-scratch-message ";; Ready\n\n"
      custom-file (concat user-emacs-directory "custom.el")
      local-init-file (concat user-emacs-directory "local.el")
      user-cache-directory (concat user-emacs-directory "cache/")
      package-archives '(("org" . "http://orgmode.org/elpa/")
                         ("gnu" . "http://elpa.gnu.org/packages/")
                         ("melpa" . "http://melpa.org/packages/"))
      package-enable-at-startup nil)

(make-directory user-cache-directory t)

;; Prune builtin [and usually obsolete] org-mode from lisp load-path:
(eval-when-compile (require 'cl-seq))
(setq load-path
      (cl-remove-if
       (lambda(path)(string-match "/.*share/emacs/.*/lisp/org$" path)) load-path))

;; Custom libs
(push (concat user-emacs-directory "el") load-path)

;; Basic UI customizations
(setq font-use-system-font t
      initial-frame-alist
      '((tool-bar-lines . 0)
        (vertical-scroll-bars . nil)
        (horizontal-scroll-bars . nil)
        (menu-bar-lines . 0)
        (height . 62)
        (width . 114))
      default-frame-alist initial-frame-alist)
