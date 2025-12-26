;; -*- coding: utf-8; mode: emacs-lisp -*-
;;
;; Temporarily increase GC threshold during init, set back to desired value
;; after completion. Permanently increase the rather small default value of
;; 800kB to 50 MiB. (Reduce if prolonged pauses/lags occur during use of Emacs.)
(setq gc-cons-threshold (* 256 1024 1024))
(add-hook 'after-init-hook #'(lambda () (setq gc-cons-threshold (* 50 1024 1024))))

;; Basic boot strapping settings
(setq user-full-name "Øyvind Stegard"
      user-mail-address "oyvind@stegard.net"
      inhibit-startup-screen t
      initial-scratch-message ";; Ready\n\n"
      custom-file (concat user-emacs-directory "custom.el")
      package-archives '(("gnu"   . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/"))
      package-enable-at-startup nil)

(defvar user-cache-directory (concat user-emacs-directory "cache/")
  "Directory for all Emacs caching needs. Avoid cluttering
`user-emacs-directory'.")
(defvar local-init-file (concat user-emacs-directory "local.el")
  "Optional local Emacs configuration that varies per
host/installation. Loaded as last step after all other intialization.")
(defvar local-init-file-after-hook nil
  "Hooks run after any `local-init-file' has been loaded.")

(make-directory user-cache-directory t)

;; Native compilation path
(when (native-comp-available-p)
  (startup-redirect-eln-cache (concat user-cache-directory "eln")))

;; Custom public libs
(push (concat user-emacs-directory "el") load-path)
(byte-recompile-directory (concat user-emacs-directory "el") 0)

;; Prune builtin org-mode from load-path because it creates issues when
;; newer org-mode is fetched and compiled from remote archive by package.el.
(eval-when-compile (require 'cl-seq))
(setq load-path (cl-remove-if
                 (lambda(path) (string-match "/.*share/emacs/.*/lisp/org$" path)) load-path))
