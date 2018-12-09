;; -*- coding: utf-8; mode: emacs-lisp -*-
;;
;; Init speed hack
(setq gc-cons-threshold 64000000)
(add-hook 'after-init-hook #'(lambda () (setq gc-cons-threshold 800000)))

;; Basic boot strapping settings
(setq user-full-name "Øyvind Stegard"
      user-mail-address "oyvind@stegard.net"
      inhibit-startup-screen t
      initial-scratch-message ";; Ready\n\n"
      custom-file (concat user-emacs-directory "custom.el")
      local-init-file (concat user-emacs-directory "local.el")
      package-archives '(("org" . "http://orgmode.org/elpa/")
                         ("gnu" . "http://elpa.gnu.org/packages/")
                         ("melpa" . "http://melpa.org/packages/"))
      package-enable-at-startup nil)     ; Prevent second run of (package-initialize) after init

;; Prune builtin [and usually obsolete] org-mode from lisp load-path:
(eval-when-compile (require 'cl))
(setq load-path
      (remove-if
       (lambda(path)(string-match "/.*share/emacs/.*/lisp/org$" path)) load-path))
(push (concat user-emacs-directory "el") load-path)
(require 'myfuncs)

;; Basic visual setup (theme loaded as package later)
(setq font-use-system-font t
      initial-frame-alist
      '((tool-bar-lines . 0)
	(vertical-scroll-bars . nil)
	(horizontal-scroll-bars . nil)
        (menu-bar-lines . 1)
        (height . 62)
        (width . 114))
      default-frame-alist initial-frame-alist)

(when (display-graphic-p)
  (cond
   ((and (eq system-type 'gnu/linux) (equal "ubuntu" (linux-os-release-field "ID")))
    (setq initial-frame-alist (cons '(font . "Ubuntu Mono 12") initial-frame-alist)
	  default-frame-alist initial-frame-alist))
   ((eq system-type 'windows-nt)
    (setq initial-frame-alist (cons '(font . "Consolas 10") initial-frame-alist)
	  default-frame-alist initial-frame-alist))))

;; Boot strap use-package
(package-initialize)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile (require 'use-package))
(setq use-package-verbose t)

;; Apply visual theme
(setq custom-theme-directory (concat user-emacs-directory "themes"))
(load-theme 'nav t)

;; Preferences
(fset 'yes-or-no-p 'y-or-n-p)              ; Write "y" instead of "yes <RET>"
(global-set-key (kbd "s-E") 'delete-frame) ; Make Win+Shift+e kill frame
(global-set-key (kbd "C-c f") 'auto-fill-mode)
(global-set-key (kbd "C-x v f") 'view-file)
(global-set-key (kbd "C-w") 'backward-kill-word)
(global-set-key (kbd "C-x C-k") 'kill-region)
(global-set-key (kbd "M-i") 'indent-region)
(global-set-key (kbd "M-I") 'tab-to-tab-stop)
(global-set-key (kbd "C-x C-b") 'buffer-menu)
(global-set-key (kbd "<f12>") 'save-buffers-kill-emacs)
(global-set-key (kbd "C-c i") (lambda() (interactive)
                                (if (equal (buffer-file-name)
                                           (file-truename (concat user-emacs-directory "init.el")))
                                    (kill-buffer)
                                  (find-file (concat user-emacs-directory "init.el")))))
(global-set-key (kbd "C-M-\\") (lambda() (interactive) (message "Use M-i !")))

(with-eval-after-load "isearch"
  (global-set-key (kbd "C-x 4 s")
		  (lambda(&optional arg) "Isearch other window"
		    (interactive "p") (other-window arg) (call-interactively 'isearch-forward)))
  (define-key isearch-mode-map (kbd "M->")
    (lambda() "Go to end of buffer and repeat search backwards."
      (interactive)
      (goto-char (point-max))(isearch-repeat-backward)))
  (define-key isearch-mode-map (kbd "M-<")
    (lambda() "Go to beginning of buffer and repeat search forwards."
      (interactive)
      (goto-char (point-min))(isearch-repeat-forward))))
(setenv "EDITOR" "emacsclient")
(setenv "PAGER" "cat")
(set-language-environment "UTF-8")
(setq-default frame-title-format "%b  %f"
              indent-tabs-mode nil
              tab-width 4
              fill-column 80)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'list-timers 'disabled nil)
(setq default-input-method "rfc1345"
      kill-buffer-query-functions (delete 'process-kill-buffer-query-function
                                          kill-buffer-query-functions)
      confirm-kill-processes nil
      blink-cursor-mode nil
      window-min-height 10
      column-number-mode t
      display-time-24hr-format t
      display-time-day-and-date t
      ps-paper-type 'a4
      undo-outer-limit 30000000
      large-file-warning-threshold 67108864
      message-log-max 10000
      make-backup-files nil
      uniquify-buffer-name-style 'forward
      require-final-newline nil
      sentence-end-double-space nil
      mouse-yank-at-point t
      set-mark-command-repeat-pop t
      read-buffer-completion-ignore-case t
      read-file-name-completion-ignore-case t)

;; Packages (both internal and external)

(use-package show-point-mode
  :commands show-point-mode)

(use-package saveplace
  :config
  (setq save-place-file (concat user-emacs-directory "cache/places"))
  (save-place-mode 1))

(use-package savehist
  :defer 4
  :config
  (setq savehist-file (concat user-emacs-directory "cache/minibuffer-history"))
  (savehist-mode))

(use-package recentf
  :defer nil
  :bind ("C-x C-r" . recentf-open-files)
  :config
  (setq recentf-max-saved-items 50
        recentf-max-menu-items 20
	    recentf-save-file (concat user-emacs-directory "cache/recentf")
        recentf-exclude '("Privat"))
  (recentf-mode 1))

(use-package bookmark
  :defer t
  :config
  (setq bookmark-bmenu-file-column 60
	bookmark-default-file (concat user-emacs-directory "cache/bookmarks.bmk")))

(use-package man
  :defer t
  :config (setq Man-notify-method 'pushy))

(use-package nsm
  :defer t
  :config (setq nsm-settings-file (concat user-emacs-directory "cache/network-security.data")))

(use-package autorevert
  :config (setq auto-revert-remote-files t))

(use-package mycommands
  :bind
  (("C-c n" . show-buffer-file-name)
   ("C-x 4 k" . kill-buffer-other-window)
   ("C-c C-<return>" . window-dedicated-toggle))
  :commands (unfill-region
	     extract-regexp-occurences
             insert-date-string
             xml-pretty-print-region
             uniquify-region
             tail-view-mode))

(use-package winner
  :init (winner-mode 1))

(use-package ace-window
  :ensure t
  :bind ("M-o" . ace-window)
  :config (setq aw-scope 'frame))

(use-package ace-jump-mode
  :ensure t
  :bind ("C-c SPC". ace-jump-mode)
  :config (setq ace-jump-mode-scope 'frame))

(use-package buffer-move
  :ensure t
  :bind (("C-c C-<up>" . buf-move-up)
         ("C-c C-<down>" . buf-move-down)
         ("C-c C-<left>" . buf-move-left)
         ("C-c C-<right>" . buf-move-right)))

(use-package tabify
  :defer t
  :config (setq tabify-regexp "^\t* [ \t]+"))

(use-package plstore
  :defer t
  :config (setq plstore-encrypt-to (concat user-full-name " <" user-mail-address ">")))

(use-package dired-x
  :commands (dired-omit-mode dired-hide-details-mode)
  :config
  (setq dired-omit-files (concat dired-omit-files "\\|^\\..+$")
        dired-recursive-copies 'always
        dired-recursive-deletes 'top
        dired-dwim-target t
        dired-hide-details-hide-symlink-targets nil)
  (put 'dired-find-alternate-file 'disabled nil)
  (defun dired-xdg-open()
    "In dired, open the file named on this line with external command 'xdg-open'."
    (interactive)
    (let* ((file (dired-get-filename nil t))
           (process-connection-type t)) ; set to `pipe` if xdg-open does not work
      (call-process "xdg-open" nil 0 nil file)))
  :bind (("C-x C-j" . dired-jump)
         :map dired-mode-map
              ("C-k" . dired-kill-subdir)
              ("e" . dired-xdg-open))
  :hook (dired-mode . (lambda ()
                       ;; Set dired-x buffer-local variables here.  For example:
                       (dired-omit-mode 1)
                       (dired-hide-details-mode))))

(use-package icomplete
  :custom
  (icomplete-mode t)
  (icomplete-compute-delay 0.1)
  (icomplete-separator ", "))

(use-package pcomplete
  :defer t
  :config (setq pcomplete-dir-ignore "\\.(git\\|svn)/\\'"
		pcomplete-ignore-case t))

(use-package ido
  :defer t
  :bind (:map ido-common-completion-map ;Make ido bindings more similar to icomplete
              ("C-j" . ido-exit-minibuffer)
              ("<return>" . ido-select-text)))

(use-package comint
  :defer t
  :custom
  (comint-input-ignoredups t)
  (comint-prompt-read-only t)
  (comint-scroll-to-bottom-on-input t)
  (comint-buffer-maximum-size 32000)
  (ansi-color-for-comint-mode 'filter)
  :hook (comint-mode . (lambda ()
            (define-key comint-mode-map [remap kill-region] 'comint-kill-region)
            (define-key comint-mode-map [remap kill-whole-line]
              'comint-kill-whole-line)
	    (set (make-local-variable 'paragraph-start) comint-prompt-regexp)))
  :config
  (setq comint-password-prompt-regexp
        (concat comint-password-prompt-regexp
                "\\|^SSH password:\\s *\\'"
                "\\|^SUDO password:\\s *\\'"))
  
  (defadvice comint-read-input-ring (around comint-read-input-ring-AROUND activate)
    "Make `comint-read-input-ring' work when variable
`comint-input-ring-separator' has buffer local value, by
temporarily making the buffer local value global."
    (let ((current-val comint-input-ring-separator)
	  global-val)
      (if (local-variable-p 'comint-input-ring-separator)
	  (progn
	    (kill-local-variable 'comint-input-ring-separator)
	    (setq global-val comint-input-ring-separator)
	    (setq comint-input-ring-separator current-val)
	    (unwind-protect ad-do-it
	      (setq comint-input-ring-separator global-val))
	    (setq-local comint-input-ring-separator current-val))
	ad-do-it)))

  (defadvice comint-write-input-ring (around comint-write-input-ring-AROUND activate)
    "Make `comint-write-input-ring' use buffer local value of
`comint-input-ring-separator' if present."
    (let ((history-buf (get-buffer-create " *Temp Input History*"))
	  (separator comint-input-ring-separator))
      (with-current-buffer history-buf
	(setq-local comint-input-ring-separator separator)))
    ad-do-it))

(use-package shell
  :commands (shell)
  :config
  (setq shell-completion-fignore '(".svn" ".git" ".bzr" "#" "~" "%"))
  ; NB Expects prompt free of ANSI codes:
  (setq-default dirtrack-list '("^\\(([a-z0-9-]+) \\)?[a-z]*@[a-z]+[[:space:]:]?\\([^$[]+\\)" 2))
  :hook ((shell-mode . dirtrack-mode)
         (shell-mode . (lambda()(shell-dirtrack-mode -1)))))

(use-package tramp
  :defer t
  :config (setq tramp-persistency-file-name (concat user-emacs-directory "cache/tramp")
                tramp-verbose 2))

(use-package json-mode
  :defer t
  :ensure t)

(use-package markdown-mode
  :ensure t
  :defer t
  :config
  (setq markdown-asymmetric-header t)
  (push '("js" . js-mode) markdown-code-lang-modes)
  (push '("json" . json-mode) markdown-code-lang-modes))

(use-package dockerfile-mode
  :ensure t
  :defer t)

(use-package web-mode
  :ensure t
  :mode "\\.p?html?\\'")

(use-package python
  :ensure t
  :mode ("\\.py\\'" . python-mode)
  :interpreter ("python" . python-mode))

(use-package log-edit
  :defer t
  :config (remove-hook 'log-edit-hook 'log-edit-insert-message-template))

(use-package scala-mode
  :ensure t
  :defer t)

(use-package sbt-mode
  :ensure t
  :defer t)

(use-package cider
  :ensure t
  :defer t)

;; Testing hydra
(use-package hydra
  :ensure t)

(use-package php-mode
  :ensure t
  :defer t)

(use-package yaml-mode
  :ensure t
  :defer t)

(use-package restclient
  :ensure t
  :commands (restclient-mode)
  :mode ("\\.restclient\\'" . restclient-mode)
  :hook (restclient-mode . (lambda()(set (make-local-variable 'js-indent-level) 2)))
  :bind (:map restclient-mode-map ("C-c C-f" . json-mode-beautify)))

(use-package projectile
  :ensure t
  :init (setq projectile-keymap-prefix (kbd "C-c p"))
  :config
  (projectile-mode 1)
  (setq projectile-project-search-path '("~/dev" "~/Prosjekter")
        projectile-completion-system 'ido
	projectile-cache-file (concat user-emacs-directory "cache/projectile.cache")
	projectile-known-projects-file (concat user-emacs-directory "cache/projectile-bookmarks.eld"))
  (defadvice projectile-project-root (around ignore-remote first activate)
     (unless (file-remote-p default-directory) ad-do-it))
  (push "jabber-.*" projectile-globally-ignored-modes)
  (push "rcirc" projectile-globally-ignored-modes))

(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status)
         ("C-x M-g" . magit-dispatch-popup))
  :hook ((git-commit-mode . turn-on-auto-fill)
	 (git-commit-mode . flyspell-mode)
	 (git-commit-mode . (lambda() (setq ispell-local-dictionary "en"))))
  :config
  (add-to-list 'display-buffer-alist
                 '(".*COMMIT_EDITMSG". ((display-buffer-pop-up-window) .
                                        ((inhibit-same-window . t)))))
  (setq magit-last-seen-setup-instructions "1.4.0"
	magit-repository-directories '(("~/Prosjekter" . 1) ("~/dev" . 1) ("~/.emacs.d" . 0))
        magit-display-buffer-function 'magit-display-buffer-fullframe-status-v1)
  (push "-x" magit-cherry-pick-arguments))

(use-package sql
  :defer t
  :config (setq sql-input-ring-file-name (concat user-emacs-directory "cache/sql-history")
		sql-mysql-options '("--silent")))

(use-package cc-mode
  :defer t
  :hook (c-mode-common . (lambda()
                           (local-set-key  (kbd "C-c h") 'ff-find-other-file))))

(use-package etags
  :defer t
  :config (setq tags-revert-without-query t))

(use-package which-func
  :defer 5
  :config (which-function-mode 1))

(use-package paren
  :defer 5
  :config (show-paren-mode 1))

(use-package fortune
  :defer t
  :config
  (defun fortune-string (&optional file)
     "Return a string containing a random fortune cookie"
     (unless file (setq file fortune-file))
     (save-excursion
       (fortune-in-buffer t file)
       (set-buffer fortune-buffer-name)
       (buffer-string))))

(use-package unfill-paragraph
  :bind ("M-Q" . unfill-paragraph))

(use-package org
  :ensure org-plus-contrib
  :ensure htmlize
  :init
  (defvar org-directory-local-override nil
    "Override default `org-directory' by local configuration.")
  (setq org-directory "~/org/"
              org-default-notes-file (concat org-directory "index.org")
              org-modules '(org-info org-man ox-md ox-publish ox-icalendar ox-html))
  :mode ("\\.org\\'" . org-mode)
  :bind (("C-c o l" . org-store-link)
	 ("C-c o a" . org-agenda)
	 ("C-c o b" . org-switchb)
	 ("C-c o r" . org-capture)
     ("C-c o o" . (lambda() (interactive) (require 'org) (find-file org-default-notes-file))))
  :hook (org-mode . visual-line-mode)
  :config
  (org-load-modules-maybe t)
  (setq
   org-directory (or org-directory-local-override org-directory)
   org-default-notes-file (concat org-directory "index.org")
   org-src-fontify-natively t
   org-agenda-files (list org-default-notes-file
                          (concat org-directory "calendar.org")
                          (concat org-directory "work.org"))
   org-agenda-skip-unavailable-files t
   org-hide-leading-stars nil
   org-indent-mode-turns-on-hiding-stars nil
   org-startup-indented t
   org-odd-levels-only nil
   org-catch-invisible-edits 'show-and-error
   org-agenda-skip-scheduled-if-done t
   org-agenda-skip-deadline-if-done t
   org-agenda-skip-timestamp-if-done t
   org-startup-with-inline-images nil
   org-agenda-skip-scheduled-if-deadline-is-shown t
   org-agenda-default-appointment-duration 60
   org-agenda-todo-ignore-scheduled 7
   org-deadline-warning-days 2
   org-agenda-todo-ignore-deadlines 30
   org-email-link-description-format "Epost %c: %.60s"
   org-startup-align-all-tables t
   org-special-ctrl-a/e t
   org-publish-timestamp-directory (concat user-emacs-directory "cache/org-timestamps/")
   org-footnote-fill-after-inline-note-extraction nil
   org-goto-auto-isearch t
   org-enforce-todo-dependencies t
   ;; Refile
   org-refile-targets '((nil :maxlevel . 3) (org-agenda-files :maxlevel . 3))
   org-refile-use-outline-path 'file
   org-refile-allow-creating-parent-nodes 'confirm
   org-refile-target-verify-function (lambda()
					                   "Exclude todo keywords with a done state from refile targets"
					                   (not (member (nth 2 (org-heading-components)) org-done-keywords)))
   ;; Global export settings
   org-export-default-language "no"
   org-export-use-babel nil
   org-html-validation-link nil
   org-html-htmlize-output-type 'css
   org-html-doctype "html5"
   org-html-html5-fancy t
   org-html-head-include-default-style nil)
  (defvar org-switchb-extra-org-paths nil
    "List of extra org paths (files or directories) that shall be
searched for org files to auto-visit before `org-switchb'.")

  (defvar org-switchb-autoload-exclude-patterns nil
    "List of regular expressions matched against file paths
which shall not be autoloaded before org-switchb is invoked.")

  (defvar org-switchb-only-include-agenda-files nil
    "Whether to only include angenda files when initially calling org-switchb")

  (setq org-switchb-autoload-exclude-patterns '("gcal/[^/]*\\.org" ".*[mM]al[^/]*.org" "sitemap.org")
        org-switchb-only-include-agenda-files t)

  ;; Make sure desired org-files are automatically loaded when org-switchb is invoked
  (require 'find-lisp)
  (defadvice org-switchb (before org-switchb-DE-BEFORE activate)
    (let ((org-paths
           (delete-duplicates
            (append
             (when (listp org-agenda-files) org-agenda-files)
             (when (not org-switchb-only-include-agenda-files) (list org-directory))
             (when (not org-switchb-only-include-agenda-files) org-switchb-extra-org-paths))
            :test 'equal))
	  org-files)
      (while org-paths
	(cond
	 ((and (file-directory-p (car org-paths)) org-switchb-only-include-agenda-files)
      (mapc (lambda(file) (push file org-files)) (directory-files (car org-paths) t "\\.org$")))
     ((file-directory-p (car org-paths))
	  (ignore-errors
	    (mapc (lambda(file) (push file org-files)) (find-lisp-find-files (car org-paths) "\\.org$"))))
	 ((file-regular-p (car org-paths)) (push (car org-paths) org-files)))
	(setq org-paths (cdr org-paths)))
      (while org-files
	(unless (or (find-buffer-visiting (car org-files))
		    (let ((regexps org-switchb-autoload-exclude-patterns)
			  match)
		      (while (and regexps (not (and (string-match (car regexps) (car org-files)) (setq match t))))
			(setq regexps (cdr regexps)))
		      match))
	  (bury-buffer (find-file-noselect (car org-files) t)))
	(setq org-files (cdr org-files)))))

  ;; Ignore case on buffer name completion for org-switchb
  (defadvice org-switchb (around org-switchb-completion-ignore-case-DE activate)
    (let ((completion-ignore-case t))
      ad-do-it))
  
  (eval-after-load 'parse-time
    '(setq parse-time-weekdays (append parse-time-weekdays
				       '(("søn" . 0)("man" . 1)("tir" . 2)("ons" . 3)
					 ("tor" . 4)("fre" . 5)("lør" . 6)
					 ("søndag" . 0)("mandag" . 1)("tirsdag" . 2)("onsdag" . 3)
					 ("torsdag" . 4)("fredag" . 5)("lørdag" . 6)
					 ))
	   
	   parse-time-months (append parse-time-months
				     '(("januar" . 1)("februar" . 2)("mars" . 3)
				       ("april" . 4)("juni" . 6)("juli" . 7)
				       ("august" . 8)("september" . 9)("oktober" . 10)
				       ("november" . 11)("desember" . 12)
				       ("mai" . 5)("okt" . 10)("des" . 12)))))
  
  )

(use-package org-bullets
  :ensure t
  :after org
  :disabled
  :hook (org-mode . org-bullets-mode)
  :config (setq org-bullets-bullet-list '("•")))

;; custom.el
(when (file-readable-p custom-file) (load custom-file))

;; local.el
(when (file-readable-p local-init-file) (load local-init-file))
