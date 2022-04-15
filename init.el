;; -*- coding: utf-8; mode: emacs-lisp -*-
;;
;; See early-init.el for code running before this init code. Early-init is loaded
;; automatically for Emacs >= 27. For older Emacs, manual load must happen.

(when (eval-when-compile (version< emacs-version "27"))
  (load (concat user-emacs-directory "early-init.el")))

(require 'myfuncs)

(when (display-graphic-p)
  (cond
   ((and
     (eq system-type 'gnu/linux)
     (equal "ubuntu" (linux-os-release-field "ID")))
    (set-face-font 'fixed-pitch "Ubuntu Mono"))

   ((eq system-type 'windows-nt)
    (setq initial-frame-alist (cons '(font . "Consolas 10") initial-frame-alist)
	  default-frame-alist initial-frame-alist))))

;; Boot strap package and use-package
(package-initialize)                    ; early-init.el sets package-enable-at-startup to nil
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile (require 'use-package))
(setq use-package-verbose t)

;; Apply visual theme, copied from https://github.com/susam/emfy with slight
;; modifications.
(setq custom-theme-directory (concat user-emacs-directory "themes"))
(load-theme 'wombat)
(set-face-background 'default "#111")
(set-face-background 'cursor "#c96")
(set-face-background 'isearch "#c60")
(set-face-foreground 'isearch "#eee")
(set-face-background 'lazy-highlight "#960")
(set-face-foreground 'lazy-highlight "#ccc")
(set-face-foreground 'font-lock-comment-face "#cdad00")

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
(global-set-key (kbd "<f5>") 'compile)
(global-set-key (kbd "<f6>") 'next-error)
(global-set-key (kbd "<S-f6>") 'previous-error)
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
(blink-cursor-mode 0)
(menu-bar-mode 0)
(xterm-mouse-mode 1)
(global-set-key (kbd "<mouse-4>") (lambda()(interactive) (scroll-down-line 5)))
(global-set-key (kbd "<mouse-5>") (lambda()(interactive) (scroll-up-line 5)))
(setq default-input-method "rfc1345"
      kill-buffer-query-functions (delete 'process-kill-buffer-query-function
                                          kill-buffer-query-functions)
      confirm-kill-processes nil
      visible-cursor nil
      ring-bell-function 'ignore
      tty-menu-open-use-tmm t
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
  (setq save-place-file (concat user-cache-directory "places")
        save-place-limit 1000)
  (save-place-mode 1))

(use-package savehist
  :config
  (setq savehist-file (concat user-cache-directory "minibuffer-history"))
  (savehist-mode))

(use-package recentf
  :bind ("C-x C-r" . recentf-open-files)
  :config
  (setq recentf-max-saved-items 1000
        recentf-max-menu-items 50
	    recentf-save-file (concat user-cache-directory "recentf")
        recentf-exclude '("Privat", "\\.emacs\\.d/elpa/"))
  (recentf-mode 1))

(use-package bookmark
  :defer t
  :config
  (setq bookmark-bmenu-file-column 60
	bookmark-default-file (concat user-cache-directory "bookmarks.bmk")))

(use-package man
  :defer t
  :config
  (setq Man-notify-method 'aggressive)
  ;; Ensure man page display always reuses an existing Man-mode window if one exists:
  (add-to-list 'display-buffer-alist
               '("^\\*Man .*"
                 (display-buffer-reuse-mode-window)
                 (inhibit-same-window . nil)
                 (mode . Man-mode)
                 (reusable-frames . visible)
                 (inhibit-switch-frame . nil))))

(use-package nsm
  :defer t
  :config (setq nsm-settings-file (concat user-cache-directory "network-security.data")))

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
             base64-encode-region-no-break
             tail-view-mode))

(use-package winner
  :defer 2
  :init (winner-mode))

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
  (icomplete-compute-delay 0.1)
  (icomplete-separator ", ")
  :config
  (icomplete-mode 1))

(use-package pcomplete
  :defer t
  :config (setq pcomplete-dir-ignore "\\.(git\\|svn)/\\'"
		pcomplete-ignore-case t))

(use-package ido
  :defer t
  :config (setq ido-enable-flex-matching t)
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

(use-package term
  :defer t
  :config
  ;; Set default term.el ANSI color mappings to something nicer:
  (set-face-background 'term-color-magenta "#9400d3")
  (set-face-foreground 'term-color-magenta "#9400d3")
  (set-face-foreground 'term-color-cyan "#00bfff")
  (set-face-background 'term-color-cyan "#00bfff")
  (set-face-foreground 'term-color-blue "#4169e1")
  (set-face-background 'term-color-blue "#4169e1")
  (set-face-foreground 'term-color-green "#3cb371")
  (set-face-background 'term-color-green "#3cb371")
  (set-face-foreground 'term-color-black "#4b4b4b")
  (set-face-background 'term-color-black "#4b4b4b")
  (set-face-foreground 'term-color-yellow "#eead0e")
  (set-face-background 'term-color-yellow "#eead0e")
  (set-face-foreground 'term-color-red "#cd3700")
  (set-face-background 'term-color-red "#cd3700"))

(use-package tramp
  :defer t
  :config (setq tramp-persistency-file-name (concat user-cache-directory "tramp")
                tramp-verbose 2))

(use-package json-mode
  :defer t
  :ensure t
  :mode ("\\.webmanifest\\'" . json-mode))

(use-package js2-mode
  :ensure t
  :commands (js2-mode)
  :mode ("\\.js\\'" . js2-mode))

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
  :interpreter ("python" . python-mode)
  :config (setq python-shell-interpreter "python3"))

(use-package log-edit
  :defer t
  :config (remove-hook 'log-edit-hook 'log-edit-insert-message-template))

(use-package groovy-mode
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

(use-package nxml-mode
  :defer t
  :config
  ;; From: https://www.emacswiki.org/emacs/NxmlMode
  (defun nxml-where ()
    "Display the hierarchy of XML elements the point is on as a path."
    (interactive)
    (let ((path nil))
      (save-excursion
        (save-restriction
          (widen)
          (while (and (< (point-min) (point)) ;; Doesn't error if point is at beginning of buffer
                      (condition-case nil
                          (progn
                            (nxml-backward-up-element) ; always returns nil
                            t)
                        (error nil)))
            (setq path (cons (xmltok-start-tag-local-name) path)))
          (if (called-interactively-p t)
              (message "/%s" (mapconcat 'identity path "/"))
            (format "/%s" (mapconcat 'identity path "/")))))))
  (define-key nxml-mode-map (kbd "C-c w") 'nxml-where))

(use-package restclient
  :ensure t
  :commands (restclient-mode)
  :mode ("\\.restclient\\'" . restclient-mode)
  :hook (restclient-mode . (lambda() (set (make-local-variable 'js-indent-level) 2)))
  :bind (:map restclient-mode-map ("C-c C-f" . json-mode-beautify)))

(use-package projectile
  :ensure t
  :init (setq projectile-keymap-prefix (kbd "C-c p"))
  (when (file-directory-p "~/dev")
    (setq projectile-project-search-path '("~/dev")))
  (setq projectile-completion-system 'ido
        projectile-cache-file (concat user-cache-directory "projectile.cache")
        projectile-known-projects-file (concat user-cache-directory "projectile-bookmarks.eld"))
  :config
  (projectile-mode 1)
  (defadvice projectile-project-root (around ignore-remote first activate)
     (unless (file-remote-p default-directory) ad-do-it))
  (push "jabber-.*" projectile-globally-ignored-modes)
  (push "rcirc" projectile-globally-ignored-modes)
  (setq projectile-switch-project-action 'neotree-projectile-action
        projectile-mode-line-prefix " Pro"))

(use-package apache-mode
  :ensure t
  :commands (apache-mode))

(use-package neotree
  :ensure t
  :bind (([f8] . neotree-toggle))
  :config
  (setq neo-theme 'arrow))

;; transient is used by magit
(use-package transient
  :defer t
  :config
  (setq transient-levels-file (concat user-cache-directory "transient/levels.el")
        transient-history-file (concat user-cache-directory "transient/history.el")
        transient-values-file (concat user-cache-directory "transient/values.el")))

(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status)
         ("C-x M-g" . magit-dispatch))
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
  ;; TODO this option no longer exists in recent Magit versions, find out how to set it:
  ;(push "-x" magit-cherry-pick-arguments)
  )

(use-package sql
  :defer t
  :config (setq sql-input-ring-file-name (concat user-cache-directory "sql-history")
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
  :config
  (which-function-mode 1)
  (set-face-foreground 'which-func "LightSkyBlue"))

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

;; NB ! Org-mode must be installed manunally using package.el on first time init !
;; use-package will not automatically download org-mode from package archive due to
;; issues with builtin vs external.
(use-package org :pin "gnu"
  :ensure t
  :ensure htmlize
  :init
  (defvar org-directory-local-override nil
    "Override default `org-directory' by local configuration.")
  (defvar org-switchb-extra-org-paths nil
    "List of extra org paths (files or directories) that shall be
searched for org files to auto-visit before `org-switchb'.")
  (defvar org-switchb-autoload-exclude-patterns nil
    "List of regular expressions matched against file paths which
shall not be autoloaded before org-switchb is invoked.")
  (defvar org-switchb-only-include-agenda-files nil
    "Whether to only include angenda files when initially calling org-switchb")
  (setq org-directory "~/org/"
        org-default-notes-file (concat org-directory "journal.org")
        org-export-backends '(ascii html icalendar md))
  (add-hook 'local-init-file-after-hook
            (lambda()
              (setq org-directory (or org-directory-local-override org-directory)
                    org-default-notes-file (concat org-directory "journal.org"))))
  :mode ("\\.org\\'" . org-mode)
  :bind (("C-c o l" . org-store-link)
	     ("C-c o a" . org-agenda)
	     ("C-c o b" . org-switchb)
	     ("C-c o c" . org-capture)
         ("C-c o o" . (lambda ()
                        (interactive) (require 'org) (find-file (concat org-directory "index.org"))))
         ("C-c o k" . (lambda ()
                        (interactive)
                        (require 'org)
                        (save-some-buffers nil (lambda() (string-match "\\.org$" (buffer-name))))
                        (dolist (buf (org-buffer-list 'files))
                          (kill-buffer buf)))))
  :hook (org-mode . visual-line-mode)
  :config
  (setq
   org-agenda-files (list org-default-notes-file
                          (concat org-directory "index.org")
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
   org-agenda-todo-ignore-scheduled 'future
   org-agenda-todo-ignore-deadlines 'far
   org-deadline-warning-days 4
   org-email-link-description-format "E-post %c: %.60s"
   org-startup-align-all-tables t
   org-special-ctrl-a/e t
   org-publish-timestamp-directory (concat user-cache-directory "org-timestamps/")
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

   org-capture-templates '(("o" "Oppgave"
                            entry (file+olp "index.org" "Oppgaver")
                            "* TODO %?
%u" :jump-to-captured t)
                           ("k" "Nytt innslag i privat kalender" entry
                            (file+headline (lambda() (concat org-directory "index.org")) "Kalender")
                            "* %^{Tittel}
:PROPERTIES:
:ID: %(org-id-new)
:END:
%?%^T
" :prepend t :jump-to-captured t)

                           ("n" "Notat i journal"
                           entry (file+olp+datetree "journal.org")
                           "* %?" :jump-to-captured t))
   
   org-src-fontify-natively t
   org-edit-src-content-indentation 0
   ;; Global export settings
   org-export-default-language "no"
   org-export-use-babel nil
   org-html-validation-link nil
   org-html-htmlize-output-type 'css
   org-html-doctype "html5"
   org-html-html5-fancy t
   org-html-head-include-default-style nil)

  (org-babel-do-load-languages 'org-babel-load-languages
                               '((emacs-lisp . t)(java . t)(shell . t)))

  (setcdr (assq 'file org-link-frame-setup) 'find-file) ; org file links open in current window

  (setq org-switchb-autoload-exclude-patterns '("gcal/[^/]*\\.org" ".*[mM]al[^/]*.org" "sitemap.org")
        org-switchb-only-include-agenda-files t)

  (bind-key "C-<delete>" 'org-table-blank-field org-mode-map)

  (defun org-switchb--preload-some-org-buffers (&rest args)
    "Preload some well known Org files into buffers."
    (require 'find-lisp)
    (let ((org-paths
           (cl-delete-duplicates
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
	      (bury-buffer (find-file-noselect (car org-files) t))
          (message "Loading org file %s ..done" (car org-files)))
	    (setq org-files (cdr org-files)))))

  (advice-add 'org-switchb :before #'org-switchb--preload-some-org-buffers)

  (advice-add 'org-switchb :around (lambda(orig-fun &rest args)
                                     "Ignore case for Org buffer completion with org-switchb"
                                      (let ((completion-ignore-case t))
                                        (apply orig-fun args))))

  (eval-after-load 'parse-time
    '(setq parse-time-weekdays (append parse-time-weekdays
				       '(("søn" . 0)("man" . 1)("tir" . 2)("ons" . 3)
					     ("tor" . 4)("fre" . 5)("lør" . 6)
                         ("sø." . 0)("ma." . 1)("ti." . 2)("on." . 3)
                         ("to." . 4)("fr." . 5)("lø." . 6)
					     ("søndag" . 0)("mandag" . 1)("tirsdag" . 2)("onsdag" . 3)
					     ("torsdag" . 4)("fredag" . 5)("lørdag" . 6)))
	   
           parse-time-months (append parse-time-months
				     '(("januar" . 1)("februar" . 2)("mars" . 3)
				       ("april" . 4)("juni" . 6)("juli" . 7)
				       ("august" . 8)("september" . 9)("oktober" . 10)
				       ("november" . 11)("desember" . 12)
				       ("mai" . 5)("okt" . 10)("des" . 12))))))

(use-package ol-man
  :after org)

(use-package org-id
  :after org
  :init (setq org-id-locations-file (concat user-cache-directory "org-id-locations"))
  :bind ("C-c o i" . org-id-get-create))

;; custom.el
(when (file-readable-p custom-file) (load custom-file))

;; local.el
(when (file-readable-p local-init-file)
  (load local-init-file)
  (run-hooks 'local-init-file-after-hook))
