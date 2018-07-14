;;; gcontacts-get.el --- Library of functions to get contacts from Google APIs.
;; -*- coding: utf-8 -*-

;; Author: Øyvind Stegard <oyvind@stegard.net>
;; License: Public domain, use at own risk, no warranties of any kind.

;; This file is NOT part of GNU Emacs.

;;; Basic usage:
;; - Set variable `gcontacts-get-email' and one of the following:
;;   `gcontacts-get-passwd', `gcontacts-get-passwd-use-auth-source' or
;;   `gcontacts-get-use-oauth2'.
;; - Call function `gcontacts-get'.
;;   It returns a simple Lisp structure which should contain all your contacts.
;;
;; Functions are present both for integrating the retrieved contacts with BBDB
;; and generating a buffer in org-contacts format.
;;
;; Be careful with the `gcontacts-get-merge-with-bbdb' function. It will
;; replace all contacts you have in your BBDB address book for which there
;; exists a Google contact, with only data from the Google contact. This is
;; because I mostly edit contacts in GMail and just export to BBDB when
;; necessary, using this function. Also bear in mind that this is *beta* code,
;; tailored to my own needs.
;; ** BACKUP YOUR BBDB DATABASE BEFORE TESTING THIS FUNCTION **

;; See doc strings for more info on variables and functions.
;;
;; Emacs 24+ recommended. (Recent versions not tested on <=Emacs 23 or other
;; Emacs flavours.)

;;; Changelog:
;; * v0.6, 2016-08-10
;;   - Robustify extraction of data elements. Fixes errors caused by insignificant
;;     changes in data format from Google.
;; 
;; * v0.5-beta, 2013-06-24
;;   - Add support for OAuth2 for API access (requires oauth2 Emacs package to
;;     be installed).
;;   - Renamed package from 'google-contacts' to 'gcontacts-get',
;;     to avoid confusion with other Emacs packages in existence.
;; 
;; * v0.4-alpha, 2013-06-01
;;   - Add support for generating an org-mode buffer with org-contacts from
;;     retrieved Google contacts.
;;   - Extract contact web sites from Google contact data.
;;  
;; * v0.3-alpha, 2012-06-17
;;   Updated according to changes in Emacs24 auth source functionality and
;;   removed work-around for TLS hang, which does not occur with Emacs24's
;;   builtin TLS/SSL support.
;;
;; * v0.2.2-alpha, 2010-11-28
;;   Add explicit requirement on common lisp extensions.
;; 
;; * v0.2.1-alpha, 2010-10-13
;;   Fixed problem with contacts that have no emails and zero-length title/name
;;   string.


(require 'auth-source)
(require 'json)
(require 'url)
(eval-when-compile
  (require 'cl))

;; OAuth2 constants
(defconst gcontacts-get-oauth-client-ID "339844472976.apps.googleusercontent.com")
(defconst gcontacts-get-oauth-client-secret "e5Btw5n6NBPAUCde3Ar9BwFh")
(defconst gcontacts-get-oauth-scope-uris "https://www.google.com/m8/feeds/")
(defconst gcontacts-get-oauth-redirect-uri "urn:ietf:wg:oauth:2.0:oob")
(defconst gcontacts-get-oauth-auth-uri "https://accounts.google.com/o/oauth2/auth")
(defconst gcontacts-get-oauth-token-uri "https://accounts.google.com/o/oauth2/token")

;; Begin user variables:

(defvar gcontacts-get-email nil "GMail address. Not required when using oauth2, optional when using auth-source.")

(defvar gcontacts-get-passwd nil "GMail password. Not required when using oauth2 or auth-source.")

(defvar gcontacts-get-passwd-use-auth-source nil
  "If t, then look in default auth source for GMail credentials.
Entry must have host 'www.google.com' and port 443.")

(defvar gcontacts-get-use-oauth2 nil
  "If t, then use OAuth2 to authenticate and authorize Google API access,
instead of (now deprecated) Google \"ClientLogin\" method. When
using this method, a browser window will be opened, and you will
be required to authorize and allow access through Google's own
web pages before this library can get your contacts.

Requires package `oauth2'.")

(defvar gcontacts-get-max-results 1000
  "Maximum number of contacts to fetch from Google. Should be
some integer value higher than your total number of contacts.")

(defvar gcontacts-get-system-group-names
  '((friends . "Friends")
    (coworkers . "Coworkers")
    (family . "Family"))
  "Alist mapping Google fixed system group ids to your preferred local name (string)")

(defvar gcontacts-get-org-contacts-title "Google contacts"
  "Title used for generated org-mode file with Google contacts when using
function `gcontacts-get-generate-org-contacts'.")

(defvar gcontacts-get-org-contacts-category nil
  "Category used for generated org-mode file with Google contacts when
using function `gcontacts-get-generate-org-contacts'.")

;; End of user variables.

(defun gcontacts-get-url-retrieve (url session &optional extra-headers timeout)
  "Retrieves URL using whatever authentication that is
configured, optionally adding EXTRA-HEADERS to the request and
with timeout TIMEOUT (seconds). The variable SESSION should
satisfy `consp' and will be used to store session state between
calls to this function."
  (if gcontacts-get-use-oauth2
      (progn
        (require 'oauth2)
        (let ((token (or (car session)
                         (setcar session
                                 (oauth2-auth-and-store gcontacts-get-oauth-auth-uri
                                                        gcontacts-get-oauth-token-uri
                                                        gcontacts-get-oauth-scope-uris
                                                        gcontacts-get-oauth-client-ID
                                                        gcontacts-get-oauth-client-secret)))))
          (if (integerp timeout)
              (with-timeout (timeout (error (concat "gcontacts-get: timed out requesting URL: " url)))
                (oauth2-url-retrieve-synchronously token url nil nil extra-headers))
            (oauth2-url-retrieve-synchronously token url nil nil extra-headers))))
    ;; Use old ClientLogin method instead
    (let* ((token (or (car session) (setcar session (gcontacts-get-clientlogin))))
           (url-request-extra-headers (cons `("Authorization" . ,(concat "GoogleLogin auth=" token))
                                            extra-headers)))
      (if (integerp timeout)
          (with-timeout (timeout (error (concat "gcontacts-get: timed out requesting URL: " url)))
            (url-retrieve-synchronously url))
        (url-retrieve-synchronously url))
      )))
    
(defun gcontacts-get-credentials ()
  (let (email passwd)
    (if gcontacts-get-passwd-use-auth-source
        (let ((entry (nth 0 (auth-source-search :host "www.google.com"
                                                :port 443
                                                :user gcontacts-get-email
                                                :max 1))))
          (when (and entry (functionp (plist-get entry :secret)))
            (setq email (plist-get entry :user)
                  passwd (funcall (plist-get entry :secret)))))
      (setq email gcontacts-get-email passwd gcontacts-get-passwd))
    (if (and email passwd)
        (cons email passwd)
      (error "Username[/password] not set or not found in auth source for host:port 'www.google.com:443'."))))

(defun gcontacts-get-clientlogin ()
  "Login to Google contacts service, obtain auth cookie which is returned as a string."
  (let* ((creds (gcontacts-get-credentials))
         (email-encoded (url-hexify-string (car creds)))
         (passwd-encoded (url-hexify-string (cdr creds)))
         (url-request-method "POST")
         (url-request-extra-headers '(("Content-type" . "application/x-www-form-urlencoded")))
         (url-request-data (concat "Email=" email-encoded "&Passwd=" passwd-encoded "&service=cp&source=Emacs"))
         auth-cookie)
    (with-current-buffer
        (with-timeout (20 (error "gcontacts-get: ClientLogin authentication took too long, aborting"))
          (url-retrieve-synchronously "https://www.google.com/accounts/ClientLogin"))
      (goto-char (point-min))
      (re-search-forward "^Auth=\\(.*\\)" nil t)
      (setq auth-cookie (match-string-no-properties 1))
      (set-buffer-modified-p nil)
      (kill-buffer (current-buffer)))
    auth-cookie))

(defun gcontacts-get-all (session)
  "Fetch all contacts from Google and return as a parsed JSON object (Lisp structure)"
  (let (data)
    (with-current-buffer
        (gcontacts-get-url-retrieve
         (format "https://www.google.com/m8/feeds/contacts/default/full?alt=json&max-results=%d" gcontacts-get-max-results) session '(("GData-Version" . "3.0")) 20) ;; Use GData version 3 to get nick-names
      (declare (special url-http-end-of-headers))
      (set-buffer-multibyte t)
      (decode-coding-region (1+ url-http-end-of-headers) (point-max) 'utf-8)
      (goto-char (1+ url-http-end-of-headers))
      (setq data (json-read-object))
      (set-buffer-modified-p nil)
      (kill-buffer (current-buffer)))
    data))

(defun gcontacts-get-groups (session)
  (let (data)
    (with-current-buffer
        (gcontacts-get-url-retrieve
         (format "https://www.google.com/m8/feeds/groups/default/full?alt=json&max-results=%d" gcontacts-get-max-results) session '(("GData-Version" . "2.0")) 20) ;; Use GData version 2.0 to get system groups.                                                                    ;; They aren't returned with version 3.0, for some unknown reason.
      (declare (special url-http-end-of-headers))
      (set-buffer-multibyte t)
      (decode-coding-region (1+ url-http-end-of-headers) (point-max) 'utf-8)
      (goto-char (1+ url-http-end-of-headers))
      (setq data (json-read-object))
      (set-buffer-modified-p nil)
      (kill-buffer (current-buffer)))
    (unless data
        (error "Could not retrieve contact groups as JSON"))
    (let ((entries (cdr (assoc 'entry (assoc 'feed data))))
          groups-alist
          entrynode)
      (loop for entrynode across entries
            do
            (let ((title (cdr (assoc-string "$t" (cdr (assoc-string "title" entrynode)))))
                  (id (cdr (assoc-string "$t" (cdr (assoc-string "id" entrynode)))))
                  )
              (when (string-match "^System Group:" title)
                (cond
                 ((string-match "^System Group: My Contacts" title)
                  (setq title nil))
                 ((string-match "^System Group: Friends" title)
                  (setq title (cdr (assoc 'friends gcontacts-get-system-group-names))))
                 ((string-match "^System Group: Family" title)
                  (setq title (cdr (assoc 'family gcontacts-get-system-group-names))))
                 ((string-match "^System Group: Coworkers" title)
                  (setq title (cdr (assoc 'coworkers gcontacts-get-system-group-names)))))
               )
              (when title
                (setq groups-alist (cons (cons id title) groups-alist)))
              )
            )
      groups-alist)))

(defun gcontacts-get-normalize-whitespace(s)
  (and s (replace-regexp-in-string "^ \\| $" "" (replace-regexp-in-string "[ \t]+" " " s))))
  
;; Used for finding common email address(es) between two sets/bags in contact
;; matching between Google contacts and BBDB.
(defun gcontacts-get-intersection-ignore-case(list1 list2)
  (cond
   ((not list1) nil)
   ((member-ignore-case (car list1) list2)
    (cons (car list1) (gcontacts-get-intersection-ignore-case (cdr list1) list2)))
   (t (gcontacts-get-intersection-ignore-case (cdr list1) list2))))

;; Mappings from Google location type schema to symbol (used for addresses and phone numbers)
(setq gcontacts-get-location-schema-mapping
      '(("http://schemas.google.com/g/2005#main" . main)
        ("http://schemas.google.com/g/2005#work" . work)
        ("http://schemas.google.com/g/2005#home" . home)
        ("http://schemas.google.com/g/2005#mobile" . mobile)
        ("http://schemas.google.com/g/2005#other" . other)))

;; Get plain phone number structure from phone-node in JSON structure
(defun gcontacts-get-get-phone-numbers(phone-node) ; expect vector of alists as cdr
  (when phone-node
    (map 'list
         (lambda(phone-number)
           (let ((number (cdr (assoc '$t phone-number)))
                 (location (cdr (assoc-string (cdr (assoc 'rel phone-number))
                                          gcontacts-get-location-schema-mapping))))
             (when (not location) (setq location 'other))
             (cons location number)))
         (cdr phone-node))))

(defun gcontacts-get-get-websites(website-node) ; expect vector of alists as cdr
  (when website-node
    (map 'list
         (lambda(website)
           (let ((rel (cdr (assoc 'rel website)))
                 (href (cdr (assoc 'href website))))
             (unless rel (setq rel "other"))
             (cons rel href))) (cdr website-node))))

;; Get plain address structure from address-node in JSON structure
(defun gcontacts-get-get-addresses(address-node)
  (when address-node
    (map 'list
         (lambda(address)
           (let ((location (cdr (assoc-string (cdr (assoc 'rel address))
                                              gcontacts-get-location-schema-mapping)))
                 (formatted-address (cdr (assoc '$t (assoc 'gd$formattedAddress address)))))
             (when (not location) (setq location 'other))
             (cons location formatted-address)))
         (cdr address-node))))

;; Get list of group names (strings) from group membership node in JSON structure
(defun gcontacts-get-get-groups(group-membership-node groups-id-name-alist)
  (delete nil (mapcar (lambda(e)
                        (cdr (assoc-string (cdr (assoc 'href e)) groups-id-name-alist)))
                      (cdr group-membership-node))))

;; Get company name from organization-node in JSON structure
(defun gcontacts-get-get-company (organization-node)
  (when organization-node
    (setq organization-node (cdr organization-node))
    (when (> (length organization-node) 0)
      (cdr (assoc '$t (assoc 'gd$orgName (aref organization-node 0)))))))

;; Retrieves contacts from Google/GMail and returns simple Lisp structure.
(defun gcontacts-get ()
  "Returns Google contacts as alist"
  (let* (google-contacts-alist
         groups-id-name-alist
         (session (cons nil nil))
         (data (gcontacts-get-all session)))
    (unless data
      (error "Could not retrieve contacts data"))
    (setq groups-id-name-alist (gcontacts-get-groups session))
    (loop for contactnode across (cdr (assoc 'entry (assoc 'feed data)))
          do
          (let* ((email-node (assoc 'gd$email contactnode))
                 (title-node (assoc 'title contactnode)) ; consider using gd$fullName node instead
                 (address-node (assoc 'gd$structuredPostalAddress contactnode))
                 (phone-node (assoc 'gd$phoneNumber contactnode))
                 (organization-node (assoc 'gd$organization contactnode))
                 (nickname-node (assoc 'gContact$nickname contactnode))
                 (birthday-node (assoc 'gContact$birthday contactnode))
                 (group-membership-node (assoc 'gContact$groupMembershipInfo contactnode))
                 (content-node (assoc 'content contactnode)) ; contact notes-field
                 (website-node (assoc 'gContact$website contactnode))
                 
                 (name
                  (and title-node (gcontacts-get-normalize-whitespace (cdr (assoc-string '$t title-node)))))
                 (emails
                  (and email-node (map 'list (lambda(e) (cdr (assoc-string 'address e))) (cdr email-node))))

                 contact addresses
                 phone-numbers company websites
                 nickname birthday groups notes)
            ;; Ignore all contacts with no emails and no name:
            (when (or emails (and name (> (length name) 0)))
              (setq phone-numbers (gcontacts-get-get-phone-numbers phone-node))
              (setq company (gcontacts-get-get-company organization-node))
              (setq websites (gcontacts-get-get-websites website-node))
              (setq addresses (gcontacts-get-get-addresses address-node))
              (setq nickname (cdr (assoc-string '$t nickname-node)))
              (setq birthday (cdr (assoc-string 'when birthday-node)))
              (setq groups (gcontacts-get-get-groups group-membership-node groups-id-name-alist))
              (setq notes (cdr (assoc '$t (cdr content-node))))
              (when (and nickname (> (length nickname) 0))
                (setq contact (cons (cons 'aka nickname) contact)))
              (when birthday
                (setq contact (cons (cons 'birthday birthday) contact)))
              (when notes
                ;; HTC Android phones store additional tagged metadata in the notes field, strip that away.
                (setq notes (replace-regexp-in-string "<HTCData>\\(.\\|\n\\)*?</HTCData>" "" notes))
                (setq notes (replace-regexp-in-string "\\`\\( \\|\n\\|\t\\)+\\|\\( \\|\n\\|\t\\)+\\'" "" notes))
                (when (> (length notes) 0)
                  (setq contact (cons (cons 'notes notes) contact))))
              (when addresses
                (setq contact (cons (cons 'formatted-addresses addresses) contact)))
              (when phone-numbers
                (setq contact (cons (cons 'phone-numbers phone-numbers) contact)))
              (when company
                (setq contact (cons (cons 'company company) contact)))
              (when websites
                (setq contact (cons (cons 'websites websites) contact)))
              (when groups
                (setq contact (cons (cons 'groups (list groups)) contact)))
              (when emails
                (setq contact (cons (cons 'emails (list emails)) contact)))
              (when (and name (> (length name) 0))
                (setq contact (cons (cons 'name name) contact)))
              
              (setq google-contacts-alist (cons contact google-contacts-alist)))))
    google-contacts-alist))

(defun gcontacts-get-groups-to-org-tag-string (groups)
  "Generate a colon-separated list of org-mode tags from GROUPS.
Each tag name is normlized and lower cased."
  (when groups
    (let ((normalized-sorted-groups (sort (mapcar 
                                            (lambda(group)
                                             (concat ":"
                                               (downcase (replace-regexp-in-string "[^a-zA-Z_@]" "_" group))))
                                           groups) 'string-lessp)))
      (concat (apply 'concat normalized-sorted-groups) ":"))))

(defun gcontacts-get-generate-org-contacts(google-contacts &optional to-buffer-or-name)
  "Creates entries for all GOOGLE-CONTACTS, with format as
expected by org-contacts, in an org-mode buffer. Either a new
buffer is created or supplied TO-BUFFER-OR-NAME is used (any existing
contents will be erased)."
  (require 'org-contacts)
  (let ((buf (get-buffer-create (or to-buffer-or-name "*Google-contacts*"))))
    (with-current-buffer buf
      (erase-buffer)
      (set-buffer-file-coding-system 'utf-8)
      (insert "# -*- mode: org; coding: utf-8 -*-\n")
      (insert (concat "#+TITLE: " gcontacts-get-org-contacts-title "\n"))
      (insert (concat "#+EMAIL: " gcontacts-get-email "\n"))
      (when gcontacts-get-org-contacts-category
        (insert (concat "#+CATEGORY: " gcontacts-get-org-contacts-category "\n")))
      (insert "#+STARTUP: showeverything\n\nGenerated by `gcontacts-get-generate-org-contacts'.\nBe careful – manual modifications may be lost.\n\n")
      (while google-contacts
        (let* ((contact (car google-contacts))
               (emails (cadr (assoc 'emails contact)))
               (firstemail (car emails))
               (name (or (cdr (assoc 'name contact))
                         (substring firstemail 0 (string-match "@" firstemail))
                         firstemail))
               (aka (cdr (assoc 'aka contact)))
               (company (cdr (assoc 'company contact)))
               (birthday (cdr (assoc 'birthday contact)))
               (groups (cadr (assoc 'groups contact)))
               (phone-numbers-alist (cdr (assoc 'phone-numbers contact)))
               (websites-alist (cdr (assoc 'websites contact)))
               (addresses-alist (cdr (assoc 'formatted-addresses contact)))
               (contact-notes (cdr (assoc 'notes contact))))
          (insert (concat "* " name))
          (when groups
            (insert (concat "   " (gcontacts-get-groups-to-org-tag-string groups))))
          (insert "\n")
          
          (insert ":PROPERTIES:\n")
          
          (when emails
            (insert (concat ":" org-contacts-email-property ": "))
            (insert (mapconcat 'identity emails " "))
            ;; (dolist (email emails) (insert (concat email " ")))
            (insert "\n"))

          (when phone-numbers-alist
            (insert (concat ":" org-contacts-tel-property ": "
                            (mapconcat (lambda(type-number)
                                         (replace-regexp-in-string "[ 	]" "" (cdr type-number)))
                                       phone-numbers-alist "  ") "\n")))

          (when addresses-alist ; first address only (regardless of type)
            (insert (concat ":" org-contacts-address-property ": "
                            (replace-regexp-in-string "\n" ";" (cdr (car addresses-alist))) "\n")))
          
          (when aka
            (insert (concat ":" org-contacts-alias-property ": " aka "\n")))
          
          (when company
            (insert (concat ":ORGANIZATION: " company "\n")))
          
          (when birthday
            (insert (concat ":" org-contacts-birthday-property ": " birthday "\n")))

          (when websites-alist
            (insert ":WEB: ")
            (dolist (site websites-alist)
              (insert (concat (cdr site) " ")))
            (insert "\n"))

          (when contact-notes
            (insert (concat ":" org-contacts-note-property ": "
                            (replace-regexp-in-string "\n" "\\n" contact-notes nil t) "\n")))
          
          (insert ":END:\n")
          (insert "\n")

          (when websites-alist
            (insert (concat (if org-odd-levels-only "*** Links\n" "** Links\n")))
            (dolist (site websites-alist)
              (insert (concat "[[" (cdr site) "][" (capitalize (car site)) "]]\n"))))
          
          (when addresses-alist
            (dolist (type-address addresses-alist)
              (insert (concat (if org-odd-levels-only "*** Address " "** Address ") (symbol-name (car type-address)) "\n"))
              (insert (concat (cdr type-address) "\n"))))

          (when contact-notes
            (insert  (concat (if org-odd-levels-only "*** Notes" "** Notes")
                             "\n" contact-notes "\n"))))
        (setq google-contacts (cdr google-contacts)))
      (org-mode)
      (goto-char (point-min))
      (org-sort-entries nil ?a)
      (org-align-all-tags)
      (org-show-block-all))))


(defun gcontacts-get-to-org-contacts-file(google-contacts filename)
  "Create org-contacts from GOOGLE-CONTACTS like
`google-contacts-generate-org-contacts' except the output is
written directly to file FILENAME. Note that the file FILENAME
will be modified outside of any buffer which may be visiting it.
Such buffers must be reverted to reflect newly generated contents
in underlying file."
  (with-temp-file filename
    (gcontacts-get-generate-org-contacts google-contacts (current-buffer))))

(defun gcontacts-get-merge-with-bbdb(google-contacts)
  "Merges Google contacts in `google-contacts' to local BBDB address book."
  (require 'bbdb)
  (while google-contacts
    (let* ((contact (car google-contacts))
           (emails (cadr (assoc 'emails contact)))
           (firstemail (car emails))
           (name (or (cdr (assoc 'name contact))
                     (substring firstemail 0 (string-match "@" firstemail))
                     firstemail))
           (aka (cdr (assoc 'aka contact)))
           (company (cdr (assoc 'company contact)))
           (birthday (cdr (assoc 'birthday contact)))
           (groups (cadr (assoc 'groups contact)))
           (phone-numbers-alist (cdr (assoc 'phone-numbers contact)))
           (addresses-alist (cdr (assoc 'formatted-addresses contact)))
           (contact-notes (cdr (assoc 'notes contact)))
           (case-fold-search t)
           (records (bbdb-records))
           notes phones addresses new-record)
      (while records
        ;; delete any matching records first
        (when (or (and name (string-match (concat "^" name "$") (bbdb-record-name (car records))))
                  (gcontacts-get-intersection-ignore-case emails (bbdb-record-net (car records))))
          (bbdb-delete-record-internal (car records)))
        (setq records (cdr records)))

      ;; Add any nick-name/AKA to mail alias/groups
      (when aka
        (setq groups (cons aka groups)))
      
      ;; Create BBDB notes field with mail-alias and possibly anniversary (birthday)
      (when (and groups emails) ;; Only populate mail-alias field if contact actually has email address(es).
        (setq notes (cons (cons 'mail-alias (mapconcat 'identity groups ", ")) notes)))

      (when contact-notes
        (setq notes (cons (cons 'notes contact-notes) notes)))

      (when phone-numbers-alist
        (setq phones (map 'list (lambda(phone)
                                  (vector (capitalize (symbol-name (car phone))) (cdr phone)))
                          phone-numbers-alist)))

      (when addresses-alist
        (setq addresses (map 'list (lambda(address)
                                     (vector (capitalize (symbol-name (car address))) (split-string (cdr address) "\n")
                                             "" "" "" ""))
                             addresses-alist)))
      
      (when birthday
        (setq notes (cons (cons 'anniversary (concat birthday " birthday")) notes)))
      
      ;; Create new record
      (setq new-record (bbdb-create-internal name company (mapconcat 'identity emails ", ")
                                             addresses phones notes))
      ;; Add a real BBDB AKA field as well
      (when aka
        (bbdb-record-set-aka new-record (list aka))
        (bbdb-change-record new-record nil))
      )
      (setq google-contacts (cdr google-contacts))
    )
  (bbdb-resort-database)
  (bbdb-save-db)
  (message "Updated BBDB with Google contacts for %s" (car (gcontacts-get-credentials)))
  )


(defun gcontacts-get-update-bbdb()
  "Fetch contacts from Google and merge them into local BBDB."
  (interactive)
  (gcontacts-get-merge-with-bbdb (gcontacts-get)))

(provide 'gcontacts-get)
