# My Personal Emacs Configuration

Author: Øyvind Stegard <oyvind@stegard.net>

Compatibility: GNU/Emacs 25.X, 26.X and 27.X, primarily for Linux env, also used
for plain terminal Emacs on WSL/Windows.

Utilizes `use-package`, the Emacs package framework and avoids the customize
facility, preferring to hand code all preferences in `init.el` and related
files. Any state saved by the Emacs customize framework is stored in
`custom.el`. The file `local.el` is loaded if present and can contain host
specific configuration.

In general, the configuration should boot strap Emacs by automatically
downloading and byte compiling any packages missing, so the first startup can
take some time.


# Code

## el/

Some reusable library code may be found under the `el` subdirectory of this
repository.

## bin/ec (emacsclient wrapper)

Emacsclient wrapper shell script for managing a single Emacs instance under X
and/or in daemon mode. I like to keep only a single Emacs instance running,
while at the same time using several frames/windows spread over multiple
desktops/viewports. To manage this the way I like, I made en emacsclient wrapper
script. It automatically starts Emacs if not already running, and it has a few
custom options and work-arounds for issues that have annoyed me.

The option --select-frame-dwim can be used to select/focus an Emacs frame on the 
currently visible X desktop/viewport, which is handy when opening files from a 
terminal window. It is also nice to use when emacsclient is launched by file 
manager, webbrowser, etc.. It avoids the file popping up in an Emacs frame on 
some other random desktop. If there is no frame on the current desktop/viewport, 
then a new frame is created. It can be configured to avoid selecting frames with 
names matching a regexp (see top of script). For other custom options, see 
--help.

Some examples of how I've integrated this on my desktop:

* Bound Win+E to launch emacsclient --create-frame in window manager.
* Bound Shift+Win+E (or "s-E") to function `delete-frame' in Emacs.
* Bound Ctrl+Win+E to launch emacsclient --select-frame-dwim in window 
  manager. This gives me a quick keyboard shortcut to focus an Emacs frame on 
  the current viewport.
* Use "emacsclient --select-frame-dwim %F" as the "Exec="-line in the Emacs 
  .desktop file.
* Set command emacsclient --mailto %u as defalt mailto: link handler.
* Bound Win+G to command emacsclient -n -e '(switch-to-gnus)' in window 
  manager. This uses emacsclient to call my custom Gnus startup function. (If 
  Gnus is already started, then the Gnus frame is activated, so window manager 
  will jump to it and give it focus.)

I use the following shell aliases:

    alias e='ec --select-frame-dwim'
    alias en='ec -n --select-frame-dwim'
    alias et='ec -t'
        

## el/gcontacts-get.el

A small library to fetch GMail/Google contacts over HTTP into Emacs. I wanted an
easy way to sync up my BBDB address book. The code is currently in beta state,
but works for me (tm).

v0.5-beta added support for OAuth2 authentication/authorization for Google API
access and package was renamed to avoid confusion with other similar Emacs
libraries.

v0.4-alpha added support for generating an [org-mode](http://orgmode.org/)
buffer (for
[org-contacts](http://julien.danjou.info/projects/emacs-packages#org-contacts))
from your Google-contacts.

Note that when using auth-source to get credentials, there must exist an entry
for www.google.com with port 443 and your email as username in auth source
database.

### Usage examples

    ;; Example of using google-contacts.el
    ;; Load library
    (load-file "/path/to/gcontacts-get.el")

    ;; Configure username/password to GMail
    (setq gcontacts-get-email "my-email@gmail.com")
    (setq gcontacts-get-passwd "secret")

    ;; Or leave `gcontacts-get-passwd' blank and use Emacs auth-source library instead:
    (setq gcontacts-get-email "my-email@gmail.com")
    (setq gcontacts-get-passwd nil)
    (setq gcontacts-get-passwd-use-auth-source t)
    ;; Note that auth-source has only been tested with Emacs 24. Mileage may vary for older versions.

    ;; Or leave both username and password blank and use OAuth2:
    (setq gcontacts-get-use-oauth2 t)

    ;; Retrieve Google contacts and put the resulting list in `my-contacts' variable:
    (setq my-google-contacts (gcontacts-get))

    ;; Use contents of `my-google-contacts' variable to whatever you need it
    ;; for. It's structure should be easy to understand. You can look at it by
    ;; doing a "C-h v my-google-contacts" to describe the variable.

    ;; You can merge the contacts into BBDB address book:
    (gcontacts-get-merge-with-bbdb my-google-contacts)

    ;; You can generate a buffer formatted as org-contacts:
    (gcontacts-get-generate-org-contacts my-google-contacts)

    ;; Here's an interactive function that retrieves Google contacts and updates
    ;; BBDB:
    (defun gcontacts-get-update-bbdb()
      "Fetch contacts from Google and merge them into local BBDB."
      (interactive)
      (gcontacts-get-merge-with-bbdb (gcontacts-get)))
    ;; Please read warning about function `gcontacts-get-merge-with-bbdb' in top
    ;; of gcontacts-get.el file.



## el/mailto-url-gnus.el

Mailto URL handler function for Gnus+message mode. Parses mailto-links and sets
up a new mail message. Supports most common fields including attachments. Can be
used to integrate Gnus as a regular mailto-handler:

    #!/bin/sh
    # Use Gnus as mailto: handler
    url="$1"

    type emacsclient 1>/dev/null 2>&1 || { echo "Error: emacsclient command is required."; exit 1; }

    exec emacsclient -a "" -n -c -e "(mailto-url-gnus \"$url\" t t)"

See source code for further documentation.


## el/nm.el

NetworkManager integration for Emacs. See source for doc.


## el/notify.el

Very simplistic Freedesktop notifications for Emacs. See source for doc.

*Deprecated*: Emacs now has built-in support for such notifications, and you 
should use that instead:
http://www.gnu.org/software/emacs/manual/html_node/elisp/Desktop-Notifications.html



## el/upower.el

Simple integration with upower daemon for Emacs. See source for doc.

I use this mainly to properly shut down any Gnus server connections on suspend 
of computer, since they tend to hang after resume otherwise.
