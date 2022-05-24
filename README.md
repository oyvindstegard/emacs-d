# My Personal Emacs Configuration

Author: Øyvind Stegard <oyvind@stegard.net>

Compatibility: GNU/Emacs 27.X and 28.X, primarily for Linux env, also used for
plain terminal Emacs on WSL/Windows.

Utilizes `use-package`, the Emacs package framework and avoids the customize
facility, preferring to hand code all preferences in `init.el` and related
files. Any state saved by the Emacs customize framework is stored in
`custom.el`. The file `local.el` is loaded if present and can contain host
specific configuration.

In general, the configuration should boot strap Emacs by automatically
downloading and byte compiling any packages missing, so the first startup can
take some time. Currently, an exception to this is the `org` package, which must
be installed explicitly from `package-list-packages`.

## Cache directory

Instead of cluttering up the root of `~/.emacs.d/`, I try to ensure all packages
write various cache files and other persistent state to `~/.emacs.d/cache/`
instead, which also makes [`.gitignore`](.gitignore) simpler.

## Managing Emacs on graphical desktop

I highly recommend running only a single instance of Emacs on the desktop. These
days, it's as simple as always starting Emacs with:

    emacsclient --alternate-editor= --create-frame ...
    
This command automatically starts Emacs as a daemon, if it's not already
running, and pops up an new frame. The key is the rather strange
`--alternate-editor=` argument, which is documented
[here](https://www.gnu.org/software/emacs/manual/html_node/emacs/emacsclient-Options.html):

> As a special exception, if command is the empty string, then emacsclient
> starts Emacs in daemon mode (as ‘emacs --daemon’) and then tries connecting
> again.

Emacs 28 also ships with a desktop file that does this for you, named "Emacs (client)":

    /usr/share/applications/emacsclient28.desktop


## [`early-init.el`](early-init.el) vs [`init.el`](init.el)

I strive to keep [`early-init.el`](early-init.el) as small as possible, keeping
only the most basic settings and boot strapping code in this file. I also try to
optimize startup time, primarily with `use-package` and always preferring lazy
load, but also by byte compiling some personal library code and the init file
itself.

## Code under [`el/`](el/)

Some reusable library code may be found under the [`el/`](el/) subdirectory of
this repository.

### [`el/tod.el`](el/tod.el)

Functions dealing with time of day. I use these as part of my personal work time
logging system.

### [`el/upower.el`](el/upower.el)

Simple integration with upower daemon for Emacs. See source for doc.

I use this mainly to properly shut down various network connections (such as
TRAMP) on suspend of computer, since they tend to hang after resume otherwise.

## [`el/nm.el`](el/nm.el)

NetworkManager integration for Emacs. See source for doc.

## [`el/notify.el`](el/notify.el)

Very simplistic Freedesktop notifications for Emacs. See source for doc.

*Deprecated*: Emacs now has built-in support for such notifications, and you 
should use that instead:
http://www.gnu.org/software/emacs/manual/html_node/elisp/Desktop-Notifications.html


### el/mailto-url-gnus.el

*Deprecated: I stopped using Gnus for e-mail a long time ago*

Mailto URL handler function for Gnus+message mode. Parses mailto-links and sets
up a new mail message. Supports most common fields including attachments. Can be
used to integrate Gnus as a regular mailto-handler:

    #!/bin/sh
    # Use Gnus as mailto: handler
    url="$1"

    type emacsclient 1>/dev/null 2>&1 || { echo "Error: emacsclient command is required."; exit 1; }

    exec emacsclient -a "" -n -c -e "(mailto-url-gnus \"$url\" t t)"

See source code for further documentation.


### el/gcontacts-get.el

*Deprecated: I haven't tested gcontacts-get in ages, and it is likely broken.*

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



