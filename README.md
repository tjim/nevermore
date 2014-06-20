# N E V E R M O R E

Nevermore is an experimental emacs email interface based on
[notmuch](http://notmuchmail.org/).  It provides:

* Interactive/autocompleting search
* Snooze 
* Junk mail filtering (via bogofilter)
* Edit tags with autocompletion

## Installation

First, install notmuch and run it once from the command line to
configure it.

Drop nm.el into your load-path and place this in your .emacs or
init.el file:

    (require 'nm)
