# N E V E R M O R E

Nevermore is an experimental emacs email interface for [Notmuch][].
It provides:

* Interactive search
* Snooze 
* Tag editing with autocompletion
* Mail address completion (via company mode)
* Junk mail filtering (via bogofilter)

## Installation

Nevermore is based on [Notmuch][], so you'll need to install Notmuch
(including emacs support), and run it once from the command line to
configure it.

Then drop nm.el into your load-path and place this in your .emacs or
init.el file:

    (require 'nm)

### Mail address completion

Nevermore uses company mode for mail address completion.  After
installing company mode, add

    (require 'company-nm)

to your init file.  Nevermore will automatically derive completion
candidates from your mail store, and display them as you type in a
To:, Cc:, or Bcc: field.  Note that company-mode has a setting,

    company-minimum-prefix-length

that controls the number of characters you must type before
autocompletion will take effect, and it defaults to three (3)
characters.  You may set this as low as 0 characters.

### Junk mail filtering

Nevermore uses bogofilter for junk mail filtering.  It works out of
the box as long as you have bogofilter installed.  Type `J` to mark a
thread or message as junk.

## Usage

Start reading mail with

    M-x nm

This will bring up a summary window with the results of the default
search, which is `tag:inbox`.  Nevermore can search for threads or
individual messages; by default it searches for individual messages.
You can toggle between thread search and message search by hitting
`M`.

You can navigate in the search results with the usual emacs commands
(`C-n`, `C-p`, `C-v`, `C->`, etc.)  You can also type `n` to move to
the next result or `p` to move to the previous result.

To view a message or thread, hit `SPC` or `RET`.  The message or
thread will appear in a window below the search results.  Hit `SPC` to
read more of the message, `DEL` to back up.  Hit `C-x 1` to get rid of
the message/thread window.

### Searching

To start a new search, hit `/`.  This will bring the current search to
the minibuffer, where you can edit it.  As you change the search, the
results buffer is updated in real time.  When you are satisfied with
the query, hit `RET` to get back into the results buffer.

The Notmuch query syntax is documented [here][syntax].

In the minibuffer you can navigate your search history with `M-p` and
`M-n`.

#### Focusing on a thread

In message search mode, there is a separate result for each message in
a thread.  Sometimes it's useful to see all of the messages of a
thread, while excluding messages from other threads; hit `T` to change
your search query to one that shows just those messages.

#### Search quirks

Nevermore's underlying query engine is [Xapian][].  Xapian's behaviour
during incremental search can take a little getting used to.  Xapian
uses *stemming* so that the queries

    look

and

    looking

are really the same query; they search for variants of look (look,
looked, looking, etc.) .  However,

    looki

is a different query that probably has no results, since it is not an
English word.  Therefore, if you search incrementally for "looking",
when you have typed just "look" you will get the full result set, but
when you go on and add an "i", you'll get an empty result set.
Eventually you will hit "looking" and you'll see the full result set
again.

You can use a wildcard character to avoid this behaviour:

    looki*

will search for "looki" and any extension of "looki".  One way to use
this is to start a query ending in "*", then back up a character
(C-b) and continue typing your query.  Note that

    looking*

can have *fewer* results than

    looking

because the later query uses stemming but the former does not.

### Sending mail

Hit `m` to send a new message.  In message search mode you can hit `r`
to reply, `R` to reply-all, and `f` to forward a message.  Nevermore
currently does not have a notion of the "current" message of thread,
so these commands don't make sense in thread search mode.

### Archiving

Hit `a` to archive a result, which simply moves it out of `tag:inbox`.

### Deleting

Hit `d` to delete a result, which simply moves it out of `tag:inbox`
and into `tag:deleted`.

### Tags

Tags are displayed at the right of every result.  You can edit the
tags in the minibuffer by hitting `t`.  You can autocomplete tags in
the minibuffer with `TAB`.

### Snooze

Nevermore has rudimentary support for snoozing messages and threads.
In the results buffer hit `s` to snooze a result.  The result will be
moved out of `tag:inbox` into `tag:later`.  Note that if you are
searching on `tag:inbox`, the result will not immediately disappear
from your search results; you need to re-run the search, which can be done by hitting `g`.

A snoozed result will be moved back to `tag:inbox` the following
morning.

Ultimately, the plan is for nevermore to have a way for you to specify
snooze targets other than the next morning.

[Notmuch]: http://notmuchmail.org/
[Xapian]: http://xapian.org/
[syntax]: http://notmuchmail.org/manpages/notmuch-search-terms-7/
