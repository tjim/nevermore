;;; nm.el --- N E V E R M O R E: an experimental email interface for Notmuch -*- lexical-binding: t -*-

;; Copyright (C) 2014 Trevor Jim
;; Author: Trevor Jim
;; Maintainer: Trevor Jim
;; URL: https://github.com/tjim/nevermore
;; Version: 1.0.0
;; Package-Requires: ((notmuch "0.18") (peg "0.6") (company "0") (emacs "24"))

;; This file is not part of GNU Emacs.

;;; Commentary
;; Nevermore is an Emacs mail application with
;; * Incremental search by message or thread
;; * Snooze
;; * Junk filtering
;; * Mail address completion

;;; * TODO update mail completion addresses on message send
;;; * TODO mail defer queue
;;; * TODO snooze/wake UI
;;; * TODO diary integration
;;; * TODO triage (http://rowansimpson.com/2013/04/16/triage/)

(require 'notmuch)
(require 'notmuch-lib)
(require 'notmuch-mua)
(require 'nm-dateparse)

;; Customization

(defgroup nm nil
  "Emacs Nm mode."
  :group 'local)

(defgroup nm-faces nil
  "Faces used in Nm mode"
  :group 'nm
  :group 'faces)

(defface nm-header-face
  '((t :inherit font-lock-builtin-face :bold t :underline t))
  "Face for Nm header."
  :group 'nm-faces)

(defface nm-query-face
  '((t :inherit font-lock-string-face))
  "Face for Nm query string."
  :group 'nm-faces)

(defface nm-separator-face
  '((t :inherit font-preprocessor-face))
  "Face for Nm separator string."
  :group 'nm-faces)

(defface nm-unread-face
  '((t :inherit font-lock-builtin-face :bold t))
  "Face for Nm unread subjects."
  :group 'nm-faces)

(defface nm-read-face
  '((t :inherit font-lock-builtin-face))
  "Face for Nm read subjects."
  :group 'nm-faces)

(defface nm-authors-face
  '((t :inherit font-lock-builtin-face :italic t))
  "Face for Nm authors."
  :group 'nm-faces)

(defface nm-date-face
  '((t :inherit font-lock-type-face :bold t))
  "Face for Nm dates."
  :group 'nm-faces)

(defface nm-tags-face
  '((t :inherit font-lock-string-face :bold t))
  "Face for Nm tags."
  :group 'nm-faces)

(defvar nm-results-window-size 8
  "Number of lines of search results to show when viewing both results and a thread or message")

(defvar nm-separator " | "
  "Text used to separate fields.")

(defvar nm-empty-subject "[No subject]"
  "Text to use as subject when missing.")

(defvar nm-empty-title "[No title]"
  "Text to use as title when missing.")

(defvar nm-empty-authors "[No authors]"
  "Text to use as authors when missing.")

(defvar nm-empty-date "[No date]"
  "Text to use as date when missing.")

(defvar nm-default-query "tag:inbox ")

(defvar nm-date-width 12
  "Width of dates in Nm buffer.")

(defvar nm-authors-width 20
  "Width of authors in Nm buffer.")

(defvar nm-mode-hook nil
  "Hook run when entering Nm mode.")

(defvar nm-sort-order 'newest-first) ; or 'oldest-first

;; Constants

(defconst nm-version "1.0")

(defconst nm-results-buffer "*nm*"
  "Nm buffer name.")

(defconst nm-view-buffer "*nm-view*"
  "Nm view buffer name.")

;; State variables

(defvar nm-query nm-default-query
  "The current query whose results are in the nm-results-buffer.")

(defvar nm-results nil
  "Dynamic array of query results for nm-query, in sexp form.")

(defvar nm-view-buffer-contents-query nil
  "The current query whose contents are in the nm-view-buffer.")

(defvar nm-all-results-count nil
  "Count of all results for the current query.")

(defvar nm-query-mode 'message) ; or 'thread

;; Helpers

(defun nm-dynarray-create ()
  (plist-put
   (plist-put '() :length 0)
   :vector (make-vector 11 nil)))

(defun nm-dynarray-length (arr)
  (when arr
    (plist-get arr :length)))

(defun nm-dynarray-append (arr obj)
  (when arr
    (let ((len (plist-get arr :length))
          (vector (plist-get arr :vector)))
      (if (< len (length vector))
                                        ; there is room in the array
          (progn
            (aset vector len obj)
            (plist-put arr :length (1+ len)))
                                        ; there is no room in the array, double it and try again
        (let ((vector2 (vconcat vector (make-vector len nil))))
          (nm-dynarray-append (plist-put arr :vector vector2) obj))))))

(defun nm-dynarray-get (arr index)
  (when arr
    (let ((len (plist-get arr :length))
          (vector (plist-get arr :vector)))
      (if (and (<= 0 index) (< index len))
          (aref vector index)
        (error "nm-dynarray-get: out of bounds (length %d, index %d)" len index)))))

(defun nm-dynarray-set (arr index obj)
  (when arr
    (let ((len (plist-get arr :length))
          (vector (plist-get arr :vector)))
      (if (and (<= 0 index) (< index len))
          (aset vector index obj)
        (error "nm-dynarray-set: out of bounds")))))

(defun nm-thread-mode ()
  (equal nm-query-mode 'thread))

(defun nm-toggle-sort-order ()
  (interactive)
  (if (eq nm-sort-order 'oldest-first)
      (setq nm-sort-order 'newest-first)
    (setq nm-sort-order 'oldest-first))
  (nm-refresh))

(defun nm-toggle-query-mode ()
  (interactive)
  (pcase nm-query-mode
    (`jotmuch nil)
    (`thread (setq nm-query-mode 'message)
             (nm-refresh))
    (`message (setq nm-query-mode 'thread)
              (nm-refresh))))

(defun nm-call-notmuch (&rest args)
  "Invoke `notmuch-command' with `args' and return the output.

If notmuch exits with a non-zero status, this will pop up a
buffer containing notmuch's output and signal an error."
  (with-temp-buffer
    (let ((err-file (make-temp-file "nm-error")))
      (unwind-protect
	  (let ((status (apply #'call-process
			       notmuch-command nil (list t err-file) nil
                               (car args) ; search, show, count, etc.
                               "--format=sexp"
                               "--format-version=1"
                               (cdr args))))
	    (notmuch-check-exit-status status (cons notmuch-command args)
				       (buffer-string) err-file)
	    (goto-char (point-min))
            (read (current-buffer)))
	(delete-file err-file)))))

(defun nm-my-addresses ()
  "Obtain my email addresses from notmuch."
  (with-temp-buffer
    (let ((err-file (make-temp-file "nm-error")))
      (unwind-protect
	  (let* ((args '("config" "get" "user.primary_email"))
                 (status (apply #'call-process
                                notmuch-command nil (list t err-file) nil
                                args)))
	    (notmuch-check-exit-status status (cons notmuch-command args)
				       (buffer-string) err-file)
            (let* ((args2 '("config" "get" "user.other_email"))
                   (status2 (apply #'call-process
                                   notmuch-command nil (list t err-file) nil
                                   args2)))
              (notmuch-check-exit-status status2 (cons notmuch-command args2)
                                         (buffer-string) err-file)
              (split-string (buffer-string) "\n+" t)))
	(delete-file err-file)))))

(defun nm-chomp (str)
  "Trim leading and trailing whitespace from STR."
  (replace-regexp-in-string "\\(^[[:space:]\n]*\\|[[:space:]\n]*$\\)" "" str))

(defun nm-truncate-or-pad (width str)
  (let ((len (length str)))
    (if (> width len)
        ; pad
        (concat str
                (format (format "%%%ds" (- width len)) ""))
      ; truncate
      (format (format "%%.%ds" width) str))))

;; Display

(defvar nm-problem-before nil)

(defun nm-sanitize (s)
  (if s
      (notmuch-sanitize s)
    (message "nm-sanitize: nil")
    s))

(defun nm-result-line (result)
  "Return a line of text for a RESULT."
  (when (and result (not nm-problem-before) (not (plist-get result :date_relative)))
    (setq nm-problem-before t)
    (message "Problem: %S" result))
  (when result
    (let* ((date (plist-get result :date_relative))
           (authors (plist-get result :authors))
           (subject (nm-sanitize (plist-get result :subject)))
           (tags (plist-get result :tags)))
      (concat
       (propertize
        (nm-truncate-or-pad nm-date-width
                            (if date date
                              nm-empty-date))
        'face 'nm-date-face)
       (propertize nm-separator 'face 'nm-separator-face)
       (propertize
        (nm-truncate-or-pad nm-authors-width
                            (if authors
                                  (if (nm-thread-mode)
                                      authors
                                    (car (split-string authors "|")))
                              nm-empty-authors))
        'face 'nm-authors-face)
       (propertize nm-separator 'face 'nm-separator-face)
       (propertize
        (if (and subject (> (length subject) 0)) subject
          nm-empty-subject)
        'face (if (member "unread" tags) 'nm-unread-face 'nm-read-face))
       (when tags
         (propertize
          (format " (%s)" (mapconcat 'identity tags " "))
          'face 'nm-tags-face))))))

(defun nm-insert-result (result)
  "Add a line to the result browser for the given RESULT."
  (when result
    (insert (nm-result-line result) "\n")))

(defun nm-draw-header ()
  (let ((inhibit-read-only t))
    (setq header-line-format
          (concat
	   (pcase nm-query-mode
	     (`jotmuch
	      (propertize "Jotmuch search" 'face 'nm-header-face))
	     (`thread
	      (propertize "Thread search" 'face 'nm-header-face))
	     (_ ; default => `message
	      (propertize "Message search" 'face 'nm-header-face)))
           ": "
           (propertize (nm-chomp nm-query) 'face 'nm-query-face)))))

;;; Needs to be installed as a post-command-hook for the nm-results-buffer.
;;; Ensures that the cursor doesn't get past the last line of the results,
;;; which would cause undesired scrolling.
(defvar nm-results-overlay nil
  "Overlay used to highlight the current result.")
(put 'nm-results-overlay 'permanent-local t)
(defun nm-highlight-result ()
  ;; Make sure we have an overlay to use.
  (or nm-results-overlay
      (progn
        (make-local-variable 'nm-results-overlay)
        (setq nm-results-overlay (make-overlay (point) (point)))
        (overlay-put nm-results-overlay 'nm-results t)))
  (move-overlay nm-results-overlay
                (line-beginning-position)
                (line-beginning-position 2))
  (overlay-put nm-results-overlay 'face 'highlight))

(defun nm-view-buffer-update ()
  "Make sure the view window (if it exists) is showing the current query."
  (let ((nm-view-buffer-window (get-buffer-window nm-view-buffer)))
    (when nm-view-buffer-window
      (nm-apply-to-result (lambda (q)
                            (when (not (equal q nm-view-buffer-contents-query))
                              (nm-show-messages q nil)))))))

(defun nm-results-post-command ()
  (if (eobp)
      (forward-line -1))
  (nm-highlight-result)
  (nm-view-buffer-update))

(defun nm-result-wrangler (handler &optional expect-sequence)
  "Return a function that parses a process output into individual results and applies HANDLER to each result.
Without EXPECT-SEQUENCE, assumes that the process output comes as a LISP list, with each list element being a result.
If EXPECT-SEQUENCE then assumes that the process output is a sequence of LISP objects, concatenated."
  (let ((pending-output (if expect-sequence "" nil))) ; NB pending-output must be lexically bound!!!
    (lambda (proc string)
      (unless pending-output
        ; nil => this is the first output, and we are expecting a list of results as a single sexp, so skip the initial left paren
        (setq string (substring string 1))
        (setq pending-output ""))
      (setq pending-output (concat pending-output string))
      (while
          (let ((result (ignore-errors (read-from-string pending-output))))
            (and result
                 (let ((obj (car result))
                       (offset (cdr result)))
                   (setq pending-output (substring pending-output offset))
                   (funcall handler obj)
                   t)))))))

(defun nm-kill (async-proc)
  (when async-proc
    (set-process-filter async-proc nil) ; kill-process is async so prevent handling stale results
    (ignore-errors (kill-process async-proc))
    (setq async-proc nil)))

(defvar nm-async-search-pending-proc nil)   ; the process of a search underway
(defun nm-async-search ()
  "Perform an asynchronous search on nm-query, displaying results in nm-results-buffer and storing sexps in nm-results"
  (nm-kill nm-async-search-pending-proc)
  (setq nm-results (nm-dynarray-create))
  (pcase nm-query-mode
    (`thread
     (setq nm-async-search-pending-proc
           (notmuch-start-notmuch
            "nm-async-search" ; process name
            nil               ; process buffer
            nil               ; process sentinel
            "search"          ; notmuch command
            "--format=sexp"
            "--format-version=2"
            "--output=summary"
            (if (eq nm-sort-order 'oldest-first) "--sort=oldest-first" "--sort=newest-first")
            nm-query))
     (set-process-filter nm-async-search-pending-proc (nm-result-wrangler 'nm-async-search-thread-result)))
    (`message
     (setq nm-async-search-pending-proc
           (notmuch-start-notmuch
            "nm-async-search" ; process name
            nil               ; process buffer
            nil               ; process sentinel
            "show"            ; notmuch command
            "--format=sexp"
            "--format-version=2"
            "--body=false"
            "--entire-thread=false"
                                        ; (if (eq nm-sort-order 'oldest-first) "--sort=oldest-first" "--sort=newest-first") ; not allowed for notmuch show
            nm-query))
     (set-process-filter nm-async-search-pending-proc (nm-result-wrangler 'nm-async-search-message-result)))
    (`jotmuch
     (setq nm-async-search-pending-proc
           (start-process
            "nm-async-search" ; process name
            nil               ; process buffer
            "jot"
            "search"
            "--format=(:id \"{{id}}\" :title \"{{title}}\" :url \"{{url}}\" :tags ({% for t in tags %} \"{{t}}\"{% endfor %}))"
            nm-query))
     (set-process-filter nm-async-search-pending-proc (nm-result-wrangler 'nm-async-search-jotmuch-result t)))))

(defun nm-async-search-thread-result (result)
  (when (and result (get-buffer nm-results-buffer))
    (with-current-buffer nm-results-buffer
      (save-excursion
        (goto-char (point-max))
        (let ((inhibit-read-only t))
          (insert (nm-result-line result) "\n"))
        (setq nm-results (nm-dynarray-append nm-results result))))))

(defun nm-async-search-message-result (obj)
  (let* ((msgs (nm-flatten-forest (list obj)))
         (results (mapcar
                   (lambda (msg)
                     `(:subject ,(plist-get (plist-get msg :headers) :Subject)
                                :authors ,(plist-get (plist-get msg :headers) :From)
                                :date_relative ,(plist-get msg :date_relative)
                                :tags ,(plist-get msg :tags)
                                :id ,(plist-get msg :id)))
                   msgs)))
    (with-current-buffer nm-results-buffer
      (save-excursion
        (goto-char (point-max))
        (let ((inhibit-read-only t))
          (mapc
           (lambda (result)
             (insert (nm-result-line result) "\n")
             (setq nm-results (nm-dynarray-append nm-results result)))
           results))))))

(defun nm-async-search-jotmuch-result (result)
  (when (and result (get-buffer nm-results-buffer))
    (with-current-buffer nm-results-buffer
      (save-excursion
        (goto-char (point-max))
        (let ((inhibit-read-only t))
          (insert (nm-jotmuch-result-line result) "\n"))
        (setq nm-results (nm-dynarray-append nm-results result))))))

(defun nm-jotmuch-result-line (result)
  "Return a line of text for a jotmuch RESULT."
  (when result
    (let* ((title (plist-get result :title))
           (url (plist-get result :url))
           (tags (plist-get result :tags)))
      (concat
       (propertize
        (cond
         ((and title (> (length title) 0)) title)
         ((and url (> (length url) 0)) url)
         (t nm-empty-title))
        'face 'nm-read-face)
       (when tags
         (propertize
          (format " (%s)" (mapconcat 'identity tags " "))
          'face 'nm-tags-face))))))

(defun nm-bury ()
  "Bury the current nevermore buffers."
  (interactive)
  (let ((w (get-buffer-window nm-view-buffer)))
    (when w
      (quit-window nil w)))
  (let ((w (get-buffer-window nm-results-buffer)))
    (when w
      (quit-window nil w))))

(defun nm-log (x &rest args)
  (let ((buffer (get-buffer-create "*nm-log*")))
    (with-current-buffer buffer
      (insert (apply 'format x args))
      (insert "\n"))))

;; maintain count
(defun nm-setq-mode-name (s)
  (when (get-buffer nm-results-buffer)
    (with-current-buffer nm-results-buffer
      (setq mode-name s)
      (force-mode-line-update))))
(defvar nm-async-count-pending-proc nil)
(defun nm-async-count ()
  (nm-kill nm-async-count-pending-proc)
  (nm-setq-mode-name "nevermore")
  (setq nm-all-results-count nil)
  (unless (equal nm-query-mode 'jotmuch)
    (setq nm-async-count-pending-proc
	  (notmuch-start-notmuch
	   "nm-async-count" ; process name
	   nil              ; process buffer
	   nil              ; process sentinel
	   "count"          ; notmuch command
	   (if (nm-thread-mode)
	       "--output=threads"
	     "--output=messages")
	   nm-query))
    (set-process-filter nm-async-count-pending-proc (nm-result-wrangler 'nm-async-count-result t))))

(defun nm-async-count-result (obj)
  (setq nm-all-results-count obj)
  (if nm-all-results-count
      (let ((results (cond ((eq nm-all-results-count 1) "1 result")
                           ((eq nm-all-results-count 0) "no results")
                           (t (format "%d results" nm-all-results-count)))))
        (nm-setq-mode-name (format "nevermore: %s" results)))
    (nm-setq-mode-name "nevermore")))

(defun nm-refresh ()
  "(Re)apply the query and refresh the *nm* buffer."
  (interactive)
  (when (get-buffer nm-results-buffer)
    (with-current-buffer nm-results-buffer
      (let ((inhibit-read-only t))
        (erase-buffer))
      (nm-draw-header)
      (nm-async-count)
      (nm-async-search))))

(defun nm-result-index-at-pos ()
  (and nm-results
       (> (nm-dynarray-length nm-results) 0)
       (- (line-number-at-pos) 1)))

(defun nm-result-at-pos ()
  (let ((index (nm-result-index-at-pos)))
    (when index
      (nm-dynarray-get nm-results index))))

(defun nm-flatten-forest (forest)
;;  (display-message-or-buffer (format "Before: %S" forest))
  (let ((result
         (apply 'append
                (mapcar 'nm-flatten-thread forest))))
;;    (display-message-or-buffer (format "After: %S" result))
    result))

(defun nm-flatten-thread (thread)
  (apply 'append
         (mapcar 'nm-flatten-tree thread)))

(defun nm-flatten-tree (tree)
  (let ((msg (car tree))
	(replies (cadr tree)))
    (if msg
        (cons msg (nm-flatten-thread replies))
      (nm-flatten-thread replies))))

;;; async address harvesting
(defvar nm-completion-addresses nil)
(defvar nm-async-harvest-pending-proc nil)   ; the process of a harvest underway
(defun nm-async-harvest ()
  (nm-kill nm-async-harvest-pending-proc)
  (setq nm-completion-addresses (make-hash-table :test 'equal))
  (setq nm-async-harvest-pending-proc
        (notmuch-start-notmuch
         "nm-async-harvest" ; process name
         nil                ; process buffer
         nil                ; process sentinel
         "show"             ; notmuch command
         "--format=sexp"
         "--format-version=2"
         "--body=false"
         "--entire-thread=false"
         (mapconcat (lambda (x) (concat "from:" x)) (nm-my-addresses) " or ")))
  (set-process-filter nm-async-harvest-pending-proc (nm-result-wrangler 'nm-async-harvest-result)))

(defun nm-async-harvest-result (obj)
  (let ((msgs (nm-flatten-forest (list obj))))
    (mapc
     (lambda (msg)
       (let* ((headers (plist-get msg :headers))
              (to (ignore-errors (mail-extract-address-components (plist-get headers :To) t)))
              (cc (ignore-errors (mail-extract-address-components (plist-get headers :Cc) t)))
              (bcc (ignore-errors (mail-extract-address-components (plist-get headers :Bcc) t))))
         (mapc (lambda (parts)
                 (let* ((name (car parts))
                        (email (cadr parts))
                        (entry (if name (format "%s <%s>" name email) email)))
                   (puthash entry t nm-completion-addresses)))
               (append to cc bcc))))
     msgs)))

;;;

(defun nm-show-url ()
  (let ((result (nm-result-at-pos)))
    (when result
      (let ((url (plist-get result :url)))
	(when url
	  (browse-url url))))))

(defun nm-show-thread (query)
  ; query should be a thread id
  (notmuch-show query nil nil nm-query nil))

(defun nm-show-messages (query &optional nodisplay)
  "Show the messages of QUERY in the nm-view-buffer.  If (not NODISPLAY) then make sure that the buffer is displayed."
  (let* ((forest (ignore-errors
                   (nm-call-notmuch
                    "show"
                    "--entire-thread=false"
                    query)))
         (msgs (nm-flatten-forest forest))
         (buffer (get-buffer-create nm-view-buffer)))
    (setq nm-view-buffer-contents-query query)
    (with-current-buffer buffer
      (setq notmuch-show-process-crypto notmuch-crypto-process-mime
            notmuch-show-elide-non-resulting-messages t
            notmuch-show-parent-buffer nil
            notmuch-show-query-context nil)
      (let ((inhibit-read-only t))
        (notmuch-show-mode)
        (set 'buffer-undo-list t)
        (erase-buffer)
        (remove-overlays)
        (goto-char (point-min))
        (mapc (lambda (msg) (notmuch-show-insert-msg msg 0)) msgs)
        (jit-lock-register #'notmuch-show-buttonise-links)
        (run-hooks 'notmuch-show-hook)
        (notmuch-show-goto-first-wanted-message)))
    (when (and (not nodisplay) (not (get-buffer-window nm-view-buffer)))
      (display-buffer-below-selected buffer `((window-height . ,(- (window-height) nm-results-window-size 2)))))))

(defun nm-apply-to-result (fn)
  (let ((result (nm-result-at-pos)))
    (when result
      (let ((query
             (if (nm-thread-mode)
                 (concat "thread:" (plist-get result :thread))
               (concat "id:" (plist-get result :id)))))
        (funcall fn query)))))

(defun nm-refresh-result ()
  (let ((result (nm-result-at-pos)))
    (when result
      (save-excursion
        (let ((init-point (point))
              (end (line-end-position))
              (inhibit-read-only t))
          (beginning-of-line)
          (delete-region (point) (line-end-position))
          (insert (nm-result-line result)))))))

(defun nm-update-tags ()
; TODO: the result may no longer match the query (e.g., if the query was tag:unread and we've now read the message).
; So we want to combine q below with nm-query to detect this case.
  (pcase nm-query-mode
    (`jotmuch nil)
    (`thread (nm-apply-to-result (lambda (q)
                                   (let ((old-result (nm-result-at-pos))
                                         (now-result (nm-call-notmuch "search" "--output=summary" "--exclude=false" q)))
                                     (if (and old-result now-result)
                                         (let ((old-tags (plist-get old-result :tags))
                                               (now-tags (plist-get (car now-result) :tags)))
                                           (unless (equal old-tags now-tags)
                                             (let ((index (nm-result-index-at-pos)))
                                               (when index
                                                 (nm-dynarray-set nm-results index (car now-result))))
                                             (nm-refresh-result))))))))
    (`message (nm-apply-to-result (lambda (q)
                                    (let* ((old-result (nm-result-at-pos))
                                           (msgs (nm-flatten-forest (nm-call-notmuch "show" "--body=false" "--entire-thread=false" "--exclude=false" q)))
                                           (msg (car msgs))
                                           (now-result `(:subject ,(plist-get (plist-get msg :headers) :Subject)
                                                                  :authors ,(plist-get (plist-get msg :headers) :From)
                                                                  :date_relative ,(plist-get msg :date_relative)
                                                                  :tags ,(plist-get msg :tags)
                                                                  :id ,(plist-get msg :id))))
                                      (if (and old-result now-result)
                                          (let ((old-tags (plist-get old-result :tags))
                                                (now-tags (plist-get now-result :tags)))
                                            (unless (equal old-tags now-tags)
                                              (let ((index (nm-result-index-at-pos)))
                                                (when index
                                                  (nm-dynarray-set nm-results index now-result)))
                                              (nm-refresh-result))))))))))

(defun nm-focus-thread ()
  "Show the thread of the current message (in message mode) or just this thread (in thread mode)"
  (interactive)
  (pcase nm-query-mode
    (`jotmuch nil)
    (`thread (nm-apply-to-result (lambda (q)
                                   (setq nm-query q)
                                   (nm-refresh))))
    (`message (nm-apply-to-result (lambda (q)
                                    (let ((result (nm-call-notmuch "search" "--output=summary" q)))
                                      (when result
                                        (let ((thread-id (plist-get (car result) :thread)))
                                          (when thread-id
                                            (setq nm-query (concat "thread:" thread-id))
                                            (nm-refresh))))))))))

(defun nm-open ()
  "Open it."
  (interactive)
  (pcase nm-query-mode
    (`jotmuch (nm-show-url))
    (`thread (nm-apply-to-result 'nm-show-thread))
    (`message (nm-apply-to-result 'nm-show-messages))))

(defun nm-delete ()
  "Delete it."
  (interactive)
  (nm-apply-to-result (lambda (q)
                        (notmuch-tag q '("+deleted" "-unread" "-inbox"))))
  (nm-update-tags)
  (forward-line))

(defun nm-archive ()
  "Archive it."
  (interactive)
  (nm-apply-to-result (lambda (q)
                        (notmuch-tag q '("-deleted" "-unread" "-inbox"))))
  (nm-update-tags)
  (forward-line))

(defun nm-forward ()
  "Forward it."
  (interactive)
  (nm-apply-to-result
   (lambda (q)
     (nm-show-messages q t)))
  (with-current-buffer nm-view-buffer
    (notmuch-mua-forward-message)))

(defvar nm-snooze-default-target "tomorrow 6am"
  "Default target for snoozing a message or thread")
(defvar nm-wakeup-timer nil)
(defvar nm-wakeup-etime nil)

(defun nm-snooze  (&optional arg)
  "Snooze it.  With prefix, prompt for deadline"
  (interactive "P")
  (let* ((target (if (not arg) nm-snooze-default-target
                  (read-string "Snooze until:" nm-snooze-default-target)))
         (target-dtime (nm-date-search-string target)))
    (when (not target-dtime) (error "Error: cannot determine snooze time"))
    (let* ((target-etime (apply 'encode-time target-dtime))
           (target-etime-tag (format "+later.%d.%d" (car target-etime) (cadr target-etime))))
      (nm-apply-to-result (lambda (q)
                            (notmuch-tag q `("+later" ,target-etime-tag "-inbox"))
                            (when (or (not nm-wakeup-etime)                        ; no wakeup time is set
                                      (nm-etime-before target-etime nm-wakeup-etime)) ; or wakeup time is after target
                              (when nm-wakeup-timer (cancel-timer nm-wakeup-timer))
                              (setq nm-wakeup-etime target-etime)
                              (setq nm-wakeup-timer (run-at-time nm-wakeup-etime nil 'nm-wakeup))))))
    (nm-update-tags)
    (forward-line)))

(defun nm-later-to-etime (later)
  (when (and later (string-match "later\\.\\([[:digit:]]+\\)\\.\\([[:digit:]]+\\)" later))
    (list (string-to-number (match-string 1 later)) (string-to-number (match-string 2 later))
          later))) ;; throw in the string itself, etime only cares that there are 2 initial ints

(defun nm-wakeup (&optional quiet)
  (interactive)
  (unless (equal nm-query-mode 'jotmuch)
    (setq nm-wakeup-etime nil)
    (when nm-wakeup-timer
      (cancel-timer nm-wakeup-timer)
      (setq nm-wakeup-timer nil))
    (let* ((now-etime (apply 'encode-time (decode-time)))
	   (count 0)
	   (messages (nm-call-notmuch
		      "search"
		      "--output=messages"
		      "tag:later")))
      (mapc
       (lambda (message-id)
	 (let* ((query (concat "id:" message-id))
		(msg (car
		      (nm-call-notmuch
		       "search"
		       "--output=summary"
		       query)))
		(tags (plist-get msg :tags))
		(later-etime (apply 'append (mapcar 'nm-later-to-etime tags))))
	   (when later-etime
	     (if (not (nm-etime-before now-etime later-etime))
                                        ; later-etime <= now-etime: wake up
		 (progn
		   (setq count (1+ count))
		   (notmuch-tag query `("-later" "+inbox" ,(concat "-" (caddr later-etime)))))
                                        ; later-etime > now-etime: find time to set timer for
	       (when (or (not nm-wakeup-etime) (nm-etime-before later-etime nm-wakeup-etime))
		 (let ((later-etime
                                        ; our later-etime may have >2 elements, run-at-time does not like this
			(list (car later-etime) (cadr later-etime))))
		   (setq nm-wakeup-etime later-etime)))))))
       messages)
      (when nm-wakeup-etime
	(setq nm-wakeup-timer (run-at-time nm-wakeup-etime nil 'nm-wakeup)))
      (cond
       ((eq count 0) (unless quiet (message "No messages are ready to wake up")))
       ((eq count 1) (message "Woke 1 message"))
       (t (message "Woke %d messages" count)))
      (nm-refresh))))

;;; https://github.com/berryboy/chrono

;;; MAYBE USEFUL, FROM PLANNER MODE, http://repo.or.cz/w/planner-el.git/blob_plain/master:/planner.el
;; BUT there is a lot of stuff there, planner-expand-name brings in a bunch.
;; Really need to separate out.
;; (defun planner-read-date (&optional prompt force-read)
;;   "Prompt for a date string in the minibuffer.
;; If PROMPT is non-nil, display it as the prompt string.
;; If FORCE-READ is non-nil, prompt for a date even when we are not
;; using day pages."
;;   (save-window-excursion
;;     (when (or planner-use-day-pages force-read)
;;       (let ((old-buffer (current-buffer)))
;;         (when planner-use-calendar-flag (calendar))
;;         (let ((old-map (copy-keymap calendar-mode-map)))
;;           (unwind-protect
;;               (progn
;;                 (define-key calendar-mode-map [return]
;;                   'planner-calendar-select)
;;                 (define-key calendar-mode-map [mouse-1]
;;                   'planner-calendar-select)
;;                 (setq planner-calendar-selected-date nil)
;;                 (let ((text (read-string
;;                              (format "%s %s"
;;                                      (or prompt "When")
;;                                      (format-time-string
;;                                       (concat "(%Y" planner-date-separator "%m"
;;                                               planner-date-separator "%d, %m"
;;                                        planner-date-separator "%d, %d): "))))))
;;                   (or planner-calendar-selected-date
;;                       (with-current-buffer old-buffer
;;                         (planner-expand-name text)))))
;;             (setq calendar-mode-map old-map)))))))

;;; Le incremental search

(defun nm-minibuffer-contents ()
  "Return the contents of the minibuffer when it is active."
  (if (active-minibuffer-window)
      (with-current-buffer (window-buffer (active-minibuffer-window))
        (minibuffer-contents))))

(defvar nm-update-delay 0.1)

(defun nm-minibuffer-refresh ()
  (let* ((s0 (nm-minibuffer-contents))
         (s (if (or (not s0) (equal (nm-chomp s0) ""))
                "*"
              s0)))
    (when (and (not (equal (nm-chomp s) (nm-chomp nm-query)))
               (sit-for nm-update-delay))
      (setq nm-query s)
      (nm-refresh))))

(defvar nm-query-history (list nm-default-query))

(defun nm-incrementally ()
  "Read query and display results incrementally."
  (interactive)
  (unwind-protect
      (progn
        (add-hook 'post-command-hook 'nm-minibuffer-refresh)
        (read-string "Query: " nm-query 'nm-query-history))
    (remove-hook 'post-command-hook 'nm-minibuffer-refresh)))

;;; Navigating within in a result

(defun nm-scroll-msg-up ()
  "Scroll the nm view window forward, or display it if it is not currently displayed."
  (interactive)
  (let ((nm-view-buffer-window (get-buffer-window nm-view-buffer)))
    (if nm-view-buffer-window
        (if (let ((nm-summary-window (selected-window)))
              (select-window nm-view-buffer-window)
              (prog1
                  ;; Is EOB visible in the buffer?
                  (save-excursion
                    (let ((ht (window-height (selected-window))))
                      (move-to-window-line (- ht 2))
                      (end-of-line)
                      (eobp)))
                (select-window nm-summary-window)))
            (error "End of buffer")
          (let ((other-window-scroll-buffer nm-view-buffer))
            (scroll-other-window)))
      ;; If it isn't visible at all, show the beginning.
      (nm-open))))

(defun nm-scroll-msg-down ()
  "Scroll the nm view window backward.  If the view window is not shown, show it."
  (interactive)
  (let ((nm-view-buffer-window (get-buffer-window nm-view-buffer)))
    (if nm-view-buffer-window
        (if (let ((nm-summary-window (selected-window)))
              (select-window nm-view-buffer-window)
              (prog1
                  ;; Is BOB visible in the buffer?
                  (save-excursion
                    (move-to-window-line 0)
                    (beginning-of-line)
                    (bobp))
                (select-window nm-summary-window)))
            (error "Beginning of buffer")
          (let ((other-window-scroll-buffer nm-view-buffer))
            (scroll-other-window-down (- (window-height nm-view-buffer-window) 2))))
      ;; If it isn't visible at all, show the beginning.
      (nm-open))))

;;; Thread display

(defun nm-get-message (message-id)
  (nm-call-notmuch
   "show"
   (concat "id:" message-id)))

;;; Replies

(defun nm-reply ()
  "Compose a reply."
  (interactive)
  (nm-apply-to-result (lambda (q)
                        (notmuch-mua-new-reply q nil nil))))

(defun nm-reply-all ()
  "Compose a reply-all."
  (interactive)
  (nm-apply-to-result (lambda (q)
                        (notmuch-mua-new-reply q nil t))))

;;; Junk mail handling

(defun nm-bogo-junk (query)
  (let ((shell-command
         (concat notmuch-command " search --output=files ("
                 (shell-quote-argument query)
                 ") and not tag:junk | bogofilter -Ns -b")))
    (call-process-shell-command shell-command)))

(defun nm-bogo-not-junk (query)
  (let ((shell-command
         (concat notmuch-command " search --output=files ("
                 (shell-quote-argument query)
                 ") and tag:junk | bogofilter -Sn -b")))
    (call-process-shell-command shell-command)))

(defun nm-junk (&optional arg)
  "Mark it as junk, or, with prefix, not junk."
  (interactive "p")
  (nm-apply-to-result (lambda (q)
                        (if (or (not arg) (eq arg 1))
                            (progn
                              (nm-bogo-junk q)
                              (notmuch-tag q '("+junk" "+deleted" "-unread" "-inbox")))
                          (progn
                            (nm-bogo-not-junk q)
                            (notmuch-tag q '("-junk" "-deleted"))))
                        (nm-refresh))))

(defun nm-bulk-junk ()
  "Mark *****SPAM***** as junk."
  (interactive)
  (let ((messages (remove-if (lambda (message-id)
                               (let* ((summary (car (nm-call-notmuch "search" "--output=summary" (concat "id:" message-id))))
                                      (subject (nm-sanitize (plist-get summary :subject))))
                                 (not (string-match "^\\*\\*\\*\\*\\*SPAM\\*\\*\\*\\*\\*" subject))))
                             (nm-call-notmuch
                              "search"
                                        ;                              "--limit=2000"
                              "--output=messages"
                              "subject:SPAM not tag:junk"))))
    (message "Marking %d messages as junk" (length messages))
    (mapc (lambda (message-id)
            (let ((q (concat "id:" message-id)))
              (nm-bogo-junk q)
              (notmuch-tag q '("+junk" "+deleted" "-unread" "-inbox"))))
          messages)
    (message "")))

(defun nm-interrupt ()
  (nm-kill nm-async-search-pending-proc)
  (nm-kill nm-async-count-pending-proc))

(defun nm-reset ()
  (interactive)
  (nm-interrupt)
  (setq nm-query nm-default-query)
  (nm-refresh))

;;; Tags

(defvar nm-read-tags-history nil)

(defun nm-read-tags (&optional initial-input)
  (let* ((tag-list (notmuch-tag-completions))
	 (crm-separator " ")
	 (crm-local-completion-map
	  (let ((map (make-sparse-keymap)))
	    (set-keymap-parent map crm-local-completion-map)
	    (define-key map " " 'self-insert-command)
	    map)))
    (delete "" (completing-read-multiple "Tags: " tag-list nil nil initial-input 'nm-read-tags-history))))

(defun nm-tag ()
  "Tag it."
  (interactive)
  (let ((result (nm-result-at-pos)))
    (when result
      (let* ((initial-tags (delete-dups (plist-get result :tags)))
             (initial-input (mapconcat 'identity initial-tags " "))
             (final-tags (nm-read-tags initial-input))
             (tag-changes nil))
        (setq initial-tags (sort initial-tags 'string<))
        (setq final-tags (sort final-tags 'string<))
        (while (or initial-tags final-tags)
          (cond
           ((and (not initial-tags) final-tags)           (push (concat "+" (pop final-tags))   tag-changes))
           ((and initial-tags (not final-tags))           (push (concat "-" (pop initial-tags)) tag-changes))
           ((string< (car final-tags) (car initial-tags)) (push (concat "+" (pop final-tags))   tag-changes))
           ((string< (car initial-tags) (car final-tags)) (push (concat "-" (pop initial-tags)) tag-changes))
           (t                                             (progn (pop initial-tags)
                                                                 (pop final-tags)))))
        (nm-apply-to-result (lambda (q)
                              (notmuch-tag q tag-changes))))))
  (nm-update-tags))

;;; Mode definition

(defvar nm-mode-map
  (let ((map (make-keymap)))
    (suppress-keymap map)
    (define-key map (kbd "RET") 'nm-open)
    (define-key map " " 'nm-scroll-msg-up)
    (define-key map (kbd "DEL") 'nm-scroll-msg-down)
    (define-key map "\C-c\C-c" 'nm-interrupt)
    (define-key map "\C-c\C-g" 'nm-reset)
    (define-key map "/" 'nm-incrementally)
    (define-key map "a" 'nm-archive)
    (define-key map "d" 'nm-delete)
    (define-key map "f" 'nm-forward)
    (define-key map "g" 'nm-refresh)
    (define-key map "J" 'nm-junk)
    (define-key map "m" 'notmuch-mua-new-mail)
    (define-key map "M" 'nm-toggle-query-mode)
    (define-key map "n" 'next-line)
    (define-key map "p" 'previous-line)
    (define-key map "q" 'nm-bury)
    (define-key map "r" 'nm-reply)
    (define-key map "R" 'nm-reply-all)
    (define-key map "s" 'nm-snooze)
    (define-key map "S" 'nm-toggle-sort-order)
    (define-key map "t" 'nm-tag)
    (define-key map "T" 'nm-focus-thread)
    (define-key map "W" 'nm-wakeup)
    map)
  "Keymap for Nm mode.")

;; Nm mode is suitable only for specially-prepared text
(put 'nm-mode 'mode-class 'special)

(defun nm-mode ()
  "Major mode for mail.
Turning on `nm-mode' runs the hook `nm-mode-hook'.

\\{nm-mode-map}."
  (kill-all-local-variables)
  (setq default-directory "~/")
  (setq truncate-lines t)
  (setq buffer-read-only t)
  (use-local-map nm-mode-map)
  (let ((inhibit-read-only t))
    (erase-buffer))
  (nm-draw-header)
  (setq nm-results (nm-dynarray-create))
  (setq nm-all-results-count nil)
  (nm-wakeup t)
  (setq major-mode 'nm-mode)
  (run-mode-hooks 'nm-mode-hook)
  (add-hook 'post-command-hook 'nm-results-post-command nil t)
  (when (and (not (equal nm-query-mode 'jotmuch)) (not nm-completion-addresses)) (nm-async-harvest))
  (nm-refresh))

;;;###autoload
(defun nm ()
  "Switch to *nm* buffer and load files."
  (interactive)
  (switch-to-buffer nm-results-buffer)
  (if (not (eq major-mode 'nm-mode))
      (nm-mode)))

;;;###autoload
(defun nm-jotmuch ()
  "Switch to *nm* buffer and run jotmuch."
  (interactive)
  (setq nm-query-mode 'jotmuch)
  (setq nm-query "*")
  (switch-to-buffer nm-results-buffer)
  (if (not (eq major-mode 'nm-mode))
      (nm-mode)))

(provide 'nm)

;;; nm.el ends here
