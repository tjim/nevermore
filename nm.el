;;; nm.el: N E V E R M O R E
;;;
;;; Emacs mail application with
;;; * Incremental search by message or thread
;;; * Snooze
;;; * Junk filtering
;;; * TODO mail address completion
;;; * TODO more navigation fixes (recenter C-l)
;;; * TODO mail defer queue
;;; * TODO UI for wakeup times
;;; * TODO IMAP integration
;;; * TODO diary integration
;;; * TODO snooze by natural date
;;; * TODO triage (http://rowansimpson.com/2013/04/16/triage/)

(require 'notmuch)
(require 'notmuch-lib)
(require 'notmuch-mua)

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

;(defvar nm-query-mode 'message) ; or 'thread
(defvar nm-query-mode 'thread)

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

(defun nm-toggle-query-mode ()
  (interactive)
  (if (nm-thread-mode)
      (setq nm-query-mode 'message)
    (setq nm-query-mode 'thread))
  (nm-refresh))

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

(defun nm-result-line (result)
  "Return a line of text for a RESULT."
  (when (and result (not nm-problem-before) (not (plist-get result :date_relative)))
    (setq nm-problem-before t)
    (message "Problem: %S" result))
  (when result
    (let* ((date (plist-get result :date_relative))
           (authors (plist-get result :authors))
           (subject (plist-get result :subject))
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
           (if (nm-thread-mode)
               (propertize "Thread search" 'face 'nm-header-face)
             (propertize "Message search" 'face 'nm-header-face))
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

(defvar nm-async-search-pending-proc nil)   ; the process of a search underway
(defvar nm-async-search-pending-output nil) ; holds the not-yet-processed part of the output of the search process
(defun nm-async-search ()
  ; perform an asynchronous search on nm-query, displaying results in nm-results-buffer and storing sexps in nm-results
  (when nm-async-search-pending-proc
      (ignore-errors (kill-process nm-async-search-pending-proc))
      ; kill-process sends signal, actual process death is asynchronous, so indicate that we want the process dead
      (setq nm-async-search-pending-proc nil))
  (setq nm-async-search-pending-output nil) ; indicate that we have not gotten any output yet
  (setq nm-results (nm-dynarray-create))
  (setq nm-async-search-pending-proc
        (if (nm-thread-mode)
            (notmuch-start-notmuch
             "nm-async-search" ; process name
             nil               ; process buffer
             nil               ; process sentinel
             "search"          ; notmuch command
             "--format=sexp"
             "--format-version=2"
             "--output=summary"
             "--sort=newest-first"
             nm-query)
          (notmuch-start-notmuch
           "nm-async-search" ; process name
           nil               ; process buffer
           nil               ; process sentinel
           "show"            ; notmuch command
           "--format=sexp"
           "--format-version=2"
           "--body=false"
           "--entire-thread=false"
           nm-query)))
  (set-process-filter
   nm-async-search-pending-proc
   (lambda (proc string)
     (when (and nm-async-search-pending-proc (equal (process-id proc) (process-id nm-async-search-pending-proc)))
       (if nm-async-search-pending-output
                                        ; This is not the first time we have seen output, add it to anything remaining from last time
           (setq nm-async-search-pending-output (concat nm-async-search-pending-output string))
                                        ; This is the first time we have seen output.  Skip the initial open paren
         (setq nm-async-search-pending-output (substring string 1)))
       (while
           (let ((result (ignore-errors (read-from-string nm-async-search-pending-output))))
             (and result
                  (progn
                    (let ((obj (car result))
                          (offset (cdr result)))
                      (setq nm-async-search-pending-output (substring nm-async-search-pending-output offset))
                      (nm-async-search-new-result obj)
                      t))))))))
  ; return value
  nil)
(defun nm-async-search-new-result (result)
  (when (and result (get-buffer nm-results-buffer))
    (if (nm-thread-mode)
         (with-current-buffer nm-results-buffer
          (save-excursion
            (goto-char (point-max))
            (let ((inhibit-read-only t))
              (insert (nm-result-line result) "\n"))
            (setq nm-results (nm-dynarray-append nm-results result))))
    (nm-async-search-message result))))
(defun nm-async-search-message (obj)
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

(defun nm-bury ()
  "Bury the current nevermore buffers."
  (interactive)
  (let ((b (get-buffer nm-view-buffer)))
    (nm-log "quitting %S" b)
    (when b (replace-buffer-in-windows b)))
  (let ((b (get-buffer nm-results-buffer)))
    (nm-log "quitting %S" b)
    (when b (replace-buffer-in-windows b))))

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
  (when nm-async-count-pending-proc
    (ignore-errors (kill-process nm-async-count-pending-proc))
    (setq nm-async-count-pending-proc nil))
  (nm-setq-mode-name "nevermore")
  (setq nm-all-results-count nil)
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
  (set-process-filter nm-async-count-pending-proc
                      (lambda (proc string)
                        (when (and nm-async-count-pending-proc (equal (process-id proc) (process-id nm-async-count-pending-proc)))
                          (setq nm-async-count-pending-proc nil)
                          (setq nm-all-results-count (string-to-number (nm-chomp string)))
                          (if nm-all-results-count
                              (let ((results (cond ((eq nm-all-results-count 1) "1 result")
                                                   ((eq nm-all-results-count 0) "no results")
                                                   (t (format "%d results" nm-all-results-count)))))
                                (nm-setq-mode-name (format "nevermore: %s" results)))
                            (nm-setq-mode-name "nevermore"))))))

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

;;; completion
(defvar nm-completion-addresses nil)
(defun nm-completion-addresses-from-file (filename header-length)
  (ignore-errors
    (with-temp-buffer
      (insert-file-contents-literally filename nil 0 header-length)
      (mail-extract-address-components (mail-fetch-field "To") t))))
(defun nm-harvest-addresses ()
  (interactive)
  (let* ((from-me (mapconcat (lambda (x) (concat "from:" x)) (nm-my-addresses) " or "))
         (files (ignore-errors
                   (nm-call-notmuch
                    "search"
                    "--output=files"
                    "--format=sexp"
                    from-me)))
         (header-lengths (when files
                           (let ((files-file (make-temp-file "nm-files")))
                             (unwind-protect
                                 (progn
                                   (with-temp-file files-file
                                     (mapc (lambda (f) (insert f "\0"))
                                           files))
                                   (with-temp-buffer
                                     (call-process-shell-command "xargs -0 -n 1 grep -h -m 1 -b '^$'" files-file t)
                                     (insert ")")
                                     (goto-char (point-min))
                                     (insert "(")
                                     (while (search-forward ":" nil t)
                                       (replace-match "" nil t))
                                     (goto-char (point-min))
                                     (read (current-buffer))))
                               (delete-file files-file))))))
    (let (results)
      (while (and files header-lengths)
        (setq results (cons (nm-completion-addresses-from-file (car files) (car header-lengths)) results)
              files (cdr files)
              header-lengths (cdr header-lengths)))
      (setq nm-completion-addresses
            (mapcar
             (lambda (parts)
               (let ((name (car parts))
                     (email (cadr parts)))
                 (if name
                     (format "%s <%s>" name email)
                   email)))
             (delete-dups (apply 'append results)))))))

;;;

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
    (when (not nodisplay)
      (display-buffer-below-selected buffer `((window-height . ,(- (window-height) nm-results-window-size 2)))))))

(defun nm-apply-to-result (fn)
  (let ((result (nm-result-at-pos)))
    (when result
      (let ((query
             (if (nm-thread-mode)
                 (concat "thread:" (plist-get result :thread))
               (concat "id:" (plist-get result :id)))))
      (funcall fn query)))))

(defun nm-focus-thread ()
  "Show the thread of the current message (in message mode) or just this thread (in thread mode)"
  (interactive)
  (if (nm-thread-mode)
      (nm-apply-to-result (lambda (q)
                            (setq nm-query q)
                            (nm-refresh)))
    (nm-apply-to-result (lambda (q)
                          (let ((result (nm-call-notmuch "search" "--output=summary" q)))
                            (when result
                              (let ((thread-id (plist-get (car result) :thread)))
                                (when thread-id
                                  (message "A")
                                  (setq nm-query (concat "thread:" thread-id))
                                  (message "B")
                                  (nm-refresh)
                                  (message "C")))))))))

(defun nm-open ()
  "Open it."
  (interactive)
  (nm-apply-to-result 'nm-show-messages))

(defun nm-delete ()
  "Delete it."
  (interactive)
  (nm-apply-to-result (lambda (q)
                        (notmuch-tag q '("+deleted" "-unread" "-inbox"))))
  (nm-refresh))

(defun nm-archive ()
  "Archive it."
  (interactive)
  (nm-apply-to-result (lambda (q)
                        (notmuch-tag q '("-deleted" "-unread" "-inbox"))))
  (nm-refresh))

(defun nm-forward ()
  "Forward it."
  (interactive)
  (nm-apply-to-result
   (lambda (q)
     (nm-show-messages q t)))
  (with-current-buffer nm-view-buffer
    (notmuch-mua-forward-message)))

;;; Times
;;; We say an etime is a time as returned by encode-time
;;; We say a dtime is a time as returned by decode-time

(defun etime-compare (a b)
  "Return <0 if a is before b, >0 if b is before a, 0 if a and b are the same time."
  (if (eq (car a) (car b))
      (- (cadr a) (cadr b))
    (- (car a) (car b))))

(defun etime-before (a b)
  (< (etime-compare a b) 0))

(defun next-morning (dtime)
  "Calculate the next morning following a dtime.  Return as a dtime."
  (let ((SEC 0)
        (MINUTE 0)
        (HOUR 4)
        (DAY (cadddr dtime))
        (REST (copy-seq (cdr(cdddr dtime)))))
    `(,SEC ,MINUTE ,HOUR ,(1+ DAY) ,@REST)))

(defvar nm-wakeup-timer nil)
(defvar nm-wakeup-etime nil)

(defun nm-snooze ()
  "Snooze it."
  (interactive)
  (nm-apply-to-result (lambda (q)
                        (let* ((now-dtime (decode-time))
                               (tomorrow-dtime (next-morning now-dtime))
                               (tomorrow-etime (apply 'encode-time tomorrow-dtime))
                               (tomorrow-etime-tag (format "+later.%d.%d" (car tomorrow-etime) (cadr tomorrow-etime))))
                          (notmuch-tag q `("+later" ,tomorrow-etime-tag "-inbox"))
                          (when (or (not nm-wakeup-etime)                          ; no wakeup time is set
                                    (etime-before tomorrow-etime nm-wakeup-etime)) ; or wakeup time is after tomorrow
                                (when nm-wakeup-timer (cancel-timer nm-wakeup-timer))
                                (setq nm-wakeup-etime tomorrow-etime)
                                (setq nm-wakeup-timer (run-at-time nm-wakeup-etime nil 'nm-wakeup)))
                          (nm-refresh)))))

(defun nm-later-to-etime (later)
  (when (and later (string-match "later\\.\\([[:digit:]]+\\)\\.\\([[:digit:]]+\\)" later))
    (list (string-to-number (match-string 1 later)) (string-to-number (match-string 2 later))
          later))) ;; throw in the string itself, etime only cares that there are 2 initial ints

(defun nm-wakeup ()
  (interactive)
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
           (if (not (etime-before now-etime later-etime))
                                        ; later-etime <= now-etime: wake up
               (progn
                 (setq count (1+ count))
                 (notmuch-tag query `("-later" "+inbox" ,(concat "-" (caddr later-etime)))))
                                        ; later-etime > now-etime: find time to set timer for
             (when (or (not nm-wakeup-etime) (etime-before later-etime nm-wakeup-etime))
               (let ((later-etime
                      ; our later-etime may have >2 elements, run-at-time does not like this
                      (list (car later-etime) (cadr later-etime))))
                 (setq nm-wakeup-etime later-etime)))))))
     messages)
    (when nm-wakeup-etime
      (setq nm-wakeup-timer (run-at-time nm-wakeup-etime nil 'nm-wakeup)))
    (cond
     ((eq count 0) (message "No messages are ready to wake up"))
     ((eq count 1) (message "Woke 1 message"))
     (t (message "Woke %d messages" count)))
    (nm-refresh)))

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
(defun nm-flat-thread ()
  (interactive)
  (let ((result (nm-result-at-pos)))
    (when result
      (let* ((thread-id (concat "thread:" (plist-get result :thread)))
             (messages
              (mapcar 'nm-get-message
                      (nm-call-notmuch
                       "search"
                       "--output=messages"
                       "--sort=oldest-first"
                       thread-id))))
        (switch-to-buffer "FOO")
        (mapc (lambda (m) (insert (format "%S\n" m))) messages)))))

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
                                      (subject (plist-get summary :subject)))
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
  (when nm-async-search-pending-proc
      (ignore-errors (kill-process nm-async-search-pending-proc))
      (setq nm-async-search-pending-proc nil))
  (when nm-async-count-pending-proc
    (ignore-errors (kill-process nm-async-count-pending-proc))
    (setq nm-async-count-pending-proc nil)))

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
  (nm-refresh))

;;; Mode definition

(defvar nm-mode-map
  (let ((map (make-keymap)))
    (suppress-keymap map)
    (define-key map (kbd "RET") 'nm-open)
    (define-key map " " 'nm-scroll-msg-up)
    (define-key map (kbd "DEL") 'nm-scroll-msg-down)
    (define-key map "\C-c\C-c" 'nm-interrupt)
    (define-key map "\C-c\C-g" 'nm-reset)
    (define-key map "\C-c\C-l" 'nm-refresh)
    (define-key map "\C-c\C-m" 'nm-toggle-query-mode)
    (define-key map "/" 'nm-incrementally)
    (define-key map "a" 'nm-archive)
    (define-key map "d" 'nm-delete)
    (define-key map "f" 'nm-forward)
    (define-key map "J" 'nm-junk)
    (define-key map "m" 'notmuch-mua-new-mail)
    (define-key map "n" 'next-line)
    (define-key map "p" 'previous-line)
    (define-key map "q" 'nm-bury)
    (define-key map "r" 'nm-reply)
    (define-key map "R" 'nm-reply-all)
    (define-key map "s" 'nm-snooze)
    (define-key map "T" 'nm-focus-thread)
    (define-key map "t" 'nm-tag)
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
;  (nm-wakeup)
  (setq major-mode 'nm-mode)
  (run-mode-hooks 'nm-mode-hook)
  (add-hook 'post-command-hook 'nm-results-post-command nil t)
  (nm-refresh)
)

;;;###autoload
(defun nm ()
  "Switch to *nm* buffer and load files."
  (interactive)
  (switch-to-buffer nm-results-buffer)
  (if (not (eq major-mode 'nm-mode))
      (nm-mode)))

;;; completion
(require 'company)
(eval-when-compile (require 'cl))

;; (defun company-nm-insert (match)
;;   "Replace MATCH with the expanded abbrev."
;;   (expand-abbrev))

;;;###autoload
(defun company-nm-insert (completion-text)
  (let* ((end (point))
         (beg (save-excursion
                (re-search-backward "\\(\\`\\|[\n:,]\\)[ \t]*")
                (goto-char (match-end 0))
                (point))))
    (delete-region beg end)
    (goto-char beg)
    (message "completion-text: %s" completion-text)
    (insert completion-text)))
(setq company-frontends '(company-pseudo-tooltip-frontend))
(defun company-nm (command &optional arg &rest ignored)
  "`company-mode' completion back-end for nm."
  (interactive (list 'interactive))
  (case command
    (interactive (company-begin-backend 'company-nm
                                        'company-nm-insert))
    (prefix (let* ((end (point))
                   (beg (save-excursion
                          (re-search-backward "\\(\\`\\|[\n:,]\\)[ \t]*")
                          (goto-char (match-end 0))
                          (point))))
              (buffer-substring-no-properties beg end)))
    (candidates (let ((completion-ignore-case t))
                  (let ((results (car (completion-substring--all-completions arg nm-completion-addresses nil 0))))
                    results)))
    (ignore-case t)
    (no-cache t)
    (sorted t)))

;;; HACK: override this function from company.el for substring completion
(defun company--create-lines (selection limit)

  (let ((len company-candidates-length)
        (numbered 99999)
        lines
        width
        lines-copy
        previous
        remainder
        new)

    ;; Scroll to offset.
    (setq limit (company-pseudo-tooltip-update-offset selection len limit))

    (when (> company-tooltip-offset 0)
      (setq previous (format "...(%d)" company-tooltip-offset)))

    (setq remainder (- len limit company-tooltip-offset)
          remainder (when (> remainder 0)
                      (setq remainder (format "...(%d)" remainder))))

    (decf selection company-tooltip-offset)
    (setq width (max (length previous) (length remainder))
          lines (nthcdr company-tooltip-offset company-candidates)
          len (min limit len)
          lines-copy lines)

    (dotimes (i len)
      (setq width (max (length (pop lines-copy)) width)))
    (setq width (min width (window-width)))

    (setq lines-copy lines)

    ;; number can make tooltip too long
    (when company-show-numbers
      (setq numbered company-tooltip-offset))

    (when previous
      (push (propertize (company-safe-substring previous 0 width)
                        'face 'company-tooltip)
            new))

    (dotimes (i len)
      (push (company-fill-propertize
             (if (>= numbered 10)
                 (pop lines)
               (incf numbered)
               (format "%s %d"
                       (company-safe-substring (pop lines)
                                               0 (- width 2))
                       (mod numbered 10)))
             width (equal i selection))
            new))

    (when remainder
      (push (propertize (company-safe-substring remainder 0 width)
                        'face 'company-tooltip)
            new))

    (setq lines (nreverse new))))

(provide 'nm)
