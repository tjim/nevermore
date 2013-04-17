;;; nm.el: N E V E R M O R E
;;;
;;; Emacs mail application with
;;; * Incremental search by message or thread
;;; * Snooze
;;; * Junk filtering
;;; * TODO UI for wakeup times
;;; * TODO mail address completion
;;; * TODO tag display
;;; * TODO tag editing
;;; * TODO tag completion
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
  '((t :inherit font-lock-type-face :bold t))
  "Face for Nm tags."
  :group 'nm-faces)

;; Constants

(defconst nm-version "1.0")

(defconst nm-results-buffer "*nm*"
  "Nm buffer name.")

(defconst nm-view-buffer "*nm-view*"
  "Nm view buffer name.")

(defconst nm-separator " | "
  "Text used to separate fields.")

(defconst nm-empty-subject "[No subject]"
  "Text to use as subject when missing.")

(defconst nm-empty-authors "[No authors]"
  "Text to use as authors when missing.")

(defconst nm-empty-date "[Unknown date]"
  "Text to use as date when missing.")

(defconst nm-default-query "tag:inbox ")

;; Global variables

(defvar nm-mode-hook nil
  "Hook run when entering Nm mode.")

(defvar nm-query nm-default-query
  "The current query whose results are in the nm-results-buffer.")

(defvar nm-view-buffer-contents-query nil
  "The current query whose contents are in the nm-view-buffer.")

(defvar nm-results nil
  "List of a screen's worth of results for the current query.")

(defvar nm-all-results-count nil
  "Count of all results for the current query.")

(defvar nm-current-offset 0)

(defvar nm-query-mode 'message) ; or 'thread

(defvar nm-window-height nil
  "Height of Nm buffer.")

(defvar nm-results-per-screen nil
  "Number of results that can fit on one screen.")

(defvar nm-date-width 12
  "Width of dates in Nm buffer.")

(defvar nm-authors-width 20
  "Width of authors in Nm buffer.")

;; Helpers

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

(defun nm-do-search (query)
  (if (nm-thread-mode)
      (ignore-errors
        (nm-call-notmuch
         "search"
         "--output=summary"
         (format "--offset=%d" nm-current-offset)
         (format "--limit=%d" nm-results-per-screen)
         "--sort=newest-first"
         query))
    (ignore-errors
      (let ((messages (nm-call-notmuch
                       "search"
                       "--output=messages"
                       (format "--offset=%d" nm-current-offset)
                       (format "--limit=%d" nm-results-per-screen)
                       "--sort=newest-first"
                       query)))
        (mapcar
         (lambda (message-id)
           (plist-put
            (car
             (nm-call-notmuch
              "search"
              "--output=summary"
              (concat "id:" message-id)))
            :id message-id))
         messages)))))

(defun nm-do-count (query)
  (let ((output (if (nm-thread-mode)
                    "--output=threads"
                  "--output=messages")))
  (or 
   (ignore-errors
     (nm-call-notmuch
      "count"
      output
      query))
   0)))

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

(defun nm-goto-first-result-pos ()
  (goto-char (point-min)))

(defun nm-result-line (result)
  "Return a line of text for a RESULT."
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
                            (if authors authors
                              nm-empty-authors))
        'face 'nm-authors-face)
       (propertize nm-separator 'face 'nm-separator-face)
       (propertize
        (if (and subject (> (length subject) 0)) subject
          nm-empty-subject)
        'face (if (member "unread" tags) 'nm-unread-face 'nm-read-face))
       (when tags
         (propertize
          (replace-regexp-in-string "\\\"" ""
                                    (format " %S" tags))
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

(defun nm-update-buffer (old new)
  (save-excursion
    (save-window-excursion
      (let ((inhibit-read-only t))
        (nm-goto-first-result-pos)
        (nm-update-lines old new)))))

(defun nm-result-equal (a b)
  (and (equal (plist-get a :thread) (plist-get b :thread))
       (equal (plist-get a :date_relative) (plist-get b :date_relative))
       (equal (plist-get a :tags) (plist-get b :tags))))

(defun nm-forward-result ()
  (interactive)
  (when (not (nm-at-final-result-pos))
    (forward-line 1)))

(defun nm-update-lines (old new)
                                        ; invariant: if old then we are at the beginning of the line for (car old)
  (cond
   ((and (not old) (not new))
    '())
   ((not old)
    (mapc 'nm-insert-result new))
   ((not new)
    (delete-region (point) (point-max)))
   ((nm-result-equal (car old) (car new))
    (progn
      (nm-forward-result)
      (nm-update-lines (cdr old) (cdr new))))
   (t
    (progn
      (delete-region (point) (line-end-position))
      (insert (nm-result-line (car new)))
      (forward-line)
;;      (nm-forward-result)
      (nm-update-lines (cdr old) (cdr new))))))

;; maintain count
(defvar nm-async-count-pending-query nil)
(defvar nm-async-count-pending-proc nil)
(defun nm-async-count ()
  (interactive)
  (if nm-async-count-pending-proc
      (ignore-errors (kill-process nm-async-count-pending-proc)))
  (let ((output (if (nm-thread-mode)
                    "--output=threads"
                  "--output=messages")))
    (setq nm-all-results-count nil)
    (setq nm-async-count-pending-query nm-query)
    (setq nm-async-count-pending-proc
          (start-process "nm-async-count" "*nm-async-count*" notmuch-command "count" output nm-query))
    (set-process-filter nm-async-count-pending-proc 
                        (lambda (proc string)
                          (when (equal nm-query nm-async-count-pending-query)
                            (setq nm-all-results-count (string-to-number (nm-chomp string)))
                            (setq nm-async-count-pending-query nil)
                            (setq nm-async-count-pending-proc nil)
                            (nm-refresh-count-display))))))
(defun nm-setq-mode-name (s)
  (with-current-buffer nm-results-buffer
    (setq mode-name s)
    (force-mode-line-update)))
(defun nm-refresh-count-display ()
  (let* ((first-result (1+ nm-current-offset))
         (len (length nm-results))
         (last-result (+ nm-current-offset len)))
    (when (and (not nm-all-results-count) (not (eq len nm-results-per-screen)))
      (setq nm-all-results-count last-result))
    (if nm-all-results-count
        (let ((results (cond ((eq nm-all-results-count 1) "1 result")
                             ((eq nm-all-results-count 0) "no results")
                             (t (format "%d results" nm-all-results-count)))))
          (if (< nm-all-results-count nm-results-per-screen)
              (nm-setq-mode-name (format "nevermore: %s" results))
            (nm-setq-mode-name (format "nevermore: %d-%d of %s" first-result last-result results))))
      (nm-setq-mode-name (format "nevermore: %d-%d" first-result last-result)))))

(defun nm-resize ()
  "Call this function if the size of the window changes."
  (interactive)
  (when (get-buffer nm-results-buffer)
    (with-current-buffer nm-results-buffer
      (let* ((new-window-height (window-body-height))
             (getting-shorter (and nm-window-height (< new-window-height nm-window-height)))
             (current-line (line-number-at-pos))
             (must-change-offset (and getting-shorter (> current-line new-window-height))))
        (setq nm-window-height new-window-height)
        (setq nm-results-per-screen nm-window-height)
        (when must-change-offset
          (setq nm-current-offset (+ nm-current-offset current-line -1))) ; old result will be at top
        (let ((inhibit-read-only t))
          (erase-buffer))
        (setq nm-results nil)
        (nm-refresh)
        (when (not (> current-line new-window-height))
          (goto-char (point-min))
          (forward-line (1- current-line)))))))

(defun nm-refresh ()
  "Reapply the query and refresh the *nm* buffer."
  (interactive)
  (when (get-buffer nm-results-buffer)
    (with-current-buffer nm-results-buffer
      (nm-async-count)
      (nm-draw-header)
      (let ((old nm-results))
        (setq nm-results (nm-do-search nm-query))
        (nm-refresh-count-display)
        (nm-update-buffer old nm-results)))))

(defun nm-at-final-result-pos ()
  (eq (1+ (nm-result-index-at-pos)) nm-results-per-screen))

(defun nm-result-index-at-pos ()
  (and nm-results
       (- (line-number-at-pos) 1)))

(defun nm-result-at-pos ()
  (let ((index (nm-result-index-at-pos)))
    (when index
      (nth index nm-results))))

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

;;; in progress
(defun f (x); maybe useful for completion.  Really ought to build a hash table.
  (let ((email (car (last x)))
        (name (car x)))
    (if (not name)
        `((,email ,email))
      (let* ((result (format "%s <%s>" name email))
             (name-words (split-string (car x)))
             (firstname (car name-words))
             (lastname (car (last name-words))))
        `((,email ,result)
          (,name ,result)
          (,firstname ,result)
          (,lastname ,result))))))
;; try this
;(let ((completion-ignore-case t))
;  (all-completions "mona" (f '("Mona Singh" "mona@cs.princeton.edu"))))

;;; works but slow on mapc of many files 
(defun nm-mail-header-length (file)
  (let ((header-length
;         (ignore-errors
           (with-temp-buffer
             (let ((err-file (make-temp-file "nm-grep-error")))
               (unwind-protect
                   (let ((status (apply #'call-process
                                        "grep" nil (list t err-file) nil
                                        "-b" "-h" "-m" "1" "^$" file nil)))
                     (if (not (eq status 0)) -1
                       (progn
                         (goto-char (point-max))
                         (delete-char -2)
;                         (read (current-buffer))
                         (string-to-number (buffer-string))
                         )))
                 (delete-file err-file))))))
    header-length))

(defun nm-addresses-from (filename header-length)
  (ignore-errors
    (with-temp-buffer
      (insert-file-contents-literally filename nil 0 header-length)
      (mail-extract-address-components (mail-fetch-field "To") t))))
(defun nm-to-addresses ()
  (let* ((files (ignore-errors
                   (nm-call-notmuch
                    "search"
                    "--output=files"
                    "--format=sexp"
                    "from:trevor@att.com or from:tjim@mac.com or from:trevor@research.att.com or from:tjim@cs.princeton.edu or from:tj2586@att.com")))
         (header-lengths (when files
                           (let ((files-file (make-temp-file "nm-files")))
                             (unwind-protect
                                 (progn
                                   (with-temp-file files-file
                                     (mapc (lambda (f) (insert f "\0"))
                                           files))
                                   (with-temp-buffer
                                     (call-process-shell-command "xargs -0 grep -h -m 1 -b '^$'" files-file t)
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
        (setq results (cons (nm-addresses-from (car files) (car header-lengths)) results)
              files (cdr files)
              header-lengths (cdr header-lengths)))
      (mapcar
       (lambda (parts)
         (let ((name (car parts))
               (email (cadr parts)))
           (if name
               (format "%s <%s>" name email)
             email)))
       (delete-dups (apply 'append results))))))

(defun nm-addresses ()
  (let* ((froms "from:trevor@att.com or from:tjim@mac.com or from:trevor@research.att.com or from:tjim@cs.princeton.edu or from:tj2586@att.com")
         (shell-command
          (format "notmuch search --output=files --format=text0 %s | xargs -0 grep -h -m 1 -b '^$'"
                  (shell-quote-argument froms))))
    (with-temp-buffer
      (call-process-shell-command shell-command nil t)
      (insert ")")
      (goto-char (point-min))
      (insert "(")
      (while (search-forward ":" nil t)
        (replace-match "" nil t))
      (goto-char (point-min))
      (read (current-buffer)))))
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
    (when (not nodisplay) (display-buffer buffer))))

(defun nm-apply-to-result (fn)
  (let ((result (nm-result-at-pos)))
    (when result
      (let ((query
             (if (nm-thread-mode)
                 (concat "thread:" (plist-get result :thread))
               (concat "id:" (plist-get result :id)))))
      (funcall fn query)))))

(defun nm-open ()
  "Open it."
  (interactive)
  (nm-apply-to-result 'nm-show-messages))

(defun nm-delete ()
  "Delete it."
  (interactive)
  (nm-apply-to-result (lambda (q)
                        (notmuch-tag q '("+deleted" "-unread" "-inbox"))
                        (nm-refresh))))

(defun nm-archive ()
  "Archive it."
  (interactive)
  (nm-apply-to-result (lambda (q)
                        (notmuch-tag q '("-deleted" "-unread" "-inbox"))
                        (nm-refresh))))

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
      (setq nm-current-offset 0)
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

;;; Navigation within results

(defun nm-forward ()
  (interactive)
  (let ((new-offset (+ nm-current-offset nm-results-per-screen -1)))
    (when (or (not nm-all-results-count) (< new-offset nm-all-results-count)) ; at least one result will be in range
      (setq nm-current-offset new-offset)
      (nm-refresh))))

(defun nm-backward ()
  (interactive)
  (let ((new-offset (max 0 (- nm-current-offset nm-results-per-screen -1))))
      (setq nm-current-offset new-offset)
      (nm-refresh)))

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
            (scroll-other-window-down dist)))
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

(defun nm-reply (&optional arg)
  "Compose a reply.  With prefix, reply-all."
  (interactive "p")
  (nm-apply-to-result (lambda (q)
                        (if (or (not arg) (eq arg 1))
                            (notmuch-mua-new-reply q nil nil)
                          (notmuch-mua-new-reply q nil t)))))

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

(defun nm-reset ()
  (interactive)
  (setq nm-query nm-default-query)
  (nm-refresh))

;;; Mode definition

(defvar nm-mode-map
  (let ((map (make-keymap)))
    (define-key map (kbd "RET") 'nm-open)
    (define-key map " " 'nm-scroll-msg-up)
    (define-key map (kbd "DEL") 'nm-scroll-msg-down)
    (define-key map [remap scroll-up-command] 'nm-forward)
    (define-key map [remap scroll-down-command] 'nm-backward)
    (define-key map (kbd "C-c C-a") 'nm-archive)
    (define-key map (kbd "C-c C-d") 'nm-delete)
    (define-key map (kbd "C-c C-f") 'nm-incrementally)
    (define-key map (kbd "C-c C-g") 'nm-reset)
    (define-key map (kbd "C-c C-j") 'nm-junk)
    (define-key map (kbd "C-c C-l") 'nm-refresh)
    (define-key map (kbd "C-c C-m") 'nm-toggle-query-mode)
    (define-key map (kbd "C-c C-n") 'notmuch-mua-new-mail)
    (define-key map (kbd "C-c C-r") 'nm-reply)
    (define-key map (kbd "C-c C-s") 'nm-snooze)
    (define-key map (kbd "C-c C-q") 'quit-window)
    (define-key map (kbd "C-c C-t") 'nm-flat-thread)
    (define-key map (kbd "C-c C-w") 'nm-wakeup)
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
  (setq nm-results nil)
  (setq nm-all-results-count nil)
  (setq nm-current-offset 0)
  (nm-resize)
  (nm-wakeup)
  (nm-goto-first-result-pos)
  (setq major-mode 'nm-mode)
  (run-mode-hooks 'nm-mode-hook)
  (add-hook 'post-command-hook 'nm-results-post-command nil t)
  (add-hook 'window-configuration-change-hook
            (lambda ()
              (when (and (eq (current-buffer) (get-buffer nm-results-buffer))
                         (not (eq nm-window-height (window-body-height))))
                (nm-resize)))))

;;;###autoload
(defun nm ()
  "Switch to *nm* buffer and load files."
  (interactive)
  (switch-to-buffer nm-results-buffer)
  (if (not (eq major-mode 'nm-mode))
      (nm-mode)))

(provide 'nm)
