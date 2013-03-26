;;; nm.el --- "nevermore" interface to notmuch
;;; work in progress
;;; based on deft.el

(require 'widget)
(require 'notmuch-lib)
(require 'notmuch-mua)

(defun nm-do-search (query)
  (notmuch-call-notmuch-json
   "search"
   "--output=summary"
   (format "--limit=%d" (- nm-window-height 2))
   "--format=json"
   "--format-version=1"
   "--sort=newest-first"
   query))

(defun nm-do-count (query)
  (notmuch-call-notmuch-json
   "count"
   query))

;; Customization

(defgroup nm nil
  "Emacs Nm mode."
  :group 'local)

;; (defcustom nm-time-format " %Y-%m-%d %H:%M"
;;   "Format string for modification times in the Nm browser.
;; Set to nil to hide."
;;   :type '(choice (string :tag "Time format")
;;                  (const :tag "Hide" nil))
;;  :group 'nm)

(defcustom nm-incremental-search t
  "Use incremental string search when non-nil and regexp search when nil.
During incremental string search, substrings separated by spaces are
treated as subfilters, each of which must match a file.  They need
not be adjacent and may appear in any order.  During regexp search, the
entire filter string is interpreted as a single regular expression."
  :type 'boolean
  :group 'nm)

;; (defcustom nm-strip-title-regexp "\\(?:^%+\\|^[#* ]+\\|-\\*-[[:alpha:]]+-\\*-\\|#+$\\)"
;;   "Regular expression to remove from file titles.
;; Presently, it removes leading LaTeX comment delimiters, leading
;; and trailing hash marks from Markdown ATX headings, leading
;; astersisks from Org Mode headings, and Emacs mode lines of the
;; form -*-mode-*-."
;;   :type 'regexp
;;   :safe 'stringp
;;   :group 'nm)

(defgroup nm-faces nil
  "Faces used in Nm mode"
  :group 'nm
  :group 'faces)

(defface nm-header-face
  '((t :inherit font-lock-keyword-face :bold t))
  "Face for Nm header."
  :group 'nm-faces)

(defface nm-filter-string-face
  '((t :inherit font-lock-string-face))
  "Face for Nm filter string."
  :group 'nm-faces)

(defface nm-filter-string-error-face
  '((t :inherit font-lock-warning-face))
  "Face for Nm filter string when regexp is invalid."
  :group 'nm-faces)

(defface nm-separator-face
  '((t :inherit font-preprocessor-face))
  "Face for Nm separator string."
  :group 'nm-faces)

(defface nm-unread-face
  '((t :inherit font-lock-keyword-face))
  "Face for Nm subjects."
  :group 'nm-faces)

(defface nm-read-face
  '((t :inherit font-lock-function-name-face))
  "Face for Nm subjects."
  :group 'nm-faces)

(defface nm-authors-face
  '((t :inherit font-lock-comment-face))
  "Face for Nm authors."
  :group 'nm-faces)

(defface nm-date-face
  '((t :inherit font-lock-constant-face :bold t))
  "Face for Nm dates."
  :group 'nm-faces)

;; Constants

(defconst nm-version "0.5.1")

(defconst nm-buffer "*Nm*"
  "Nm buffer name.")

(defconst nm-separator " | "
  "Text used to separate fields.")

(defconst nm-empty-subject "[No subject]"
  "Text to use as subject when missing.")

(defconst nm-empty-authors "[No authors]"
  "Text to use as authors when missing.")

(defconst nm-empty-date "[Unknown date]"
  "Text to use as date when missing.")

;; Global variables

(defvar nm-mode-hook nil
  "Hook run when entering Nm mode.")

(defvar nm-filter-regexp (list "" "tag:inbox")
  "A list of string representing the current filter used by Nm.

In incremental search mode, when `nm-incremental-search' is
non-nil, the elements of this list are the individual words of
the filter string, in reverse order.  That is, the car of the
list is the last word in the filter string.

In regexp search mode, when `nm-incremental-search' is nil,
this list has a single element containing the entire filter
regexp.")

(defvar nm-current-matches nil
  "List of matches for the current filter.")

(defvar nm-current-count nil
  "Count of matches for the current filter.")

(defvar nm-all-matches nil
  "List of all possible matches.")

(defvar nm-all-count nil
  "Count of matches for the current filter.")

;; (defvar nm-hash-contents nil
;;   "Hash containing complete cached file contents, keyed by filename.")

;; (defvar nm-hash-mtimes nil
;;   "Hash containing cached file modification times, keyed by filename.")

;; (defvar nm-hash-titles nil
;;   "Hash containing cached file titles, keyed by filename.")

;; (defvar nm-hash-summaries nil
;;   "Hash containing cached file summaries, keyed by filename.")

;; (defvar nm-auto-save-buffers nil
;;   "List of buffers that will be automatically saved.")

(defvar nm-window-width nil
  "Width of Nm buffer.")

(defvar nm-window-height nil
  "Height of Nm buffer.")

(defvar nm-filter-history nil
  "History of interactive filter strings.")

(defvar nm-regexp-error nil
  "Flag for indicating invalid regexp errors.")

(defvar nm-date-width 11
  "Width of dates in Nm buffer.")

(defvar nm-authors-width 20
  "Width of authors in Nm buffer.")

(defvar nm-subject-width nil
  "Width of authors in Nm buffer.")

;; Helpers

(defun nm-search-term ()
  (nm-whole-filter-regexp))

(defun nm-whole-filter-regexp ()
  "Join incremental filters into one."
  (mapconcat 'identity (reverse nm-filter-regexp) " "))

;; (defun nm-search-forward (str)
;;   "Function to use when matching files against filter strings.
;; This function calls `search-forward' when `nm-incremental-search'
;; is non-nil and `re-search-forward' otherwise."
;;   (if nm-incremental-search
;;       (search-forward str nil t)
;;     (re-search-forward str nil t)))

(defun nm-set-mode-name ()
  (let* ((count (or nm-current-count 0))
         (matches
          (cond ((eq count 1) "1 match")
                ((< count (- nm-window-height 2)) (format "%d matches" count))
                (t (format "%d of %d matches" (- nm-window-height 2) count)))))
    (if nm-incremental-search
        (setq mode-name (format "Nm: %s" matches))
      (setq mode-name (format "Nm/R: %s" matches)))))

(defun nm-toggle-incremental-search ()
  "Toggle the `nm-incremental-search' setting."
  (interactive)
  (cond
   (nm-incremental-search
    (setq nm-incremental-search nil)
    (message "Regexp search"))
   (t
    (setq nm-incremental-search t)
    (message "Incremental string search")))
  (nm-filter (nm-whole-filter-regexp) t)
  (nm-set-mode-name))

;; (defun nm-filter-regexp-as-regexp ()
;;   "Return a regular expression corresponding to the current filter string.
;; When `nm-incremental-search' is non-nil, we must combine each individual
;; whitespace separated string.  Otherwise, the `car' of `nm-filter-regexp'
;; is the complete regexp."
;;   (if nm-incremental-search
;;       (mapconcat 'regexp-quote (reverse nm-filter-regexp) "\\|")
;;     (car nm-filter-regexp)))

;; File processing

;; (defun nm-chomp (str)
;;   "Trim leading and trailing whitespace from STR."
;;   (replace-regexp-in-string "\\(^[[:space:]\n]*\\|[[:space:]\n]*$\\)" "" str))

;; (defun nm-base-filename (file)
;;   "Strip the path and extension from filename FILE."
;;   (setq file (file-name-nondirectory file))
;;   (if (> (length nm-extension) 0)
;;       (setq file (replace-regexp-in-string (concat "\." nm-extension "$") "" file)))
;;   file)

;; (defun nm-find-all-files ()
;;   "Return a list of all files in the Nm directory.

;; It is important to note that the return value is a list of
;; absolute filenames.  These absolute filenames are used as keys
;; for the various hash tables used for storing file metadata and
;; contents.  So, any functions looking up values in these hash
;; tables should use `expand-file-name' on filenames first."
;;   (if (file-exists-p nm-directory)
;;       (let (files result)
;;         ;; List all files
;;         (setq files
;;               (directory-files nm-directory t
;;                                (concat "\." nm-extension "$") t))
;;         ;; Filter out files that are not readable or are directories
;;         (dolist (file files)
;;           (when (and (file-readable-p file)
;;                      (not (file-directory-p file)))
;;             (setq result (cons file result))))
;;         result)))

;; (defun nm-strip-title (title)
;;   "Remove all strings matching `nm-strip-title-regexp' from TITLE."
;;   (nm-chomp (replace-regexp-in-string nm-strip-title-regexp "" title)))

;; (defun nm-parse-title (file contents)
;;   "Parse the given FILE and CONTENTS and determine the title.
;; According to `nm-use-filename-as-title', the title is taken to
;; be the first non-empty line of a file or the file name."
;;   (if nm-use-filename-as-title
;;       (nm-base-filename file)
;;     (let ((begin (string-match "^.+$" contents)))
;;       (if begin
;;           (funcall nm-parse-title-function
;;                    (substring contents begin (match-end 0)))))))

;; (defun nm-parse-summary (contents title)
;;   "Parse the file CONTENTS, given the TITLE, and extract a summary.
;; The summary is a string extracted from the contents following the
;; title."
;;   (let ((summary (replace-regexp-in-string "[\n\t]" " " contents)))
;;     (if (and (not nm-use-filename-as-title) title)
;;         (if (string-match (regexp-quote title) summary)
;;             (nm-chomp (substring summary (match-end 0) nil))
;;           "")
;;       summary)))

;; (defun nm-cache-file (file)
;;   "Update file cache if FILE exists."
;;   (when (file-exists-p file)
;;     (add-to-list 'nm-all-matches file)
;;     (let ((mtime-cache (nm-file-mtime file))
;;           (mtime-file (nth 5 (file-attributes (file-truename file)))))
;;       (if (or (not mtime-cache)
;;               (time-less-p mtime-cache mtime-file))
;;           (nm-cache-newer-file file mtime-file)))))

;; (defun nm-cache-newer-file (file mtime)
;;   "Update cached information for FILE with given MTIME."
;;   ;; Modification time
;;   (puthash file mtime nm-hash-mtimes)
;;   (let (contents title)
;;     ;; Contents
;;     (with-current-buffer (get-buffer-create "*Nm temp*")
;;       (insert-file-contents file nil nil nil t)
;;       (setq contents (concat (buffer-string))))
;;     (puthash file contents nm-hash-contents)
;;     ;; Title
;;     (setq title (nm-parse-title file contents))
;;     (puthash file title nm-hash-titles)
;;     ;; Summary
;;     (puthash file (nm-parse-summary contents title) nm-hash-summaries))
;;   (kill-buffer "*Nm temp*"))

;; (defun nm-file-newer-p (file1 file2)
;;   "Return non-nil if FILE1 was modified since FILE2 and nil otherwise."
;;   (let (time1 time2)
;;     (setq time1 (nm-file-mtime file1))
;;     (setq time2 (nm-file-mtime file2))
;;     (time-less-p time2 time1)))

;; (defun nm-cache-initialize ()
;;   "Initialize hash tables for caching files."
;;   (setq nm-hash-contents (make-hash-table :test 'equal))
;;   (setq nm-hash-mtimes (make-hash-table :test 'equal))
;;   (setq nm-hash-titles (make-hash-table :test 'equal))
;;   (setq nm-hash-summaries (make-hash-table :test 'equal)))

;; (defun nm-cache-update-all ()
;;   "Update file list and update cached information for each file."
;;   (setq nm-all-matches (nm-find-all-files))             ; List all files
;;   (mapc 'nm-cache-file nm-all-matches)                  ; Cache contents
;;   (setq nm-all-matches (nm-sort-files nm-all-matches))) ; Sort by mtime

;; (defun nm-cache-update-file (file)
;;   "Update cached information for a single file."
;;   (nm-cache-file file)                                  ; Cache contents
;;   (setq nm-all-matches (nm-sort-files nm-all-matches))) ; Sort by mtime

;; Cache access

;; (defun nm-file-contents (file)
;;   "Retrieve complete contents of FILE from cache."
;;   (gethash file nm-hash-contents))

;; (defun nm-file-mtime (file)
;;   "Retrieve modified time of FILE from cache."
;;   (gethash file nm-hash-mtimes))

;; (defun nm-file-title (file)
;;   "Retrieve title of FILE from cache."
;;   (gethash file nm-hash-titles))

;; (defun nm-file-summary (file)
;;   "Retrieve summary of FILE from cache."
;;   (gethash file nm-hash-summaries))

;; File list display

(defun nm-print-header ()
  "Prints the *Nm* buffer header."
  (if nm-filter-regexp
      (progn
        (widget-insert
         (propertize "Nm: " 'face 'nm-header-face))
        (widget-insert
         (propertize (nm-whole-filter-regexp) 'face
                     (if (and (not nm-incremental-search) nm-regexp-error)
                         'nm-filter-string-error-face
                       'nm-filter-string-face))))
    (widget-insert
         (propertize "Nm" 'face 'nm-header-face)))
  (widget-insert "\n\n"))

(defun nm-buffer-setup ()
  "Render the match browser in the *Nm* buffer."
  (setq nm-window-width (window-width))
  (setq nm-window-height (window-body-height))
  (setq nm-subject-width (- nm-window-width nm-authors-width nm-date-width (* 2 (length nm-separator))))
  (let ((inhibit-read-only t))
    (erase-buffer))
  (remove-overlays)
  (nm-print-header)

  ;; Print the files list
  (if nm-current-matches
      (progn
        (mapc 'nm-match-widget nm-current-matches))
    (widget-insert (nm-no-matches-message)))

  (use-local-map nm-mode-map)
  (widget-setup)
  (goto-char 1)
  (forward-line 2)
  (widget-forward 1))

(defun nm-truncate-or-pad (width str)
  (let ((len (length str)))
    (if (> width len)
        ; pad
        (concat str
                (format (format "%%%ds" (- width len)) ""))
      ; truncate
      (format (format "%%.%ds" width) str))))

(defun nm-match-widget (match)
  "Add a line to the match browser for the given MATCH."
  (when match
    (let* ((date (plist-get match :date_relative))
           (authors (plist-get match :authors))
           (subject (plist-get match :subject))
           (tags (plist-get match :tags)))
      (widget-insert
       (propertize
        (nm-truncate-or-pad nm-authors-width
                            (if authors authors
                              nm-empty-authors))
        'face 'nm-authors-face))
      (widget-insert (propertize nm-separator 'face 'nm-separator-face))
      (widget-create 'link
                     :button-prefix ""
                     :button-suffix ""
                     :button-face (if (member "unread" tags) 'nm-unread-face 'nm-read-face)
                     :format "%[%v%]"
                     :tag match
                     :help-echo nil
                     :notify (lambda (widget &rest ignore)
                               (nm-open-match (widget-get widget :tag)))
                     (nm-truncate-or-pad nm-subject-width
                                         (if (and subject (> (length subject) 0)) subject
                                           nm-empty-subject)))
      (widget-insert (propertize nm-separator 'face 'nm-separator-face))
      (widget-insert
       (propertize
        (nm-truncate-or-pad nm-date-width
                            (if date date
                              nm-empty-date))
        'face 'nm-date-face))
      (widget-insert "\n"))))

(add-hook 'window-configuration-change-hook
          (lambda ()
            ;; (when (and (eq (current-buffer) (get-buffer nm-buffer))
            ;;            (not (and (eq nm-window-width (window-width))
            ;;                      (eq nm-window-height (window-body-height)))))
            (when (eq (current-buffer) (get-buffer nm-buffer))
              (nm-buffer-setup))))

(defun nm-refresh ()
  "Update the file cache, reapply the filter, and refresh the *Nm* buffer."
  (interactive)
;;  (nm-cache-update-all)
  (nm-refresh-filter))

(defun nm-refresh-filter ()
  "Reapply the filter and refresh the *Nm* buffer.
Call this after any actions which update the cache."
  (interactive)
  (nm-filter-update)
  (nm-refresh-browser))

(defun nm-refresh-browser ()
  "Refresh the *Nm* buffer in the background.
Call this function after any actions which update the filter and file list."
  (when (get-buffer nm-buffer)
    (with-current-buffer nm-buffer
      (nm-buffer-setup))))

;; (defun nm-no-directory-message ()
;;   "Return a short message to display when the Nm directory does not exist."
;;   (concat "Directory " nm-directory " does not exist.\n"))

(defun nm-no-matches-message ()
  "Return a short message to display if no files are found."
  (if nm-filter-regexp
      "No matches for current filter string.\n"
    "No matches found."))

;; File list file management actions

;; (defun nm-absolute-filename (slug &optional extension)
;;   "Return an absolute filename to file named SLUG with optional EXTENSION.
;; If EXTENSION is not given, `nm-extension' is assumed."
;;   (concat (file-name-as-directory (expand-file-name nm-directory))
;;           slug "." (or extension nm-extension)))

;; (defun nm-unused-slug ()
;;   "Return an unused filename slug (short name) in `nm-directory'."
;;   (let* ((fmt "nm-%d")
;;          (counter 0)
;;          (slug (format fmt counter))
;;          (file (nm-absolute-filename slug)))
;;     (while (or (file-exists-p file) (get-file-buffer file))
;;       (setq counter (1+ counter))
;;       (setq slug (format fmt counter))
;;       (setq file (nm-absolute-filename slug)))
;;     slug))

;; (defun nm-update-visiting-buffers (old new)
;;   "Rename visited file of buffers visiting file OLD to NEW."
;;   (let ((buffer (get-file-buffer old)))
;;     (when buffer
;;       (with-current-buffer (get-file-buffer old)
;;         (set-visited-file-name new nil t)
;;         (when (not (eq major-mode nm-text-mode))
;;           (funcall nm-text-mode)
;;           (hack-local-variables))))))

(defun nm-open-match (match)
  (notmuch-show (concat "thread:" (plist-get match :thread))))

;; (defun nm-open-file (file &optional other switch)
;;   "Open FILE in a new buffer and setting its mode.
;; When OTHER is non-nil, open the file in another window.  When
;; OTHER and SWITCH are both non-nil, switch to the other window.
;; FILE must be a relative or absolute path, with extension."
;;   (let ((buffer (find-file-noselect file)))
;;     (with-current-buffer buffer
;;       ;; Set the mode and search forward for the filter string
;;       (when (not (eq major-mode nm-text-mode))
;;         (funcall nm-text-mode)
;;         (hack-local-variables))
;;       (when nm-filter-regexp
;;         (re-search-forward (nm-filter-regexp-as-regexp) nil t))
;;       ;; Ensure that Nm has been initialized
;;       (when (not (get-buffer nm-buffer))
;;         (with-current-buffer (get-buffer-create nm-buffer)
;;           (nm-mode)))
;;       ;; Set up auto save hooks
;;       (add-to-list 'nm-auto-save-buffers buffer)
;;       (add-hook 'after-save-hook
;;                 (lambda () (save-excursion
;;                              (nm-cache-update-file buffer-file-name)
;;                              (nm-refresh-filter)))
;;                 nil t))
;;     (if other
;;         (if switch
;;             (switch-to-buffer-other-window buffer)
;;           (display-buffer buffer other))
;;       (switch-to-buffer buffer))))

;; ;;;###autoload
;; (defun nm-find-file (file)
;;   "Find FILE interactively using the minibuffer.
;; FILE must exist and be a relative or absolute path, with extension.
;; If FILE is not inside `nm-directory', fall back to using `find-file'."
;;   (interactive
;;    (list (read-file-name "Nm find file: " nm-directory)))
;;   (if (and (file-exists-p file)
;;            (string-match (concat "^" (expand-file-name nm-directory)) file))
;;       (nm-open-file file)
;;     (find-file file)))

;; (defun nm-new-file-named (slug)
;;   "Create a new file named SLUG.
;; SLUG is the short filename, without a path or a file extension.
;; If the filter string is non-nil and title is not from file name,
;; use it as the title."
;;   (interactive "sNew filename (without extension): ")
;;   (let ((file (nm-absolute-filename slug)))
;;     (if (file-exists-p file)
;;         (message "Aborting, file already exists: %s" file)
;;       ;; Insert the contents of the filter string in the file.
;;       (when (and nm-filter-regexp (not nm-use-filename-as-title))
;;         (write-region (concat (nm-whole-filter-regexp) "\n\n") nil file nil))
;;       (nm-cache-update-file file)
;;       (nm-refresh-filter)
;;       (nm-open-file file)
;;       (with-current-buffer (get-file-buffer file)
;;         (goto-char (point-max))))))

;; ;;;###autoload
;; (defun nm-new-file ()
;;   "Create a new file quickly.
;; Use either an automatically generated filename or the filter
;; string if non-nil and `nm-use-filename-as-title' is set.  If the
;; filter string is non-nil and title is not from filename, use it
;; as the title."
;;   (interactive)
;;   (let (slug)
;;     (if (and nm-filter-regexp nm-use-filename-as-title)
;;         ;; If the filter string is non-emtpy and titles are taken from
;;         ;; filenames is set, construct filename from filter string.
;;         (setq slug (nm-whole-filter-regexp))
;;       ;; If the filter string is empty, or titles are taken from file
;;       ;; contents, then use an automatically generated unique filename.
;;       (setq slug (nm-unused-slug)))
;;     (nm-new-file-named slug)))

;; (defun nm-open-file-other-window (&optional arg)
;;   "When the point is at a widget, open the file in the other window."
;;   (interactive "P")
;;   (let ((file (widget-get (widget-at) :tag)))
;;     (when file
;;       (nm-open-file file t arg))))

;; (defun nm-delete-file ()
;;   "Delete the file represented by the widget at the point.
;; If the point is not on a file widget, do nothing.  Prompts before
;; proceeding."
;;   (interactive)
;;   (let ((filename (widget-get (widget-at) :tag)))
;;     (when filename
;;       (when (y-or-n-p
;;              (concat "Delete file " (file-name-nondirectory filename) "? "))
;;         (delete-file filename)
;;         (delq filename nm-current-matches)
;;         (delq filename nm-all-matches)
;;         (nm-refresh)))))

(defun nm-delete-animated () ;; not really great.
  "Gratuitous animation!"
  (interactive)
  (let ((bol (line-beginning-position))
        (eol (line-end-position)))
    (beginning-of-line)
    (delete-char (- eol bol))
    (sleep-for 0 300)
    (delete-char 1)))

(defun nm-delete ()
  "Delete it."
  (interactive)
  (let ((match (widget-get (widget-at) :tag)))
     (when match
       (notmuch-tag (concat "thread:" (plist-get match :thread)) '("+deleted" "-unread" "-inbox"))
       (nm-refresh))))

(defun nm-archive ()
  "Archive it."
  (interactive)
  (let ((match (widget-get (widget-at) :tag)))
     (when match
       (notmuch-tag (concat "thread:" (plist-get match :thread)) '("-deleted" "-unread" "-inbox"))
       (nm-refresh))))

;; (defun nm-rename-file ()
;;   "Rename the file represented by the widget at the point
;; If the point is not on a file widget, do nothing."
;;   (interactive)
;;   (let (old-filename new-filename old-name new-name)
;;     (setq old-filename (widget-get (widget-at) :tag))
;;     (when old-filename
;;       (setq old-name (nm-base-filename old-filename))
;;       (setq new-name (read-string
;;                       (concat "Rename " old-name " to (without extension): ")))
;;       (setq new-filename
;;             (concat (file-name-as-directory nm-directory)
;;                     new-name "." nm-extension))
;;       (rename-file old-filename new-filename)
;;       (nm-update-visiting-buffers old-filename new-filename)
;;       (nm-refresh))))

;; (defun nm-archive-file ()
;;   "Archive the file represented by the widget at the point.
;; If the point is not on a file widget, do nothing."
;;   (interactive)
;;   (let (old new name-ext)
;;     (setq old (widget-get (widget-at) :tag))
;;     (when old
;;       (setq name-ext (file-name-nondirectory old))
;;       (setq new (concat nm-archive-directory name-ext))
;;       (when (y-or-n-p (concat "Archive file " name-ext "? "))
;;         ;; if the filename already exists ask for a new name
;;         (while (file-exists-p new)
;;           (setq name-ext (read-string "File exists, choose a new name: " name-ext))
;;           (setq new (concat nm-archive-directory name-ext)))
;;         (when (not (file-exists-p nm-archive-directory))
;;           (make-directory nm-archive-directory t))
;;         (rename-file old new)
;;         (nm-update-visiting-buffers old new)
;;         (nm-refresh)))))

;; File list filtering

;; (defun nm-sort-files (files)
;;   "Sort FILES in reverse order by modified time."
;;   (sort files (lambda (f1 f2) (nm-file-newer-p f1 f2))))

(defun nm-filter-initialize ()
  "Initialize the filter string (nil) and matches list (all matches)."
  (interactive)
  (setq nm-filter-regexp (list "" "tag:inbox"))
  (setq nm-all-count (nm-do-count (nm-whole-filter-regexp)))
  (setq nm-all-matches (nm-do-search (nm-whole-filter-regexp)))
  (setq nm-current-count nm-all-count)
  (nm-set-mode-name)
  (setq nm-current-matches nm-all-matches))

;; (defun nm-filter-match-file (file &optional batch)
;;   "Return FILE if FILE matches the current filter regexp."
;;   (with-temp-buffer
;;     (insert file)
;;     (let ((title (nm-file-title file))
;;           (contents (nm-file-contents file)))
;;       (when title (insert title))
;;       (when contents (insert contents)))
;;     (if batch
;; 	(if (every (lambda (filter)
;; 		     (goto-char (point-min))
;;                      (nm-search-forward filter))
;; 		   nm-filter-regexp)
;; 	    file)
;;       (goto-char (point-min))
;;       (if (nm-search-forward (car nm-filter-regexp))
;; 	  file))))

;; (defun nm-filter-files (files)
;;   "Update `nm-current-matches' given a list of paths, FILES.
;; Apply `nm-filter-match-file' to `nm-all-matches', handling
;; any errors that occur."
;;   (delq nil
;;         (condition-case nil
;;             ;; Map `nm-filter-match-file' onto FILES.  Return
;;             ;; filtered files list and clear error flag if no error.
;;             (progn
;;               (setq nm-regexp-error nil)
;;               (mapcar (lambda (file) (nm-filter-match-file file t)) files))
;;           ;; Upon an error (`invalid-regexp'), set an error flag
;;           (error
;;            (progn
;;              (setq nm-regexp-error t)
;;              files)))))

(defun nm-filter-update ()
  "Update the filtered files list using the current filter regexp.
Starts from scratch using `nm-all-matches'.  Does not refresh the
Nm buffer."
  (if (not nm-filter-regexp)
      (setq nm-current-matches nm-all-matches)
    (progn
      (setq nm-current-count (nm-do-count (nm-search-term)))
      (nm-set-mode-name)
      (setq nm-current-matches (nm-do-search (nm-search-term))))))

;; Filters that cause a refresh

(defun nm-filter-clear ()
  "Clear the current filter string and refresh the file browser."
  (interactive)
  (when nm-filter-regexp
    (setq nm-filter-regexp nil)
    (setq nm-current-matches nm-all-matches)
    (setq nm-current-count nm-all-count)
    (nm-set-mode-name)
    (nm-refresh))
  (message "Filter cleared."))

(defun nm-filter (str &optional reset)
  "Update the filter with STR and update the file browser.

In incremental search mode, the car of `nm-filter-regexp' will
be replaced with STR.  If STR has zero length and the length of
the list is greater than one, the empty string will be retained
to simulate whitespace.  However, if STR has zero length and the
list is of length one, then the filter will be cleared.  If STR
is nil, then the car is removed from the list.

In regexp search mode, the current filter string will be replaced
with STR.

When called interactively, or when RESET is non-nil, always
replace the entire filter string."
  (interactive
   (list (read-from-minibuffer "Filter: " (nm-whole-filter-regexp)
                               nil nil 'nm-filter-history)))
  (if nm-incremental-search
      ;; Incremental search mode
      (if (or (called-interactively-p 'any) reset)
          ;; Called interactively or RESET non-nil
          (if (= (length str) 0)
              (setq nm-filter-regexp nil)
            (setq nm-filter-regexp (reverse (split-string str " "))))
        ;; Called noninteractively
        (if (not str)
            ;; If str is nil, remove it and filter with the cdr
            (setq nm-filter-regexp (cdr nm-filter-regexp))
          ;; Use STR it as the new car, even when empty (to simulate
          ;; whitespace), unless this is the only element in the list.
          (if (and (= (length nm-filter-regexp) 1)
                   (= (length str) 0))
              (setq nm-filter-regexp nil)
            (setcar nm-filter-regexp str))))
    ;; Regexp search mode
    (if (> (length str) 0)
        (setq nm-filter-regexp (list str))
      (setq nm-filter-regexp nil)))
  (nm-filter-update)
  (nm-refresh-browser))

(defun nm-filter-increment ()
  "Append character to the filter regexp and update `nm-current-matches'."
  (interactive)
  (let ((char last-command-event))
    (if (= char ?\S-\ )
	(setq char ?\s))
    (setq char (char-to-string char))
    (if (and nm-incremental-search (string= char " "))
	(setq nm-filter-regexp (cons "" nm-filter-regexp))
      (progn
	(if (car nm-filter-regexp)
	    (setcar nm-filter-regexp (concat (car nm-filter-regexp) char))
	  (setq nm-filter-regexp (list char)))
        (nm-filter-update)
	(nm-refresh-browser)))))

(defun nm-filter-decrement ()
  "Remove last character from the filter, if possible, and update.

In incremental search mode, the elements of `nm-filter-regexp'
are the words of the filter string in reverse order.  In regexp
search mode, the list is a single element containing the entire
filter regexp.  Therefore, in both cases, only the car of
`nm-filter-regexp' is modified."
  (interactive)
  (let ((str (car nm-filter-regexp)))
    (nm-filter
      (if (> (length str) 0)
          ;; If the last string element has at least one character,
          ;; simply remove the last character.
          (substring str 0 -1)
        ;; Otherwise, return nil
        nil))))

(defun nm-filter-decrement-word ()
  "Remove last word from the filter, if possible, and update."
  (interactive)
  (nm-filter
    (if nm-incremental-search
        ;; In incremental search mode, remove the car
        nil
      ;; In regexp search mode, remove last "word" component
      ;(replace-regexp-in-string "[[:space:]\n]*$" "" s)
      (let ((str (car nm-filter-regexp)))
        (if (> (length str) 0)
            (with-temp-buffer
              (insert (concat "\"" str "\""))
              (lisp-interaction-mode)
              (goto-char (- (point-max) 1))
              (backward-word 1)
              (buffer-substring 2 (point)))
          nil)))))

(defun nm-filter-yank ()
  "Append the most recently killed or yanked text to the filter."
  (interactive)
  (nm-filter
    (concat (nm-whole-filter-regexp) (current-kill 0 t)) t))

(defun nm-complete ()
  "Complete the current action.
If there is a widget at the point, press it.  If a filter is
applied and there is at least one match, open the first matching
file.  If there is an active filter but there are no matches,
quick create a new file using the filter string as the title.
Otherwise, quick create a new file."
  (interactive)
  (cond
   ;; Activate widget
   ((widget-at)
    (widget-button-press (point)))
   ;; Active filter string with match
   ((and nm-filter-regexp nm-current-matches)
    (nm-open-file (car nm-current-matches)))
   ;; Default
   (t
    (nm-new-file))))

;;; Automatic File Saving

;; (defun nm-auto-save ()
;;   (save-excursion
;;     (dolist (buf nm-auto-save-buffers)
;;       (if (buffer-name buf)
;;           ;; Save open buffers that have been modified.
;;           (progn
;;             (set-buffer buf)
;;             (when (buffer-modified-p)
;;               (basic-save-buffer)))
;;         ;; If a buffer is no longer open, remove it from auto save list.
;;         (delq buf nm-auto-save-buffers)))))

;;; Mode definition

(defun nm-show-version ()
  "Show the version number in the minibuffer."
  (interactive)
  (message "Nm %s" nm-version))

(defun nm-setup ()
  "Prepare environment by creating the Nm notes directory."
  (interactive)
  ;; (when (not (file-exists-p nm-directory))
  ;;   (make-directory nm-directory t))
  (nm-refresh))

(defvar nm-mode-map
  (let ((i 0)
        (map (make-keymap)))
    ;; Make multibyte characters extend the filter string.
    (set-char-table-range (nth 1 map) (cons #x100 (max-char))
                          'nm-filter-increment)
    ;; Extend the filter string by default.
    (setq i ?\s)
    (while (< i 256)
      (define-key map (vector i) 'nm-filter-increment)
      (setq i (1+ i)))
    ;; Handle backspace and delete
    (define-key map (kbd "DEL") 'nm-filter-decrement)
    (define-key map (kbd "M-DEL") 'nm-filter-decrement-word)
    ;; Handle return via completion or opening file
    (define-key map (kbd "RET") 'nm-complete)
    ;; Filtering
    (define-key map (kbd "C-c C-l") 'nm-filter)
    (define-key map (kbd "C-c C-c") 'nm-filter-clear)
    (define-key map (kbd "C-y") 'nm-filter-yank)
    ;; File creation
    (define-key map (kbd "C-c C-n") 'nm-new-file)
    (define-key map (kbd "C-c C-m") 'nm-new-file-named)
    (define-key map (kbd "<C-return>") 'nm-new-file-named)
    ;; File management
    (define-key map (kbd "C-c C-d") 'nm-delete)
    (define-key map (kbd "C-c C-r") 'nm-rename-file)
    (define-key map (kbd "C-c C-f") 'nm-find-file)
    (define-key map (kbd "C-c C-a") 'nm-archive)
    ;; Settings
    (define-key map (kbd "C-c C-t") 'nm-toggle-incremental-search)
    ;; Miscellaneous
    (define-key map (kbd "C-g")     'nm-reset)
    (define-key map (kbd "C-c C-g") 'nm-refresh)
    (define-key map (kbd "C-c C-q") 'quit-window)
    ;; Widgets
    (define-key map [down-mouse-1] 'widget-button-click)
    (define-key map [down-mouse-2] 'widget-button-click)
    (define-key map (kbd "<tab>") 'widget-forward)
    (define-key map (kbd "<backtab>") 'widget-backward)
    (define-key map (kbd "<S-tab>") 'widget-backward)
    (define-key map (kbd "C-o") 'nm-open-file-other-window)
    map)
  "Keymap for Nm mode.")

;; Nm mode is suitable only for specially-prepared text
(put 'nm-mode 'mode-class 'special)

(defun nm-reset ()
  "Reset the filter string."
  (interactive)
  (nm-filter-initialize)
  (nm-buffer-setup))

(defun nm-mode ()
  "Major mode for quickly browsing, filtering, and editing plain text notes.
Turning on `nm-mode' runs the hook `nm-mode-hook'.

\\{nm-mode-map}."
  (message "Nm initializing...")
  (kill-all-local-variables)
  (setq truncate-lines t)
  (setq buffer-read-only t)
  ;; (setq default-directory (expand-file-name nm-directory))
  (use-local-map nm-mode-map)
  ;; (nm-cache-initialize)
  ;; (nm-cache-update-all)
  (nm-filter-initialize)
  (setq major-mode 'nm-mode)
  (nm-set-mode-name)
  (nm-buffer-setup)
  ;; (when (> nm-auto-save-interval 0)
  ;;   (run-with-idle-timer nm-auto-save-interval t 'nm-auto-save))
  (run-mode-hooks 'nm-mode-hook)
  (message ""))

;;;###autoload
(defun nm ()
  "Switch to *Nm* buffer and load files."
  (interactive)
  (switch-to-buffer nm-buffer)
  (if (not (eq major-mode 'nm-mode))
      (nm-mode)))

(provide 'nm)

;; Local Variables:
;; byte-compile-warnings: (not cl-functions)
;; End:

;;; nm.el ends here
