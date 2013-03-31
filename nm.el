;;; nm.el --- "nevermore" interface to notmuch

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

(defconst nm-version "1.0")

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

(defconst nm-default-filter-string "tag:inbox ")

;; Global variables

(defvar nm-i-am-dead-inside t
  "If you are dead insider.")

(defvar nm-mode-hook nil
  "Hook run when entering Nm mode.")

(defvar nm-filter-string nm-default-filter-string)

(defvar nm-current-matches nil
  "List of matches for the current filter.")

(defvar nm-current-count nil
  "Count of matches for the current filter.")

(defvar nm-current-offset 0)

(defvar nm-query-mode 'thread)

(defvar nm-window-width nil
  "Width of Nm buffer.")

(defvar nm-window-height nil
  "Height of Nm buffer.")

(defvar nm-results-per-screen nil
  "Number of results that can fit on one screen.")

(defvar nm-date-width 12
  "Width of dates in Nm buffer.")

(defvar nm-authors-width 20
  "Width of authors in Nm buffer.")

(defvar nm-subject-width nil
  "Width of authors in Nm buffer.")

;; Helpers

(defun nm-splash-screen ()
  (let ((animation-buffer-restore
         (if (boundp 'animation-buffer-name)
             (let ((saved-animation-buffer-name animation-buffer-name))
               (lambda () (setq animation-buffer-name saved-animation-buffer-name)))
           (lambda () (makunbound 'animation-buffer-name)))))
    (unwind-protect
        (when (not nm-i-am-dead-inside)
          (setq animation-buffer-name nm-buffer)
          (animate-sequence '("N E V E R M O R E") 0)
          (sit-for (log 4)))
      (funcall animation-buffer-restore))))

(defun nm-thread-mode ()
  (equal nm-query-mode 'thread))

(defun nm-toggle-query-mode ()
  (interactive)
  (if (nm-thread-mode)
      (setq nm-query-mode 'message)
    (setq nm-query-mode 'thread))
  (nm-refresh))

(defun nm-do-search (query)
  (if (nm-thread-mode)
      (ignore-errors
        (notmuch-call-notmuch-json
         "search"
         "--output=summary"
         (format "--offset=%d" nm-current-offset)
         (format "--limit=%d" nm-results-per-screen)
         "--format=json"
         "--format-version=1"
         "--sort=newest-first"
         query))
    (ignore-errors
      (let ((messages (notmuch-call-notmuch-json
                       "search"
                       "--output=messages"
                       (format "--offset=%d" nm-current-offset)
                       (format "--limit=%d" nm-results-per-screen)
                       "--format=json"
                       "--format-version=1"
                       "--sort=newest-first"
                       query)))
        (mapcar
         (lambda (message-id)
           (plist-put
            (car
             (notmuch-call-notmuch-json
              "search"
              "--output=summary"
              "--format=json"
              "--format-version=1"
              (concat "id:" message-id)))
            :id message-id))
         messages)))))

(defun nm-do-count (query)
  (let ((output (if (nm-thread-mode)
                    "--output=threads"
                  "--output=messages")))
  (or 
   (ignore-errors
     (notmuch-call-notmuch-json
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

(defun nm-goto-first-match-pos ()
  "Render the match browser in the *Nm* buffer."
  (goto-char 1)
  (forward-line 2))

(defun nm-match-text (match)
  "Return text for a line for the given MATCH."
  (when match
    (let* ((date (plist-get match :date_relative))
           (authors (plist-get match :authors))
           (subject (plist-get match :subject))
           (tags (plist-get match :tags)))
      (concat
       (propertize
        (nm-truncate-or-pad nm-authors-width
                            (if authors authors
                              nm-empty-authors))
        'face 'nm-authors-face)
      (propertize nm-separator 'face 'nm-separator-face)
      (propertize
       (nm-truncate-or-pad nm-subject-width
                           (if (and subject (> (length subject) 0)) subject
                             nm-empty-subject))
       'face (if (member "unread" tags) 'nm-unread-face 'nm-read-face))
      (propertize nm-separator 'face 'nm-separator-face)
      (propertize
        (nm-truncate-or-pad nm-date-width
                            (if date date
                              nm-empty-date))
        'face 'nm-date-face)))))

(defun nm-insert-match (match)
  "Add a line to the match browser for the given MATCH."
  (when match
    (insert (nm-match-text match) "\n")))

(defun nm-draw-header ()
  (save-excursion
    (save-window-excursion
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert
         (propertize "Nm: " 'face 'nm-header-face)
         (propertize (nm-chomp nm-filter-string) 'face 'nm-filter-string-face)
         "\n"
         "\n")))))

(defun nm-update-buffer (old new)
  (save-excursion
    (save-window-excursion
      (let ((inhibit-read-only t))
        (nm-goto-first-match-pos)
        (nm-update-lines old new)))))

(defun nm-match-equal (a b)
  (and (equal (plist-get a :thread) (plist-get b :thread))
       (equal (plist-get a :tags) (plist-get b :tags))))

(defun nm-forward-match ()
  (interactive)
  (forward-line 1))
  ;; (let ((index (nm-match-index-at-pos)))
  ;;   (when (and index (< (+1 index) nm-current-count))
  ;;     (forward-line 1))))

(defun nm-update-lines (old new)
                                        ; invariant: if old then we are at the beginning of the line for (car old)
  (cond
   ((and (not old) (not new))
    '())
   ((not old)
    (mapc 'nm-insert-match new))
   ((not new)
    (delete-region (point) (point-max)))
   ((nm-match-equal (car old) (car new))
    (progn
      (nm-forward-match)
      (nm-update-lines (cdr old) (cdr new))))
   (t
    (progn
      (delete-region (point) (line-end-position))
      (insert (nm-match-text (car new)))
      (nm-forward-match)
      (nm-update-lines (cdr old) (cdr new))))))

(defun nm-refresh-count ()
  (setq nm-current-count (nm-do-count nm-filter-string))
  (let ((matches (cond ((eq nm-current-count 1) "1 match")
                       (t (format "%d matches" nm-current-count)))))
    (if (< nm-current-count nm-results-per-screen)
        (setq mode-name (format "Nm: %s" matches))
      (let ((first-match (1+ nm-current-offset))
            (last-match (min nm-current-count (+ nm-current-offset nm-results-per-screen))))
        (setq mode-name (format "Nm: %d-%d of %s" first-match last-match matches))))))

(defun nm-resize ()
  "Call this function if the size of the window changes."
  (interactive)
  (when (get-buffer nm-buffer)
    (with-current-buffer nm-buffer
      (setq nm-window-height (window-body-height))
      (setq nm-results-per-screen (- nm-window-height 2))
      (setq nm-window-width (window-width))
      (setq nm-subject-width (- nm-window-width nm-authors-width nm-date-width (* 2 (length nm-separator))))
      (nm-refresh))))

(defun nm-refresh ()
  "Reapply the filter and refresh the *Nm* buffer."
  (interactive)
  (when (get-buffer nm-buffer)
    (with-current-buffer nm-buffer
      (nm-refresh-count)
      (save-excursion
        (save-window-excursion
          (let ((inhibit-read-only t))
            (goto-char 5)
            (delete-region (point) (line-end-position))
            (insert (propertize (nm-chomp nm-filter-string) 'face 'nm-filter-string-face)))))
      (let ((old nm-current-matches))
        (setq nm-current-matches (nm-do-search nm-filter-string))
        (nm-update-buffer old nm-current-matches)))))

(defun nm-match-index-at-pos ()
  (let ((index (- (line-number-at-pos) 3)))
    (if (or (< index 0)
            (>= index nm-current-count))
        nil
      index)))

(defun nm-match-at-pos ()
  (let ((index (nm-match-index-at-pos)))
    (when index
      (nth index nm-current-matches))))

(defun nm-open-match ()
  "Open it."
  (interactive)
  (let ((match (nm-match-at-pos)))
    (when match
      (notmuch-show (concat "thread:" (plist-get match :thread))))))

(defun nm-delete ()
  "Delete it."
  (interactive)
  (let ((match (nm-match-at-pos)))
    (when match
      (notmuch-tag (concat "thread:" (plist-get match :thread)) '("+deleted" "-unread" "-inbox"))
      (nm-refresh))))

(defun nm-archive ()
  "Archive it."
  (interactive)
  (let ((match (nm-match-at-pos)))
    (when match
      (notmuch-tag (concat "thread:" (plist-get match :thread)) '("-deleted" "-unread" "-inbox"))
      (nm-refresh))))

;;; Le incremental search

(defun nm-minibuffer-contents ()
  "Return the contents of the minibuffer when it is active."
  (if (active-minibuffer-window)
      (with-current-buffer (window-buffer (active-minibuffer-window))
        (minibuffer-contents))))

(defun nm-minibuffer-refresh ()
  (let ((s (nm-minibuffer-contents)))
    (if (or (not s) (equal (nm-chomp s) ""))
        (setq nm-filter-string "*")
      (setq nm-filter-string s))
    (setq nm-current-offset 0)
    (nm-refresh)))

(defun nm-incrementally ()
  "Read string with PROMPT and display results incrementally."
  (interactive)
  (unwind-protect
      (minibuffer-with-setup-hook
          (lambda ()
            (insert nm-filter-string)
            (goto-char (point-max)))
        (progn
          (add-hook 'post-command-hook 'nm-minibuffer-refresh)
          (read-string "Filter: ")))
    (remove-hook 'post-command-hook 'nm-minibuffer-refresh)))

;;; Navigation within results

(defun nm-forward ()
  (interactive)
  (let ((new-offset (+ nm-current-offset nm-results-per-screen)))
    (when (< new-offset nm-current-count)
      (setq nm-current-offset new-offset)
      (nm-refresh))))

(defun nm-backward ()
  (interactive)
  (let ((new-offset (max 0 (- nm-current-offset nm-results-per-screen))))
      (setq nm-current-offset new-offset)
      (nm-refresh)))

;;; Thread display

(defun nm-get-message (message-id)
  (notmuch-call-notmuch-json
   "show"
   "--format=json"
   (concat "id:" message-id)))
(defun nm-flat-thread ()
  (interactive)
  (let ((match (nm-match-at-pos)))
    (when match
      (let* ((thread-id (concat "thread:" (plist-get match :thread)))
             (messages 
              (mapcar 'nm-get-message
                      (notmuch-call-notmuch-json
                       "search"
                       "--output=messages"
                       "--format=json"
                       "--sort=oldest-first"
                       thread-id))))
        (switch-to-buffer "FOO")
        (mapc (lambda (m) (insert (format "%S\n" m))) messages)))))

;;; Mode definition

(defun nm-show-version ()
  "Show the version number in the minibuffer."
  (interactive)
  (message "Nm %s" nm-version))

(defvar nm-mode-map
  (let ((map (make-keymap)))
    (define-key map (kbd "RET") 'nm-open-match)
    (define-key map [remap scroll-up-command] 'nm-forward)
    (define-key map [remap scroll-down-command] 'nm-backward)
    (define-key map (kbd "C-c C-a") 'nm-archive)
    (define-key map (kbd "C-c C-d") 'nm-delete)
    (define-key map (kbd "C-c C-f") 'nm-incrementally)
    (define-key map (kbd "C-c C-m") 'nm-toggle-query-mode)
    (define-key map (kbd "C-c C-r") 'nm-refresh)
    (define-key map (kbd "C-c C-q") 'quit-window)
    (define-key map (kbd "C-c C-t") 'nm-flat-thread)
    map)
  "Keymap for Nm mode.")

;; Nm mode is suitable only for specially-prepared text
(put 'nm-mode 'mode-class 'special)

(defun nm-mode ()
  "Major mode for mail.
Turning on `nm-mode' runs the hook `nm-mode-hook'.

\\{nm-mode-map}."
  (kill-all-local-variables)
  (setq truncate-lines t)
  (nm-splash-screen)
  (setq buffer-read-only t)
  (use-local-map nm-mode-map)
  (nm-draw-header)
  (setq nm-current-matches nil)
  (setq nm-current-count 0)
  (setq nm-current-offset 0)
  (nm-resize)
  (nm-goto-first-match-pos)
  (setq major-mode 'nm-mode)
  (run-mode-hooks 'nm-mode-hook)
  (add-hook 'window-configuration-change-hook
            (lambda ()
              (when (and (eq (current-buffer) (get-buffer nm-buffer))
                         (not (and (eq nm-window-width (window-width))
                                   (eq nm-window-height (window-body-height)))))
                (nm-resize)))))

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
