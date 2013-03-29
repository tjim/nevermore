;;; nm.el --- "nevermore" interface to notmuch
;;; work in progress
;;; based on deft.el

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

(defvar nm-mode-hook nil
  "Hook run when entering Nm mode.")

(defvar nm-filter-string nm-default-filter-string)

(defvar nm-current-matches nil
  "List of matches for the current filter.")

(defvar nm-current-count nil
  "Count of matches for the current filter.")

(defvar nm-window-width nil
  "Width of Nm buffer.")

(defvar nm-window-height nil
  "Height of Nm buffer.")

(defvar nm-date-width 12
  "Width of dates in Nm buffer.")

(defvar nm-authors-width 20
  "Width of authors in Nm buffer.")

(defvar nm-subject-width nil
  "Width of authors in Nm buffer.")

;; Helpers

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

(defun nm-buffer-setup ()
  "Render the match browser in the *Nm* buffer."
  (nm-resize)
  (nm-refresh)
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

(defun nm-draw-buffer ()
  (save-excursion
    (save-window-excursion
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert
         (propertize "Nm: " 'face 'nm-header-face)
         (propertize nm-filter-string 'face 'nm-filter-string-face)
         "\n"
         "\n")
        (mapc 'nm-insert-match nm-current-matches)))))

(defun nm-refresh-count ()
  (setq nm-current-count (nm-do-count nm-filter-string))
  (let ((matches
         (cond ((eq nm-current-count 1) "1 match")
               ((< nm-current-count (- nm-window-height 2)) (format "%d matches" nm-current-count))
               (t (format "%d of %d matches" (- nm-window-height 2) nm-current-count)))))
    (setq mode-name (format "Nm: %s" matches))))

(defun nm-resize ()
  "Call this function if the size of the window changes.  Does NOT re-run the filter."
  (interactive)
  (when (get-buffer nm-buffer)
    (with-current-buffer nm-buffer
      (setq nm-window-width (window-width))
      (setq nm-window-height (window-body-height))
      (setq nm-subject-width (- nm-window-width nm-authors-width nm-date-width (* 2 (length nm-separator))))
      (nm-draw-buffer))))

(defun nm-refresh ()
  "Reapply the filter and refresh the *Nm* buffer."
  (interactive)
  (nm-refresh-count)
  (setq nm-current-matches (nm-do-search nm-filter-string))
  (nm-draw-buffer))

(defun nm-match-at-pos ()
  (nth (- (line-number-at-pos) 3) nm-current-matches))

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

;;; Mode definition

(defun nm-show-version ()
  "Show the version number in the minibuffer."
  (interactive)
  (message "Nm %s" nm-version))

(defvar nm-mode-map
  (let ((map (make-keymap)))
    ;; Handle return via completion or opening file
    (define-key map (kbd "RET") 'nm-open-match)
    ;; File creation
    (define-key map (kbd "C-c C-a") 'nm-archive)
    (define-key map (kbd "C-c C-d") 'nm-delete)
    (define-key map (kbd "C-c C-r") 'nm-refresh)
    (define-key map (kbd "C-c C-q") 'quit-window)
    map)
  "Keymap for Nm mode.")

;; Nm mode is suitable only for specially-prepared text
(put 'nm-mode 'mode-class 'special)

(defun nm-mode ()
  "Major mode for quickly browsing, filtering, and editing plain text notes.
Turning on `nm-mode' runs the hook `nm-mode-hook'.

\\{nm-mode-map}."
  (message "Nm initializing...")
  (kill-all-local-variables)
  (setq truncate-lines t)
  (setq buffer-read-only t)
  (use-local-map nm-mode-map)
  (nm-buffer-setup)
  (setq major-mode 'nm-mode)
  (run-mode-hooks 'nm-mode-hook)
  (add-hook 'window-configuration-change-hook
            (lambda ()
              (when (and (eq (current-buffer) (get-buffer nm-buffer))
                         (not (and (eq nm-window-width (window-width))
                                   (eq nm-window-height (window-body-height)))))
                (nm-resize))))
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
