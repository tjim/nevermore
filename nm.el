;;; nm.el: N E V E R M O R E
;;; Emacs mail application

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
  '((t :inherit font-lock-constant-face :bold t))
  "Face for Nm dates."
  :group 'nm-faces)

;; Constants

(defconst nm-version "1.0")

(defconst nm-buffer "*nm*"
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

(defvar nm-i-am-dead-inside t
  "If you are dead inside.")

(defvar nm-mode-hook nil
  "Hook run when entering Nm mode.")

(defvar nm-query nm-default-query)

(defvar nm-results nil
  "List of a screen's worth of results for the current query.")

(defvar nm-all-results-count nil
  "Count of all results for the current query.")

(defvar nm-current-offset 0)

(defvar nm-query-mode 'message) ; or 'thread

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
          (sit-for 1))
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

(defun nm-goto-first-result-pos ()
  (goto-char 1))

(defun nm-result-line (result)
  "Return a line of text for a RESULT."
  (when result
    (let* ((date (plist-get result :date_relative))
           (authors (plist-get result :authors))
           (subject (plist-get result :subject))
           (tags (plist-get result :tags)))
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

(defun nm-update-buffer (old new)
  (save-excursion
    (save-window-excursion
      (let ((inhibit-read-only t))
        (nm-goto-first-result-pos)
        (nm-update-lines old new)))))

(defun nm-result-equal (a b)
  (and (equal (plist-get a :thread) (plist-get b :thread))
       (equal (plist-get a :tags) (plist-get b :tags))))

(defun nm-forward-result ()
  (interactive)
  (forward-line 1))
  ;; (let ((index (nm-result-index-at-pos)))
  ;;   (when (and index (< (+1 index) nm-all-results-count))
  ;;     (forward-line 1))))

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
      (nm-forward-result)
      (nm-update-lines (cdr old) (cdr new))))))

(defun nm-refresh-count ()
  (setq nm-all-results-count (nm-do-count nm-query))
  (let ((results (cond ((eq nm-all-results-count 1) "1 result")
                       ((eq nm-all-results-count 0) "no results")
                       (t (format "%d results" nm-all-results-count)))))
    (if (< nm-all-results-count nm-results-per-screen)
        (setq mode-name (format "nevermore: %s" results))
      (let ((first-result (1+ nm-current-offset))
            (last-result (min nm-all-results-count (+ nm-current-offset nm-results-per-screen))))
        (setq mode-name (format "nevermore: %d-%d of %s" first-result last-result results))))))

(defun nm-resize ()
  "Call this function if the size of the window changes."
  (interactive)
  (when (get-buffer nm-buffer)
    (with-current-buffer nm-buffer
      (setq nm-window-height (window-body-height))
      (setq nm-results-per-screen nm-window-height)
      (setq nm-window-width (window-width))
      (setq nm-subject-width (- nm-window-width nm-authors-width nm-date-width (* 2 (length nm-separator))))
      (nm-refresh))))

(defun nm-refresh ()
  "Reapply the query and refresh the *nm* buffer."
  (interactive)
  (when (get-buffer nm-buffer)
    (with-current-buffer nm-buffer
      (nm-refresh-count)
      (nm-draw-header)
      (let ((old nm-results))
        (setq nm-results (nm-do-search nm-query))
        (nm-update-buffer old nm-results)))))

(defun nm-result-index-at-pos ()
  (let ((index (- (line-number-at-pos) 1)))
    (if (or (< index 0)
            (>= index nm-all-results-count))
        nil
      index)))

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

(defun nm-show-message (result)
  (save-excursion
    (let* ((msg-id (concat "id:" (plist-get result :id)))
           (thread-id (concat "thread:" (plist-get result :thread)))
           (forest (ignore-errors
                     (notmuch-call-notmuch-json
                      "show"
                      "--entire-thread=false"
                      "--format=json"
                      "--format-version=1"
                      msg-id)))
           (msgs (nm-flatten-forest forest)) ; we expect a single msg only
           (buffer (get-buffer-create nm-view-buffer)))
      (display-buffer buffer)
      (with-current-buffer buffer
        (setq notmuch-show-thread-id thread-id
              notmuch-show-process-crypto notmuch-crypto-process-mime
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
          (notmuch-show-goto-first-wanted-message))))))

(defun nm-open-result ()
  "Open it."
  (interactive)
  (let ((result (nm-result-at-pos)))
    (when result
      (if (nm-thread-mode)
          (notmuch-show (concat "thread:" (plist-get result :thread)))
        (nm-show-message result)))))

(defun nm-open-result-1 ()
  "Open it."
  (interactive)
  (let ((result (nm-result-at-pos)))
    (when result
      (if (nm-thread-mode)
          (notmuch-show (concat "thread:" (plist-get result :thread)))
        ;; (let ((notmuch-show-elide-non-resulting-messages t))
          ;; (notmuch-show (concat "id:" (plist-get result :id)))

        (let* ((id (concat "id:" (plist-get result :id))); see notmuch-show-view-raw-message
               (buf (get-buffer-create (concat "*notmuch-raw-" id "*"))))
          (notmuch-tag id '("-unread"))
          (call-process notmuch-command nil buf nil "show" "--format=raw" id)
          (switch-to-buffer buf)
          (goto-char (point-min))
          (save-excursion
            (save-restriction
              (narrow-to-region (point-min) (point-max))
              (run-hook-with-args 'notmuch-show-insert-text/plain-hook nil 0)))
          (set-buffer-modified-p nil)
          (view-buffer buf 'kill-buffer-if-not-modified))
        ))))

(defun nm-apply-to-result (fn)
  (let ((result (nm-result-at-pos)))
    (when result
      (let ((query
             (if (nm-thread-mode)
                 (concat "thread:" (plist-get result :thread))
               (concat "id:" (plist-get result :id)))))
      (funcall fn query)))))

(defun nm-delete ()
  "Delete it."
  (interactive)
  (nm-apply-to-result (lambda (q)
                        (notmuch-tag q '("+deleted" "-unread" "-inbox"))
                        (nm-refresh))))

(defun nm-archive ()
  "Delete it."
  (interactive)
  (nm-apply-to-result (lambda (q)
                        (notmuch-tag q '("-deleted" "-unread" "-inbox"))
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
        (setq nm-query "*")
      (setq nm-query s))
    (setq nm-current-offset 0)
    (nm-refresh)))

(defun nm-incrementally ()
  "Read query and display results incrementally."
  (interactive)
  (unwind-protect
      (minibuffer-with-setup-hook
          (lambda ()
            (insert nm-query)
            (goto-char (point-max)))
        (progn
          (add-hook 'post-command-hook 'nm-minibuffer-refresh)
          (read-string "Query: ")))
    (remove-hook 'post-command-hook 'nm-minibuffer-refresh)))

;;; Navigation within results

(defun nm-forward ()
  (interactive)
  (let ((new-offset (+ nm-current-offset nm-results-per-screen)))
    (when (< new-offset nm-all-results-count)
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
  (let ((result (nm-result-at-pos)))
    (when result
      (let* ((thread-id (concat "thread:" (plist-get result :thread)))
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
                              (message "junk: %d" arg)
                              (nm-bogo-junk q)
                              (notmuch-tag q '("+junk" "+deleted" "-unread" "-inbox")))
                          (progn
                            (message "not junk: %d" arg)
                            (nm-bogo-not-junk q)
                            (notmuch-tag q '("-junk" "-deleted")))
                        (nm-refresh)))))

;;; Mode definition

(defun nm-show-version ()
  "Show the version number in the minibuffer."
  (interactive)
  (message "Nm %s" nm-version))

(defvar nm-mode-map
  (let ((map (make-keymap)))
    (define-key map (kbd "RET") 'nm-open-result)
    (define-key map [remap scroll-up-command] 'nm-forward)
    (define-key map [remap scroll-down-command] 'nm-backward)
    (define-key map (kbd "C-c C-a") 'nm-archive)
    (define-key map (kbd "C-c C-d") 'nm-delete)
    (define-key map (kbd "C-c C-f") 'nm-incrementally)
    (define-key map (kbd "C-c C-j") 'nm-junk)
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
  (let ((inhibit-read-only t))
    (erase-buffer))
  (nm-draw-header)
  (setq nm-results nil)
  (setq nm-all-results-count 0)
  (setq nm-current-offset 0)
  (nm-resize)
  (nm-goto-first-result-pos)
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
  "Switch to *nm* buffer and load files."
  (interactive)
  (switch-to-buffer nm-buffer)
  (if (not (eq major-mode 'nm-mode))
      (nm-mode)))

(provide 'nm)
