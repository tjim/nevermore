; Mail address completion for nevermore via company-mode

; NB company-minimum-prefix-length defaults to 3 so you don't get completion unless you type 3 characters

(require 'company)
(require 'message)
(require 'nm)

(defvar-local nm-company-last-prefix nil)
;;;###autoload
(defun nm-company (command &optional arg &rest ignore)
  "`company-mode' completion back-end for `nevermore (nm)'."
  (interactive (list 'interactive))
  (let ((case-fold-search t))
    (pcase command
      (`interactive (company-begin-backend 'nm-company))
      (`prefix (and (eq major-mode 'message-mode)
                    (looking-back "^\\(To\\|Cc\\|Bcc\\):.*"
                                  (line-beginning-position))
                    (setq nm-company-last-prefix (company-grab-symbol))))
      (`candidates (let ((results (completion-substring--all-completions arg nm-completion-addresses nil 0)))
                     (when results (car results))))
      (`match (if (string-match nm-company-last-prefix arg)
                  (match-end 0)
                0))
      (`no-cache t))))

(add-hook 'message-mode-hook '(lambda ()
                                (company-mode)
                                (make-local-variable 'company-backends)
                                (setq company-backends '(nm-company))))
(provide 'nm-company)
