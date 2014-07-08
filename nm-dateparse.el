; nm-dateparse.el
; natural language date parser for nevermore
; some kind of mutant version of chronic (https://github.com/mojombo/chronic)

(require 'peg)

;;; Times
;;; We say an etime is a time as returned by encode-time
;;; We say a dtime is a time as returned by decode-time

(defun nm-etime-compare (a b)
  "Return <0 if a is before b, >0 if b is before a, 0 if a and b are the same time."
  (if (eq (car a) (car b))
      (- (cadr a) (cadr b))
    (- (car a) (car b))))

(defun nm-etime-before (a b)
  (< (nm-etime-compare a b) 0))

(defvar nm-endian 'little) ; 'little = M/D/Y, 'middle = D/M/Y

(defun nm-next-month (&optional dtime)
  "One month from now, or specified dtime"
  (pcase (or dtime (decode-time))
    (`(,SECONDS ,MINUTES ,HOUR ,DAY ,MONTH ,YEAR ,DOW ,DST ,ZONE)
     (nm-noon
      (if (eq MONTH 12)
          `(,SECONDS ,MINUTES ,HOUR ,DAY ,1 ,(1+ YEAR) ,DOW ,DST ,ZONE)
        `(,SECONDS ,MINUTES ,HOUR ,DAY ,(1+ MONTH) ,YEAR ,DOW ,DST ,ZONE))))))

(defun nm-last-month (&optional dtime)
  "One month before now, or specified dtime"
  (pcase (or dtime (decode-time))
    (`(,SECONDS ,MINUTES ,HOUR ,DAY ,MONTH ,YEAR ,DOW ,DST ,ZONE)
     (nm-noon
      (if (eq MONTH 1)
          `(,SECONDS ,MINUTES ,HOUR ,DAY ,12 ,(1- YEAR) ,DOW ,DST ,ZONE)
        `(,SECONDS ,MINUTES ,HOUR ,DAY ,(1- MONTH) ,YEAR ,DOW ,DST ,ZONE))))))

(defconst SECONDS-IN-DAY (* 24 60 60))

(defun nm-next-week (&optional dtime)
  "One week from now, or from specified dtime"
  (let* ((etime (if dtime (apply 'encode-time dtime) (current-time)))
         (target-etime (time-add etime (seconds-to-time (* 7 SECONDS-IN-DAY))))
         (target-dtime (decode-time target-etime)))
    (nm-noon target-dtime)))

(defun nm-last-week (&optional dtime)
  "One week before now, or from specified dtime"
  (let* ((etime (if dtime (apply 'encode-time dtime) (current-time)))
         (target-etime (time-add etime (seconds-to-time (- (* 7 SECONDS-IN-DAY)))))
         (target-dtime (decode-time target-etime)))
    (nm-noon target-dtime)))

(defun nm-tomorrow (&optional dtime)
  "One day from now, or from specified dtime"
  (let* ((etime (if dtime (apply 'encode-time dtime) (current-time)))
         (target-etime (time-add etime (seconds-to-time SECONDS-IN-DAY)))
         (target-dtime (decode-time target-etime)))
    (nm-noon target-dtime)))

(defun nm-yesterday (&optional dtime)
  "One day before now, or from specified dtime"
  (let* ((etime (if dtime (apply 'encode-time dtime) (current-time)))
         (target-etime (time-add etime (seconds-to-time (- SECONDS-IN-DAY))))
         (target-dtime (decode-time target-etime)))
    (nm-noon target-dtime)))

(defun nm-next-dow (dow &optional dtime)
  "Next day-of-week (0=sunday, etc.) from now, or from specified dtime"
  (pcase (or dtime (decode-time))
    (`(,SECONDS ,MINUTES ,HOUR ,DAY ,MONTH ,YEAR ,DOW ,DST ,ZONE)
     (let* ((delta-days (if (> dow DOW) (- dow DOW) (- (+ 7 dow) DOW)))
            (delta-seconds (* SECONDS-IN-DAY delta-days))
            (etime (apply 'encode-time `(,SECONDS ,MINUTES ,HOUR ,DAY ,MONTH ,YEAR ,DOW ,DST ,ZONE)))
            (target-etime (time-add etime (seconds-to-time delta-seconds)))
            (target-dtime (decode-time target-etime)))
       (nm-noon target-dtime)))))

(defun nm-last-dow (dow &optional dtime)
  "Last day-of-week (0=sunday, etc.) from now, or from specified dtime"
  (pcase (or dtime (decode-time))
    (`(,SECONDS ,MINUTES ,HOUR ,DAY ,MONTH ,YEAR ,DOW ,DST ,ZONE)
     (let* ((delta-days (if (> DOW dow) (- dow DOW) (- dow 7 DOW)))
            (delta-seconds (* SECONDS-IN-DAY delta-days))
            (etime (apply 'encode-time `(,SECONDS ,MINUTES ,HOUR ,DAY ,MONTH ,YEAR ,DOW ,DST ,ZONE)))
            (target-etime (time-add etime (seconds-to-time delta-seconds)))
            (target-dtime (decode-time target-etime)))
       (nm-noon target-dtime)))))

(defun set-time-of-day (target &optional dtime)
  (pcase target
    (`(,T-SECONDS ,T-MINUTES ,T-HOUR . ,_)
     (pcase (or dtime (decode-time))
       (`(,SECONDS ,MINUTES ,HOUR ,DAY ,MONTH ,YEAR ,DOW ,DST ,ZONE)
        `(,T-SECONDS ,T-MINUTES ,T-HOUR ,DAY ,MONTH ,YEAR ,DOW ,DST ,ZONE))))
    (_ (or dtime (decode-time)))))

(defun nm-morning (&optional dtime)
  (nm-handle-time `(time nil 9) dtime))

(defun nm-noon (&optional dtime)
  (nm-handle-time `(time t 12) dtime))

(defun nm-afternoon (&optional dtime)
  (nm-handle-time `(time t 3) dtime))

(defun nm-evening (&optional dtime)
  (nm-handle-time `(time t 6 30) dtime))

(defun nm-midnight (&optional dtime)
  (nm-handle-time `(time nil 0) (nm-tomorrow dtime))) ; at midnight, this returns time + 1 day

(defun nm-canonical-dtime (dtime)
  (decode-time (apply 'encode-time dtime)))
(defun nm-valid-dtime (dtime)
  (equal dtime (nm-canonical-dtime dtime)))
(defun nm-validate-dtime (dtime)
  (unless (nm-valid-dtime dtime) (error "Error: invalid date/time %S" dtime))
  dtime)
(defun nm-handle-time (time &optional dtime)
  (pcase time
    (`(time ,pm . ,x)
     (let* ((raw-hour (nth 0 x))
            (hour (cond
                   ((and (not pm) (eq raw-hour 12)) 0)
                   ((not pm) raw-hour)
                   ((and pm (eq raw-hour 12)) 12)
                   (pm (+ 12 raw-hour))))
            (minutes (or (nth 1 x) 0))
            (seconds (or (nth 2 x) 0)))
       (pcase (or dtime (decode-time))
         (`(,SECONDS ,MINUTES ,HOUR ,DAY ,MONTH ,YEAR ,DOW ,DST ,ZONE)
          `(,seconds ,minutes ,hour ,DAY ,MONTH ,YEAR ,DOW ,DST ,ZONE)))))
    (_ (nm-noon))))
(defun nm-handle-endian (a b year time)
  (let ((month (if (eq nm-endian 'little) a b))
        (day   (if (eq nm-endian 'little) b a)))
    (nm-canonical-dtime ; to fix dow
     (pcase (nm-handle-time time)
       (`(,SECONDS ,MINUTES ,HOUR ,DAY ,MONTH ,YEAR ,DOW ,DST ,ZONE)
        `(,SECONDS ,MINUTES ,HOUR ,day ,month ,year ,DOW ,DST ,ZONE))))))
(defun nm-handle-dow-m-d-y-t (dow month day year time)
  "Return a dtime for (dow month day year time)"
  (nm-validate-dtime
   (pcase (nm-handle-time time)
     (`(,SECONDS ,MINUTES ,HOUR ,DAY ,MONTH ,YEAR ,DOW ,DST ,ZONE)
      `(,SECONDS ,MINUTES ,HOUR ,day ,month ,year ,dow ,DST ,ZONE)))))
(defun nm-handle-week (a b)
  (pcase a
      (`last (nm-last-week))
      (`next (nm-next-week))
      (`this (nm-last-dow 1 (nm-next-dow 1))))) ; this monday)
(defun nm-handle-day (a b c)
  (set-time-of-day c
                   (pcase a
                     (`last (nm-yesterday))
                     (`next (nm-tomorrow))
                     (`this (nm-noon)))))
(defun nm-handle-month-name (a b)
  (pcase a
      (`last (nm-last-month b))
      (`next (nm-next-month b))
      (`this (nm-last-month b (nm-next-month b)))
      (_     (nm-last-month b (nm-next-month b)))))
(defun nm-handle-day-name (a b c)
  (pcase a
      (`last (nm-last-dow b))
      (`next (nm-next-dow b))
      (`this (nm-last-dow b (nm-next-dow b)))
      (_     (nm-last-dow b (nm-next-dow b)))))

(defun nm-dateparse ()
  "Parse a date/time at point in the current buffer"
  (interactive)
  (let ((starting-point (point))
        (result
         (ignore-errors
           (peg-parse
            (date (or
                                        ; endian
                   (and number (or @/ @-) number (or @/ @-) number-opt (opt @@) time-opt `(a b c d -- (nm-handle-endian a b c d)))
                                        ; emacs standard date format
                   (and day-name @ month-name @ number @ time @ number `(a b c d e -- (nm-handle-dow-m-d-y-t a b c e d)))
                                        ; date
                   (and day-name @ month-name @ number @ number-opt (opt @@) time-opt `(a b c d e -- (nm-handle-dow-m-d-y-t a b c d e)))
                                        ; my hacks
                   (and grabber week `(a b -- (nm-handle-week a b)))
                   (and tomorrow @@-opt time-opt `(a -- (nm-handle-time a (nm-tomorrow))))
                   (and yesterday (opt @@) time-opt `(a -- (nm-handle-time a (nm-yesterday))))
                   (and noon `(a b -- (nm-handle-day 'this b)))
                   (and grabber-opt @ (or (and month-name                 `(a b c -- (nm-handle-month-name a b)))
                                          (and day-name (opt @@) time-opt `(a b c -- (nm-handle-day-name a b c)))))
                   (and day-name (opt @ month-name @ number (opt @comma number))) ; Tuesday May 1, 2012
                   (and month-name @ number (opt @comma number))))                ; May 9, 2012
            (number-opt (or number empty))
            (time-opt (or time empty))
            (grabber-opt (or grabber empty))
            (empty "" `(-- nil))

            (from "from"     (eow))
            (at "at"         (eow))
            (hence "hence"   (eow))
            (now "now"       (eow))
            (before "before" (eow))
            (to "to"         (eow))
            (till "till"     (eow))
            (ago "ago"       (eow))

            (grabber (or last this next))
            (last "last" (eow) `(-- 'last))
            (this "this" (eow) `(-- 'this))
            (next "next" (eow) `(-- 'next))

            (pointer (or past future))
            (past "past"               (eow) `(-- 'past))
            (future (or "future" "in") (eow) `(-- 'future))

            (noon "noon"           (eow))
            (quarter "quarter"     (eow))
            (half "half"           (eow))
            (during "during"       (eow))
            (oclock "oclock"       (eow))
            (after "after"         (eow))
            (yesterday "yesterday" (eow))
            (tomorrow "tomorrow"   (eow))

                                        ; RepeaterTime
            (time (list (and number (opt (and ":" number (opt (and ":" number))))))
                  day-portion-opt
                  `(a b -- `(time ,b ,@a)))
            (day-portion-opt (or day-portion empty))
                                        ; misc Repeaters
            (opt-s (opt "s"))
            (year "year" opt-s                          (eow) `(-- 'year))
            (season "season" opt-s                      (eow) `(-- 'season))
            (month "month" opt-s                        (eow) `(-- 'month))
            (fortnight "fortnight" opt-s                (eow) `(-- 'fortnight))
            (week "week" opt-s                          (eow) `(-- 'week))
            (weekend "weekend" opt-s                    (eow) `(-- 'weekend))
            (weekday (or "week" "business") "day" opt-s (eow) `(-- 'weekday))
            (day "day" opt-s                            (eow) `(-- 'day))
            (hour (or "hr" "hour") opt-s                (eow) `(-- 'hour))
            (minute (or "min" "minute") opt-s           (eow) `(-- 'minute))
            (second (or "sec" "second") opt-s           (eow) `(-- 'second))

            (day-portion (or am pm morning afternoon evening night)) ; RepeaterDayPortion
            (am (or "am" "a.m." "a.m") (eow) `(-- nil))
            (pm (or "pm" "p.m." "p.m") (eow) `(-- t))
            (morning "morning"         (eow) `(-- nil))
            (afternoon "afternoon"     (eow) `(-- t))
            (evening "evening"         (eow) `(-- t))
            (night (or "night" "nite") (eow) `(-- t))

            (season-name (or spring summer autumn winter)) ; RepeaterSeasonName
            (spring "spring"             (eow) `(--  0))
            (summer "summer"             (eow) `(--  1))
            (autumn (or "autumn" "fall") (eow) `(--  2))
            (winter "winter"             (eow) `(--  3))

            (day-name (or sun mon tue wed thu fri sat))
            (sun (or "sunday" "sun")    (eow) `(-- 0))
            (mon (or "monday" "mon")    (eow) `(-- 1))
            (tue (or "tuesday" "tue")   (eow) `(-- 2))
            (wed (or "wednesday" "wed") (eow) `(-- 3))
            (thu (or "thursday" "thu")  (eow) `(-- 4))
            (fri (or "friday" "fri")    (eow) `(-- 5))
            (sat (or "saturday" "sat")  (eow) `(-- 6))

            (month-name (or jan feb mar apr may jun jul aug sep oct nov dec)) ; RepeaterMonthName
            (jan (or "january" "jan")   (eow) `(--  1))
            (feb (or "february" "feb")  (eow) `(--  2))
            (mar (or "march" "mar")     (eow) `(--  3))
            (apr (or "april" "apr")     (eow) `(--  4))
            (may (or "may" "may")       (eow) `(--  5))
            (jun (or "june" "jun")      (eow) `(--  6))
            (jul (or "july" "jul")      (eow) `(--  7))
            (aug (or "august" "aug")    (eow) `(--  8))
            (sep (or "september" "sep") (eow) `(--  9))
            (oct (or "october" "oct")   (eow) `(-- 10))
            (nov (or "november" "nov")  (eow) `(-- 11))
            (dec (or "december" "dec")  (eow) `(-- 12))

            (number (and
                     (or digits
                         direct-nums
                         single-nums
                         direct-ordinals
                         single-ordinals
                         (and ten-prefixes @ single-nums `(a b -- (+ a b)))
                         (and ten-prefixes @ single-ordinals `(a b -- (+ a b)))
                         ten-prefixes)
                     (list (* (and @ (or fractions big-prefixes)))) ; big-prefixes should be big-suffixes
                     `(a b --
                         (progn
                           (while b
                             (setq a (funcall (car b) a))
                             (setq b (cdr b)))
                           a))
                     ))
            (digits (substring (+ [digit])) `(s -- (string-to-number s)))
            (direct-nums (or
                          (and "eleven"    (eow) `(-- 11))
                          (and "twelve"    (eow) `(-- 12))
                          (and "thirteen"  (eow) `(-- 13))
                          (and "fourteen"  (eow) `(-- 14))
                          (and "fifteen"   (eow) `(-- 15))
                          (and "sixteen"   (eow) `(-- 16))
                          (and "seventeen" (eow) `(-- 17))
                          (and "eighteen"  (eow) `(-- 18))
                          (and "nineteen"  (eow) `(-- 19))
                          (and "ninteen"   (eow) `(-- 19)) ; common misspelling
                          (and "zero"      (eow) `(-- 0))
                          (and "ten"       (eow) `(-- 10))
                          (and "a"         (eow)
                               (not (eol))
                               (not (eob))       `(-- 1)))) ; doesn't make sense for an 'a' at the end to be a 1
            (single-nums (or
                          (and "one"       (eow) `(-- 1))
                          (and "two"       (eow) `(-- 2))
                          (and "three"     (eow) `(-- 3))
                          (and "four"      (eow) `(-- 4))
                          (and "five"      (eow) `(-- 5))
                          (and "six"       (eow) `(-- 6))
                          (and "seven"     (eow) `(-- 7))
                          (and "eight"     (eow) `(-- 8))
                          (and "nine"      (eow) `(-- 9))))
            (ten-prefixes (or
                           (and "twenty"   (eow) `(-- 20))
                           (and "thirty"   (eow) `(-- 30))
                           (and "forty"    (eow) `(-- 40))
                           (and "fourty"   (eow) `(-- 40)) ; common misspelling
                           (and "fifty"    (eow) `(-- 50))
                           (and "sixty"    (eow) `(-- 60))
                           (and "seventy"  (eow) `(-- 70))
                           (and "eighty"   (eow) `(-- 80))
                           (and "ninety"   (eow) `(-- 90))))
            (big-prefixes (or
                           (and "hundred"        (eow) `(-- (lambda (n) (* n 100))))
                           (and "thousand"       (eow) `(-- (lambda (n) (* n 1000))))
                           (and "million"        (eow) `(-- (lambda (n) (* n 1000000))))
                           (and "billion"        (eow) `(-- (lambda (n) (* n 1000000000))))
                           (and "trillion"       (eow) `(-- (lambda (n) (* n 1000000000000))))))
            (fractions (or
                        (and "half"              (eow) `(-- (lambda (n) (/ n 2))))
                        (and "third" (opt "s")   (eow) `(-- (lambda (n) (/ n 3))))
                        (and "fourth" (opt "s")  (eow) `(-- (lambda (n) (/ n 4))))
                        (and "quarter" (opt "s") (eow) `(-- (lambda (n) (/ n 4))))
                        (and "fifth" (opt "s")   (eow) `(-- (lambda (n) (/ n 5))))
                        (and "sixth" (opt "s")   (eow) `(-- (lambda (n) (/ n 6))))
                        (and "seventh" (opt "s") (eow) `(-- (lambda (n) (/ n 7))))
                        (and "eighth" (opt "s")  (eow) `(-- (lambda (n) (/ n 8))))
                        (and "nineth" (opt "s")  (eow) `(-- (lambda (n) (/ n 9))))))
            (single-ordinals (or
                              (and "first"       (eow) `(-- 1))
                              (and "third"       (eow) `(-- 3))
                              (and "fourth"      (eow) `(-- 4))
                              (and "fifth"       (eow) `(-- 5))
                              (and "sixth"       (eow) `(-- 6))
                              (and "seventh"     (eow) `(-- 7))
                              (and "eighth"      (eow) `(-- 8))
                              (and "ninth"       (eow) `(-- 9))))
            (direct-ordinals (or
                              (and "tenth"       (eow) `(-- 10))
                              (and "eleventh"    (eow) `(-- 11))
                              (and "twelfth"     (eow) `(-- 12))
                              (and "thirteenth"  (eow) `(-- 13))
                              (and "fourteenth"  (eow) `(-- 14))
                              (and "fifteenth"   (eow) `(-- 15))
                              (and "sixteenth"   (eow) `(-- 16))
                              (and "seventeenth" (eow) `(-- 17))
                              (and "eighteenth"  (eow) `(-- 18))
                              (and "nineteenth"  (eow) `(-- 19))
                              (and "twentieth"   (eow) `(-- 20))
                              (and "thirtieth"   (eow) `(-- 30))
                              (and "fourtieth"   (eow) `(-- 40))
                              (and "fiftieth"    (eow) `(-- 50))
                              (and "sixtieth"    (eow) `(-- 60))
                              (and "seventieth"  (eow) `(-- 70))
                              (and "eightieth"   (eow) `(-- 80))
                              (and "ninetieth"   (eow) `(-- 90))))
            (@ (* [space]) (bow))
            (@. (* [space]) "." (* [space]) (bow))
            (@: (* [space]) ":" (* [space]) (bow))
            (@/ (* [space]) "/" (* [space]) (bow))
            (@- (* [space]) "-" (* [space]) (bow))
            (@@ (* [space]) (or (and "@" (* [space]))
                                (and "at" (+ [space]))) (bow))
            (@@-opt (* [space]) (or (and "@" (* [space]))
                                    (and "at" (+ [space]))
                                    "") (bow))
            (@comma (* [space]) "," (* [space]) (bow))
            (@comma-opt (* [space]) (opt "," (* [space])) (bow))))))
    (unless result (goto-char starting-point))
    ; (when result (message "Result: %S" (current-time-string (apply 'encode-time (car result))))) ; for debugging
    (when result (car result))))

(defun nm-date-search-forward ()
  "Search for a date/time in the current buffer"
  (interactive)
  (let ((starting-point (point))
        (result nil))
    (while
        (and (not (eq (point) (point-max)))
             (not (setq result (ignore-errors (nm-dateparse))))
             (when (not (eq (point) (point-max)))
               (forward-char)
               (re-search-forward "\\<" nil t))))
    (unless result (goto-char starting-point))
    result))

(defun nm-date-search-string (s)
  "Parse as a date."
  (with-temp-buffer
    (insert s)
    (goto-char (point-min))
    (nm-date-search-forward)))

(provide 'nm-dateparse)
