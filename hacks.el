(require 'peg)
(defun date-search-forward (&optional noerror)
  (interactive)
  (let ((starting-point (point))
        (success nil))
    (while (not (or success (eq (point) (point-max))))
      (let ((result
             (ignore-errors
               (peg-parse
;                (main number `(-- 0))
                (date (or
                       ; endian, month/day only, ignoring day/month, see definition.rb
                       (and scalar-month (or @/ @-) scalar-day (or @/ @-) scalar-year (opt @@) (opt time))
                       (and scalar-month (or @/ @-) scalar-day (opt @@) (opt time))
                       (and scalar-day month-name scalar-year (opt @@) (opt time))
                       ; my hacks
                       (and (or last next) @ (or week month-name day-name))
                       (and day-name (opt @ month-name @ scalar-day (opt @comma scalar-year)))
                       (and month-name @ scalar-day (opt @comma scalar-year)))
                      `(-- 0))
                (from "from" (eow))
                (at "at" (eow))
                (hence "hence" (eow))
                (now "now" (eow))
                (before "before" (eow))
                (to "to" (eow))
                (till "till" (eow))
                (ago "ago" (eow))

                (grabber (or last this next))
                (last "last" (eow) `(-- 'last))
                (this "this" (eow) `(-- 'this))
                (next "next" (eow) `(-- 'next))

                (pointer (or past future))
                (past "past" (eow) `(-- 'past))
                (future (or "future" "in") (eow) `(-- 'future))

                (noon "noon" (eow))
                (quarter "quarter" (eow))
                (half "half" (eow))
                (during "during" (eow))
                (oclock "oclock" (eow))
                (after "after" (eow))
                (yesterday "yesterday" (eow))

                ; RepeaterTime
                (time number (opt (and ":" number (opt (and ":" number (opt (and ":" number)))))) (eow))

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
                (am (or "am" "a.m." "a.m") (eow))
                (pm (or "pm" "p.m." "p.m") (eow))
                (morning "morning" (eow))
                (afternoon "afternoon" (eow))
                (evening "evening" (eow))
                (night (or "night" "nite") (eow))

                (season-name (or spring summer autumn winter)) ; RepeaterSeasonName
                (spring "spring"             (eow) `(--  0))
                (summer "summer"             (eow) `(--  1))
                (autumn (or "autumn" "fall") (eow) `(--  2))
                (winter "winter"             (eow) `(--  3))

                (day-name (or mon tue wed thu fri sat sun))
                (mon (or "monday" "mon")    (eow) `(-- 0))
                (tue (or "tuesday" "tue")   (eow) `(-- 1))
                (wed (or "wednesday" "wed") (eow) `(-- 2))
                (thu (or "thursday" "thu")  (eow) `(-- 3))
                (fri (or "friday" "fri")    (eow) `(-- 4))
                (sat (or "saturday" "sat")  (eow) `(-- 5))
                (sun (or "sunday" "sun")    (eow) `(-- 6))

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

                (scalar-day number)   ; chronic enforces range [1,31]

                (scalar-month number) ; chronic enforces range [1,12]

                (scalar-year number)  ; chronic enforces range [1,9999]

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
                (@comma (* [space]) "," (* [space]) (bow))
                ))))
        (if result
            (progn
              (setq success t)
              (message "Got number %S" result))
          (when (not (eq (point) (point-max)))
            (forward-char)
            (re-search-forward "\\<" nil 'end-of-buffer-on-fail)))
        ))
    (unless success
      (goto-char starting-point))
    ))

(defun day()
  (interactive)
  (re-search-forward "\\<\\(monday\\|tuesday\\|wednesday\\|thursday\\|friday\\|saturday\\|sunday\\)\\>"))
(defun skip()
  (interactive)
  (re-search-forward "[^[:word:]]+"))
(defun peg-ex-parse-int ()
  (interactive)
  (message "Result: %S"
           (peg-parse (number sign digit (* digit 
                                            `(a b -- (+ (* a 10) b)))
                              `(sign val -- (* sign val)))
                      (sign (or (and "+" `(-- 1))
                                (and "-" `(-- -1))
                                (and ""  `(-- 1))))
                      (digit [0-9] `(-- (- (char-before) ?0))))))
(defun tt()
  (interactive)
  (peg-parse
   (main (or single-nums direct-nums))
   (direct-nums (or
                 (and "eleven" (eow)                     `(-- 11))
                 (and "twelve" (eow)                     `(-- 12))
                 (and "thirteen" (eow)                   `(-- 13))
                 (and "fourteen" (eow)                   `(-- 14))
                 (and "fifteen" (eow)                    `(-- 15))
                 (and "sixteen" (eow)                    `(-- 16))
                 (and "seventeen" (eow)                  `(-- 17))
                 (and "eighteen" (eow)                   `(-- 18))
                 (and "nineteen" (eow)                   `(-- 19))
                 (and "ninteen" (eow)                    `(-- 19)) ; common misspelling
                 (and "zero" (eow)                       `(-- 0))
                 (and "ten" (eow)                        `(-- 10))
                 (and "a" (eow) (not (eol)) (not (eob)) `(-- 101)))) ; doesn't make sense for an 'a' at the end to be a 1
   (single-nums (or
                 (and "one" (eow)   `(-- 1000))
                 (and "two" (eow)   `(-- 2))
                 (and "three" (eow) `(-- 3))
                 (and "four" (eow)  `(-- 4))
                 (and "five" (eow)  `(-- 5))
                 (and "six" (eow)   `(-- 6))
                 (and "seven" (eow) `(-- 7))
                 (and "eight" (eow) `(-- 8))
                 (and "nine" (eow)  `(-- 9))))))

