;;; tod.el - Misc Time of Day stuff
;;
;; Functions I use as part of my work time logging.

(defun tod-valid-time-p (hh-mm)
  "Check if time of day expression HH-MM is considered valid."
  (if (and (stringp hh-mm) (string-match "^\\(2[0-4]\\|[01]?[0-9]\\)\\([:.][0-5][0-9]\\)?$" hh-mm)) t
    nil))

(defun tod-diff (earlier later)
  "Calculate how many hours (in decimal) there are between two times in a day.
The two points in time, EARLIER and LATER, must be strings in the
format `%H[:%M]' or `%H[.%M]'. Minutes may be omitted.

The return value is a floating point number which is the time
difference in hours, rounded to two decimal places. If LATER is
actually less than EARLIER within a 24 hour day, then LATER is
assumed to be over 24 hours later. If the time strings are not
formatted correctly, then nil is returned."
  (if (and (tod-valid-time-p earlier) (tod-valid-time-p later))
      (let* ((earlier-nlist (mapcar 'string-to-number (split-string earlier "[.:]")))
             (later-nlist (mapcar 'string-to-number (split-string later "[.:]")))
             (total-minutes-earlier (+ (* 60 (car earlier-nlist)) (or (cadr earlier-nlist) 0)))
             (total-minutes-later (+ (* 60 (car later-nlist)) (or (cadr later-nlist) 0))))
        
        (when (< total-minutes-later total-minutes-earlier)
          ;; Assume that total-minutes-later have rolled past 00:00 and into next day.
          ;; Add 24 hours to it. Later is later.
          (setq total-minutes-later (+ total-minutes-later 1440)))

        (/ (round (* (/ (- total-minutes-later total-minutes-earlier) 60.0) 100)) 100.0))))

(defun tod-get-hhmm (&optional round)
  "Get current time of day as %H:%M. Optionally ROUND to given
number of minutes (1-60 only)."
  (interactive)
  (let* ((curtime (decode-time (current-time)))
         (cursecs (car curtime))
         (curmin (nth 1 curtime))
         (curhour (nth 2 curtime))
         (round (cond ((null round) 1) ((= 60 round) 60) ((<= round 0) 1) ((% round 60))))
         (roundedmin curmin))
    (if (< (+ (% curmin round) (/ cursecs 60.0)) (/ round 2.0))
        (setq roundedmin (- curmin (% curmin round)))
      (setq roundedmin (+ curmin (- round (% curmin round)))))
    (when (>= roundedmin 60)
      (setq curhour (% (+ 1 curhour) 24))
      (setq roundedmin (% roundedmin 60)))
    (format "%d:%02d" curhour roundedmin)))

(defun tod-current-week()
  "Return current week (ISO) as a number."
  (string-to-number (format-time-string "%V")))

(defun tod-parse-ddmm (ddmm &optional year)
  (cond
   ((string-match "^[0-9]\\{1,2\\}\\.[0-9]\\{1,2\\}$" ddmm)
    (let* ((elts (split-string ddmm "\\."))
           (year (or year (nth 5 (decode-time (current-time)))))
           (day (string-to-number (car elts)))
           (month (string-to-number (cadr elts))))
      (encode-time `(0 0 0 ,day ,month ,year nil -1 nil))))
   (t nil)))

(defun tod-week-from-ddmm (ddmm &optional year)
  "Parse day of month formatted as \"dd.mm\" using current year,
return the week number as formatted by `format-time-string' with
argument \"%V\". Optional number argument YEAR can override to
specific year."
  (let ((time (tod-parse-ddmm ddmm year)))
    (if time (format-time-string "%V" time) nil)))

(defun tod-dayname-from-ddmm (ddmm &optional year)
  "Parse day of month formatted as \"dd.mm\" using current year,
return the name of the day as formatted by `format-time-string'
with argument \"%a\". Optional number argument YEAR can override
to specific year."
  (let ((time (tod-parse-ddmm ddmm year)))
    (if time (format-time-string "%a" time) nil)))

(provide 'tod)
