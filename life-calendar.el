;;; life-calendar.el --- Display your life in weeks  -*- lexical-binding: t; -*-

;; Copyright (c) 2025 Vadzim Shender

;; Author: Vadzim Shender <https://github.com/vshender>
;; URL: https://github.com/vshender/emacs-life-calendar
;; Version: 0.2.0
;; Keywords: calendar, visualization
;; Package-Requires: ((emacs "27.1"))
;; SPDX-License-Identifier: ISC

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This package displays a "life in weeks" calendar --- a grid visualization
;; showing all weeks of a person's life, with past weeks, the current week,
;; and future weeks visually distinguished.
;;
;; Usage:
;;   M-x life-calendar
;;
;; On first use, you will be prompted to enter your birthday.
;; The birthday is saved and remembered for future sessions.
;;
;; Navigation:
;;   f/b, C-f/C-b, arrows  -- move by week
;;   n/p, C-n/C-p, up/down -- move by row (multiple years in multi-column view)
;;   ]/[, M-f/M-b          -- move by year
;;   M-<, M->              -- first/last week
;;   C-a, C-e, Home/End    -- beginning/end of row
;;   .                     -- jump to current week
;;
;; Life chapters (marking significant dates):
;;   +                     -- add a life chapter
;;   -                     -- remove a life chapter from current week
;;
;; Other:
;;   g                     -- refresh
;;   ?                     -- help (show keybindings)
;;   q                     -- quit
;;
;; Customization:
;;   M-x customize-group RET life-calendar RET

;;; Code:

(require 'calendar)
(require 'seq)

;;; Customization

(defgroup life-calendar nil
  "Display your life in weeks."
  :group 'applications
  :prefix "life-calendar-")

(defcustom life-calendar-birthday nil
  "Your birthday as a string in YYYY-MM-DD format.
If nil, you will be prompted to enter it on first use."
  :type '(choice (const :tag "Not set" nil)
                 (string :tag "Birthday (YYYY-MM-DD)"))
  :group 'life-calendar)

(defcustom life-calendar-years 90
  "Number of years to display in the life calendar."
  :type 'integer
  :group 'life-calendar)

(defcustom life-calendar-past-char "▪"
  "Character to display for past weeks."
  :type 'string
  :group 'life-calendar)

(defcustom life-calendar-current-char "▪"
  "Character to display for the current week."
  :type 'string
  :group 'life-calendar)

(defcustom life-calendar-future-char "▫"
  "Character to display for future weeks."
  :type 'string
  :group 'life-calendar)

(defcustom life-calendar-chapter-char "★"
  "Character to display for weeks with life chapters."
  :type 'string
  :group 'life-calendar)

(defcustom life-calendar-week-start-day 'birthday
  "Day of week when a new week of life is counted.
If `birthday', use the day of week you were born (default).
Otherwise, use a day symbol: `sunday', `monday', etc."
  :type '(choice (const :tag "Day of week of birthday" birthday)
                 (const :tag "Sunday" sunday)
                 (const :tag "Monday" monday)
                 (const :tag "Tuesday" tuesday)
                 (const :tag "Wednesday" wednesday)
                 (const :tag "Thursday" thursday)
                 (const :tag "Friday" friday)
                 (const :tag "Saturday" saturday))
  :group 'life-calendar)

(defcustom life-calendar-columns nil
  "Number of columns to display in the life calendar.
If nil, automatically display as many columns as fit in the window.
If a positive integer, display exactly that many columns."
  :type '(choice (const :tag "Auto (fit to window)" nil)
                 (integer :tag "Fixed number of columns"))
  :group 'life-calendar)

(defcustom life-calendar-chapters nil
  "List of life chapters marking significant dates.
Each chapter is a cons cell (DATE . DESCRIPTION) where DATE is a
string in YYYY-MM-DD format and DESCRIPTION is a string describing
the life event or milestone."
  :type '(alist :key-type (string :tag "Date (YYYY-MM-DD)")
                :value-type (string :tag "Description"))
  :group 'life-calendar)

(defcustom life-calendar-data-file nil
  "File to store life calendar data (birthday and chapters).
If nil, data will be saved using `customize-save-variable' to the
`custom-file'.  If set to a file path, data will be saved to that
file instead.

Example: \"~/.emacs.d/life-calendar-data.el\""
  :type '(choice (const :tag "Use custom-file (default)" nil)
                 (file :tag "Custom data file path"))
  :group 'life-calendar)

;;; Faces

(defface life-calendar-past-face
  '((t :inherit shadow))
  "Face for past weeks in the life calendar."
  :group 'life-calendar)

(defface life-calendar-current-face
  '((t :inherit warning))
  "Face for the current week in the life calendar."
  :group 'life-calendar)

(defface life-calendar-future-face
  '((t :inherit default))
  "Face for future weeks in the life calendar."
  :group 'life-calendar)

(defface life-calendar-age-face
  '((t :inherit font-lock-comment-face))
  "Face for age labels in the life calendar."
  :group 'life-calendar)

(defface life-calendar-header-face
  '((t :inherit bold :height 1.2))
  "Face for the header in the life calendar."
  :group 'life-calendar)

(defface life-calendar-chapter-face
  '((t :inherit success))
  "Face for weeks with life chapters in the life calendar."
  :group 'life-calendar)

;;; Data persistence

(defun life-calendar--load-data-file ()
  "Load data from `life-calendar-data-file' if it exists."
  (when (and life-calendar-data-file
             (file-exists-p life-calendar-data-file))
    (load life-calendar-data-file t t)))

(defun life-calendar--save-to-file (variable value)
  "Save VARIABLE with VALUE to `life-calendar-data-file'.
If `life-calendar-data-file' is nil, falls back to `customize-save-variable'."
  (if life-calendar-data-file
      (progn
        ;; Create directory if it doesn't exist
        (let ((dir (file-name-directory (expand-file-name life-calendar-data-file))))
          (when (and dir (not (string-empty-p dir)))
            (make-directory dir t)))
        ;; Read existing data or start fresh
        (let ((data nil))
          ;; Load existing file to get current data
          (when (file-exists-p life-calendar-data-file)
            (with-temp-buffer
              (insert-file-contents life-calendar-data-file)
              (goto-char (point-min))
              ;; Parse all setq forms
              (condition-case nil
                  (while (not (eobp))
                    (let ((form (read (current-buffer))))
                      (when (and (listp form) (eq (car form) 'setq))
                        ;; Handle setq with multiple var-value pairs
                        (let ((args (cdr form)))
                          (while (>= (length args) 2)
                            (let* ((var (car args))
                                   (val-form (cadr args))
                                   ;; Evaluate the value form to handle quoted values
                                   (val (eval val-form t)))
                              (setq data (cons (list var val) data))
                              (setq args (cddr args))))))))
                (end-of-file nil))))
          ;; Update or add the variable
          (if (assq variable data)
              (setcdr (assq variable data) (list value))
            (setq data (cons (list variable value) data)))
          ;; Write back to file
          (with-temp-file life-calendar-data-file
            (insert ";;; Life Calendar Data File\n")
            (insert ";; This file is automatically generated by life-calendar.el\n")
            (insert ";; Do not edit manually unless you know what you are doing.\n\n")
            (dolist (entry (reverse data))  ; Reverse to maintain order
              (let ((var (car entry))
                    (val (cadr entry)))
                ;; Quote the value if it's a list, otherwise use as-is
                (if (listp val)
                    (insert (format "(setq %s '%S)\n" var val))
                  (insert (format "(setq %s %S)\n" var val)))))
            (insert "\n;;; End of life-calendar data\n"))))
    ;; Fall back to customize-save-variable
    (customize-save-variable variable value)))

;;; Layout

(defconst life-calendar--age-label-width 5
  "Width of the age label (e.g., \"  0: \").")

(defconst life-calendar--weeks-width 53
  "Maximum number of weeks displayed per year.")

(defconst life-calendar--column-width
  (+ life-calendar--age-label-width life-calendar--weeks-width)
  "Total width of a single year column.")

(defconst life-calendar--column-gap 2
  "Gap between columns in multi-column display.")

(defvar-local life-calendar--num-columns nil
  "Number of columns used in the current calendar display.
This is set during rendering and used for navigation.")

(defun life-calendar--calculate-columns (window-width)
  "Calculate the number of columns that fit in WINDOW-WIDTH.
If `life-calendar-columns' is set to a positive integer, return that value.
Otherwise, calculate based on window width."
  (if (and life-calendar-columns (> life-calendar-columns 0))
      life-calendar-columns
    ;; Calculate how many columns fit: first column needs column-width,
    ;; each additional column needs column-width + gap.
    (max 1
         (1+ (/ (- window-width life-calendar--column-width)
                (+ life-calendar--column-width life-calendar--column-gap))))))

;;; Date Utilities

;; Dates are represented as Emacs time values with hours, minutes, and seconds
;; set to zero.  UTC is used to avoid daylight saving time issues with date
;; arithmetic.

(defun life-calendar--encode-date (year month day)
  "Encode YEAR, MONTH, DAY as a time value in UTC."
  (encode-time (list 0 0 0 day month year nil nil t)))

(defun life-calendar--decode-date (time)
  "Decode TIME to a decoded time structure in UTC."
  (decode-time time t))

(defun life-calendar--current-date ()
  "Return today's date as a time value."
  (let ((decoded (life-calendar--decode-date (current-time))))
    (life-calendar--encode-date (decoded-time-year decoded)
                                (decoded-time-month decoded)
                                (decoded-time-day decoded))))

(defun life-calendar--valid-date-string-p (date-string)
  "Return non-nil if DATE-STRING is a valid date in YYYY-MM-DD format."
  (and (stringp date-string)
       (string-match "\\`\\([0-9]\\{4\\}\\)-\\([0-9]\\{2\\}\\)-\\([0-9]\\{2\\}\\)\\'" date-string)
       (let ((year (string-to-number (match-string 1 date-string)))
             (month (string-to-number (match-string 2 date-string)))
             (day (string-to-number (match-string 3 date-string))))
         (calendar-date-is-valid-p (list month day year)))))

(defun life-calendar--read-date-string (prompt)
  "Read a date from the user with PROMPT, returning YYYY-MM-DD string.
Validates the date format and signals an error if invalid or empty."
  (let ((input (read-string (concat prompt " (YYYY-MM-DD): "))))
    (cond
     ((string-empty-p input)
      (user-error "Date is required"))
     ((life-calendar--valid-date-string-p input)
      input)
     (t
      (user-error "Invalid date: %s" input)))))

(defun life-calendar--parse-date (date-string)
  "Parse DATE-STRING in YYYY-MM-DD format to a time value."
  (pcase-let ((`(,year ,month ,day)
               (mapcar #'string-to-number (split-string date-string "-"))))
    (life-calendar--encode-date year month day)))

(defun life-calendar--day-of-week (time)
  "Return day of week for TIME (0=Sunday, 1=Monday, ..., 6=Saturday)."
  (decoded-time-weekday (life-calendar--decode-date time)))

(defun life-calendar--dow-symbol-to-number (dow)
  "Convert DOW symbol to number (0=Sunday, ..., 6=Saturday)."
  (pcase dow
    ('sunday 0)
    ('monday 1)
    ('tuesday 2)
    ('wednesday 3)
    ('thursday 4)
    ('friday 5)
    ('saturday 6)
    (_ (error "Invalid day of week: %s" dow))))

(defun life-calendar--add-days (time days)
  "Add DAYS to TIME and return the new time."
  (time-add time (seconds-to-time (* days 24 60 60))))

;;; Age and Week Calculation

(defun life-calendar--nth-birthday (birth-time n)
  "Return the date of the Nth birthday given BIRTH-TIME.
N=0 returns the birth date itself."
  (let ((decoded (life-calendar--decode-date birth-time)))
    (life-calendar--encode-date (+ (decoded-time-year decoded) n)
                                (decoded-time-month decoded)
                                (decoded-time-day decoded))))

(defun life-calendar--effective-week-start-dow (birth-time)
  "Return the effective day of week for counting weeks as a number.
If `life-calendar-week-start-day' is `birthday', return the day of week
of BIRTH-TIME.  Otherwise convert the configured day symbol to a number."
  (if (eq life-calendar-week-start-day 'birthday)
      (life-calendar--day-of-week birth-time)
    (life-calendar--dow-symbol-to-number life-calendar-week-start-day)))

(defun life-calendar--last-week-start-on-or-before (time dow)
  "Return the last occurrence of day-of-week DOW on or before TIME."
  (let* ((time-dow (life-calendar--day-of-week time))
         (days-back (mod (- time-dow dow) 7)))
    (life-calendar--add-days time (- days-back))))

(defun life-calendar--count-completed-weeks (start-week-dow from-time before-time)
  "Count weeks that complete in the interval [FROM-TIME, BEFORE-TIME).
START-WEEK-DOW is the day of week (0=Sunday, ..., 6=Saturday) when weeks start.
A week is counted if its completion day (last day) falls within the interval."
  (let* (;; Find the completion day (last day) of the week containing
         ;; FROM-TIME.  Since a week that completes before FROM-TIME
         ;; doesn't belong to the interval, this is the first `week-completion'
         ;; to consider.
         (first-week-start (life-calendar--last-week-start-on-or-before
                            from-time start-week-dow))
         (week-completion (life-calendar--add-days first-week-start 6))
         (weeks 0))
    (while (time-less-p week-completion before-time)
      (setq weeks (1+ weeks))
      (setq week-completion (life-calendar--add-days week-completion 7)))
    weeks))

(defun life-calendar--count-weeks-in-year (birth-time year-num)
  "Count weeks in YEAR-NUM of life (0-indexed) for BIRTH-TIME.
Returns the number of weeks that complete within the year.  A week
is counted if it completes in the interval [birthday-N, birthday-N+1).
This is typically 52, occasionally 53."
  (life-calendar--count-completed-weeks
   (life-calendar--effective-week-start-dow birth-time)
   (life-calendar--nth-birthday birth-time year-num)
   (life-calendar--nth-birthday birth-time (1+ year-num))))

(defun life-calendar--time-to-year-week (birth-time time)
  "Convert TIME to (YEAR . WEEK) relative to BIRTH-TIME.
Returns the number of complete years and weeks since birth.
Returns nil if TIME is before BIRTH-TIME."
  ;; Calculate years by decoding the time difference.
  ;; Subtracting the epoch year converts the decoded year to a duration.
  (let ((years (- (decoded-time-year
                   (life-calendar--decode-date (time-subtract time birth-time)))
                  (decoded-time-year
                   (life-calendar--decode-date 0)))))
    (when (>= years 0)
      (let* ((year-start (life-calendar--nth-birthday birth-time years))
             (weeks (life-calendar--count-completed-weeks
                     (life-calendar--effective-week-start-dow birth-time)
                     year-start
                     time)))
        (cons years weeks)))))

(defun life-calendar--date-string-to-year-week (birth-time date-string)
  "Convert DATE-STRING to (YEAR . WEEK) relative to BIRTH-TIME.
Returns nil if the date is before birth."
  (life-calendar--time-to-year-week
   birth-time (life-calendar--parse-date date-string)))

(defun life-calendar--current-age (birth-time)
  "Return (YEARS-LIVED . WEEKS-LIVED) for today given BIRTH-TIME.
YEARS-LIVED is the number of complete years, WEEKS-LIVED is the number
of complete weeks within the current year.  Uses the configured week
start day for accurate calculation."
  (life-calendar--time-to-year-week birth-time (life-calendar--current-date)))

;;; Life Chapters

(defun life-calendar--build-chapters-index (birth-time)
  "Build a hash table mapping (YEAR . WEEK) to list of chapter descriptions.
Uses BIRTH-TIME to convert chapter dates to year/week coordinates.
Invalid chapters are skipped with a warning."
  (let ((index (make-hash-table :test 'equal)))
    (dolist (chapter life-calendar-chapters)
      (let* ((date (car chapter))
             (description (cdr chapter))
             (invalid-reason
              (cond
               ((not (life-calendar--valid-date-string-p date))
                (format "invalid date format: %s" date))
               ((not (stringp description))
                (format "description is not a string for date %s" date))
               ((string-empty-p description)
                (format "empty description for date %s" date)))))
        (if invalid-reason
            (display-warning 'life-calendar
                             (format "Skipping invalid chapter: %s" invalid-reason)
                             :warning)
          (let ((year-week (life-calendar--date-string-to-year-week birth-time date)))
            (if (null year-week)
                (display-warning 'life-calendar
                                 (format "Skipping chapter before birthday: %s" date)
                                 :warning)
              (push (format "%s: %s" date description)
                    (gethash year-week index)))))))
    index))

(defvar-local life-calendar--chapters-index nil
  "Hash table mapping (YEAR . WEEK) to list of chapter descriptions.
Built during rendering for efficient lookup.")

(defun life-calendar--chapters-for-week (year week)
  "Return list of chapter descriptions for YEAR and WEEK.
Returns nil if no chapters exist for this week."
  (when life-calendar--chapters-index
    (gethash (cons year week) life-calendar--chapters-index)))

(defun life-calendar--show-chapters-at-point ()
  "Display chapter descriptions for the week at point in the echo area."
  (let ((chapters (get-text-property (point) 'life-calendar-chapters)))
    (when chapters
      (message "%s"
               (mapconcat #'identity chapters "\n")))))

;;; Rendering

(defun life-calendar--render-single-week-header ()
  "Render a single week number header (without newline).
Shows week numbers at positions 1, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50."
  (let ((header (make-string life-calendar--weeks-width ?\s))
        (week-marks '(1 5 10 15 20 25 30 35 40 45 50)))
    (dolist (week week-marks)
      (let* ((pos (1- week))
             (num-str (number-to-string week))
             (num-len (length num-str)))
        (dotimes (i num-len)
          (aset header (+ pos i) (aref num-str i)))))
    (concat (make-string life-calendar--age-label-width ?\s)
            (propertize header 'face 'life-calendar-age-face))))

(defun life-calendar--render-week-header (num-columns)
  "Render the week number header line for NUM-COLUMNS columns."
  (let ((single-header (life-calendar--render-single-week-header))
        (gap (make-string life-calendar--column-gap ?\s))
        (parts '()))
    (push single-header parts)
    (dotimes (_ (1- num-columns))
      (push gap parts)
      (push single-header parts))
    (concat (apply #'concat (nreverse parts)) "\n")))

(defun life-calendar--render-year-row (birth-time year years-lived weeks-lived)
  "Render a single year row for YEAR given BIRTH-TIME.
YEARS-LIVED and WEEKS-LIVED are used to determine past/current/future status.
Returns a string of exactly `life-calendar--column-width' characters."
  (let* ((weeks-in-year (life-calendar--count-weeks-in-year birth-time year))
         (parts '()))
    ;; Age label
    (push (propertize (format "%3d: " year) 'face 'life-calendar-age-face)
          parts)
    ;; Weeks
    (dotimes (week weeks-in-year)
      (let* ((is-current-year (= year years-lived))
             (is-past-year (< year years-lived))
             (is-current-week (and is-current-year (= week weeks-lived)))
             (is-past-week (or is-past-year
                               (and is-current-year (< week weeks-lived))))
             (chapters (life-calendar--chapters-for-week year week))
             ;; Face: current-week takes priority so today is always highlighted.
             (face (cond
                    (is-current-week 'life-calendar-current-face)
                    (chapters 'life-calendar-chapter-face)
                    (is-past-week 'life-calendar-past-face)
                    (t 'life-calendar-future-face)))
             ;; Character: chapter takes priority so chapter marker is visible
             ;; even on the current week.
             (char-str (cond
                        (chapters life-calendar-chapter-char)
                        (is-current-week life-calendar-current-char)
                        (is-past-week life-calendar-past-char)
                        (t life-calendar-future-char)))
             (char (propertize char-str
                               'face face
                               'life-calendar-week-p t
                               'life-calendar-year year
                               'life-calendar-week week
                               'life-calendar-chapters chapters)))
        (push char parts)))
    ;; Pad to full column width if needed (some years have 52 weeks)
    (let* ((content (apply #'concat (nreverse parts)))
           (content-len (length content))
           (padding-needed (- life-calendar--column-width content-len)))
      (if (> padding-needed 0)
          (concat content (make-string padding-needed ?\s))
        content))))

(defun life-calendar--render-calendar (birth-time years)
  "Render the life calendar for BIRTH-TIME with YEARS total years.
Returns a string containing the rendered calendar.
Also sets `life-calendar--num-columns' and `life-calendar--chapters-index'
as side effects."
  (let* ((age (life-calendar--current-age birth-time))
         (years-lived (car age))
         (weeks-lived (cdr age))
         (years (max years (1+ years-lived)))
         (num-columns (life-calendar--calculate-columns (window-width)))
         (gap (make-string life-calendar--column-gap ?\s))
         (parts '()))
    (setq life-calendar--num-columns num-columns)
    (setq life-calendar--chapters-index
          (life-calendar--build-chapters-index birth-time))
    ;; Header
    (push (propertize
           (format "Life Calendar - %d years, %d weeks old\n\n"
                   years-lived weeks-lived)
           'face 'life-calendar-header-face)
          parts)
    ;; Week numbers header
    (push (life-calendar--render-week-header num-columns) parts)
    ;; Calendar grid - render in groups of num-columns
    (let ((year 0))
      (while (< year years)
        (push (life-calendar--render-year-row
               birth-time year years-lived weeks-lived)
              parts)
        (push (if (or (= year (1- years))
                      (= (mod year num-columns) (1- num-columns)))
                  "\n"
                gap)
              parts)
        (setq year (1+ year))))
    (apply #'concat (nreverse parts))))

;;; Birthday Management

(defun life-calendar--ensure-birthday ()
  "Ensure `life-calendar-birthday' is set to a valid date.
If the saved birthday is invalid, warn and prompt for a new one.
If not set, prompt the user to enter their birthday.
Loops until a valid date is entered; user can abort with \\[keyboard-quit]."
  ;; Validate saved birthday if present.
  (when life-calendar-birthday
    (let ((invalid-reason
           (cond
            ((not (life-calendar--valid-date-string-p life-calendar-birthday))
             (format "invalid date format: %s" life-calendar-birthday))
            ((time-less-p (life-calendar--current-date)
                          (life-calendar--parse-date life-calendar-birthday))
             (format "birthday is in the future: %s" life-calendar-birthday)))))
      (when invalid-reason
        (display-warning 'life-calendar
                         (format "Saved birthday is invalid: %s" invalid-reason)
                         :warning)
        (setq life-calendar-birthday nil))))
  ;; Prompt for birthday if not set, loop until valid.
  (let ((prompt "Enter your birthday"))
    (while (null life-calendar-birthday)
      (condition-case err
          (let ((date (life-calendar--read-date-string prompt)))
            (cond
             ((time-less-p (life-calendar--current-date)
                           (life-calendar--parse-date date))
              (setq prompt
                    "Birthday cannot be in the future.  Enter your birthday"))
             (t
              (life-calendar--save-to-file 'life-calendar-birthday date)
              (message "Birthday saved: %s" date))))
        (user-error
         (setq prompt
               (format "%s.  Enter your birthday" (error-message-string err)))))))
  life-calendar-birthday)

;;; Navigation

(defun life-calendar--on-week-p (pos)
  "Return non-nil if POS is on a week square."
  (when (<= (point-min) pos (point-max))
    (get-text-property pos 'life-calendar-week-p)))

(defun life-calendar--week-at-point (pos)
  "Return (YEAR . WEEK-NUM) at POS, or nil if not on a week."
  (when (life-calendar--on-week-p pos)
    (cons (get-text-property pos 'life-calendar-year)
          (get-text-property pos 'life-calendar-week))))

(defun life-calendar--find-next-week (pos)
  "Find position of next week square after POS.
Returns the position or nil if not found."
  (let ((next (1+ pos))
        (pmax (point-max)))
    (while (and (<= next pmax)
                (not (life-calendar--on-week-p next)))
      (setq next (next-single-property-change
                  next 'life-calendar-week-p nil (1+ pmax))))
    (when (and (<= next pmax)
               (life-calendar--on-week-p next))
      next)))

(defun life-calendar--find-previous-week (pos)
  "Find position of previous week square before POS.
Returns the position or nil if not found."
  (let ((prev (1- pos))
        (pmin (point-min)))
    (while (and (<= pmin prev)
                (not (life-calendar--on-week-p prev)))
      ;; Pass (1+ prev) because `previous-single-property-change' starts
      ;; examining from position-1 due to Emacs cursor model asymmetry.
      ;; Wrap with (1- ...) because the function returns the boundary
      ;; position, and we want the character before it (the actual week).
      (setq prev (1- (previous-single-property-change
                      (1+ prev) 'life-calendar-week-p nil pmin))))
    (when (and (<= pmin prev)
               (life-calendar--on-week-p prev))
      prev)))

(defun life-calendar--find-week (target-year target-week)
  "Find buffer position of week TARGET-WEEK in year TARGET-YEAR.
Returns position or nil if not found."
  (let ((pos (point-min))
        (found nil))
    (while (and (not found)
                (setq pos (life-calendar--find-next-week pos)))
      (let ((info (life-calendar--week-at-point pos)))
        (when (and (= (car info) target-year)
                   (= (cdr info) target-week))
          (setq found pos))))
    found))

(defun life-calendar--move-years (delta &optional error-message)
  "Move point to the same week DELTA years forward/backward.
DELTA can be positive (forward) or negative (backward).
ERROR-MESSAGE is shown when target year doesn't exist."
  (let ((info (life-calendar--week-at-point (point))))
    (if (not info)
        ;; Not on a week, find nearest week.
        (if (> delta 0)
            (life-calendar-next-week)
          (life-calendar-previous-week))
      (let* ((cur-year (car info))
             (cur-week (cdr info))
             (target-year (+ cur-year delta)))
        (if (< target-year 0)
            (message (or error-message "Year not found"))
          ;; Try exact week first, then week-1 for 53/52 weeks difference.
          (let ((pos (or (life-calendar--find-week target-year cur-week)
                         (life-calendar--find-week target-year (1- cur-week)))))
            (if pos
                (goto-char pos)
              (message (or error-message "Year not found")))))))))

;;; Commands

(defun life-calendar-not-implemented ()
  "Signal error for commands not available in `life-calendar-mode'."
  (interactive)
  (error "%s not available in life-calendar"
         (global-key-binding (this-command-keys))))

(defun life-calendar-refresh (&optional target)
  "Refresh the life calendar display.
TARGET controls where point moves after refresh:
  nil         -- move to current week (default)
  t           -- preserve current position
  DATE-STRING -- move to the week containing this date (YYYY-MM-DD format)"
  (interactive)
  (when (derived-mode-p 'life-calendar-mode)
    (let* ((birthday (life-calendar--ensure-birthday))
           (birth-time (life-calendar--parse-date birthday))
           (age (life-calendar--current-age birth-time))
           (saved-week (when (eq target t)
                         (life-calendar--week-at-point (point))))
           (target-week (cond
                         (saved-week saved-week)
                         ((stringp target)
                          (life-calendar--date-string-to-year-week
                           birth-time target))
                         (t nil)))
           (inhibit-read-only t))
      (erase-buffer)
      (insert (life-calendar--render-calendar birth-time life-calendar-years))
      ;; Navigate to target week or current week.
      (let ((pos (if target-week
                     (life-calendar--find-week (car target-week)
                                               (cdr target-week))
                   (life-calendar--find-week (car age) (cdr age)))))
        (goto-char (or pos
                       (life-calendar--find-next-week (point-min))))))))

(defun life-calendar-first-week ()
  "Move point to the first week of life (year 0, week 0)."
  (interactive)
  (let ((pos (life-calendar--find-next-week (point-min))))
    (if pos
        (goto-char pos)
      (message "No weeks found"))))

(defun life-calendar-last-week ()
  "Move point to the last week in the calendar."
  (interactive)
  (let ((pos (life-calendar--find-previous-week (point-max))))
    (if pos
        (goto-char pos)
      (message "No weeks found"))))

(defun life-calendar-beginning-of-row ()
  "Move point to the first week in the current visual row."
  (interactive)
  (let ((pos (save-excursion
               (beginning-of-line)
               (life-calendar--find-next-week (point)))))
    (if pos
        (goto-char pos)
      (message "No week in row"))))

(defun life-calendar-end-of-row ()
  "Move point to the last week in the current visual row."
  (interactive)
  (let ((pos (save-excursion
               (end-of-line)
               (life-calendar--find-previous-week (point)))))
    (if pos
        (goto-char pos)
      (message "No week in row"))))

(defun life-calendar-next-week ()
  "Move point to the next week square (one square to the right)."
  (interactive)
  (if-let ((pos (life-calendar--find-next-week (point))))
      (goto-char pos)
    (message "No next week")))

(defun life-calendar-previous-week ()
  "Move point to the previous week square (one square to the left)."
  (interactive)
  (if-let ((pos (life-calendar--find-previous-week (point))))
      (goto-char pos)
    (message "No previous week")))

(defun life-calendar-next-year ()
  "Move point to the same week in the next year."
  (interactive)
  (life-calendar--move-years 1 "No next year"))

(defun life-calendar-previous-year ()
  "Move point to the same week in the previous year."
  (interactive)
  (life-calendar--move-years -1 "No previous year"))

(defun life-calendar-next-row ()
  "Move point to the same week in the next visual row.
With multi-column display, this moves down by the number of columns."
  (interactive)
  (life-calendar--move-years life-calendar--num-columns "No next row"))

(defun life-calendar-previous-row ()
  "Move point to the same week in the previous visual row.
With multi-column display, this moves up by the number of columns."
  (interactive)
  (life-calendar--move-years (- life-calendar--num-columns) "No previous row"))

(defun life-calendar-goto-current-week ()
  "Move point to the current week (today)."
  (interactive)
  (let* ((birthday (life-calendar--ensure-birthday))
         (birth-time (life-calendar--parse-date birthday))
         (age (life-calendar--current-age birth-time))
         (pos (life-calendar--find-week (car age) (cdr age))))
    (if pos
        (goto-char pos)
      (message "Current week not found in calendar"))))

(defun life-calendar-add-chapter ()
  "Add a new life chapter.
Prompts for a date and description, saves to `life-calendar-chapters',
refreshes the display, and moves point to the new chapter's week.
Loops until valid date and description are entered; user can abort
with \\[keyboard-quit]."
  (interactive)
  (let* ((birthday (life-calendar--ensure-birthday))
         (birth-time (life-calendar--parse-date birthday))
         (prompt "Chapter date")
         (date nil))
    ;; Loop until valid date is entered.
    (while (null date)
      (condition-case err
          (let ((input (life-calendar--read-date-string prompt)))
            (if (time-less-p (life-calendar--parse-date input) birth-time)
                (setq prompt
                      (format "Chapter date cannot be before birthday (%s).  Chapter date"
                              birthday))
              (setq date input)))
        (user-error
         (setq prompt
               (format "%s.  Chapter date" (error-message-string err))))))
    ;; Loop until valid description is entered.
    (let ((desc-prompt "Chapter description")
          (description nil))
      (while (null description)
        (let ((input (read-string (concat desc-prompt ": "))))
          (if (string-empty-p input)
              (setq desc-prompt "Description is required.  Chapter description")
            (setq description input))))
      (life-calendar--save-to-file
       'life-calendar-chapters
       (cons (cons date description) life-calendar-chapters))
      (life-calendar-refresh date)
      (message "Life chapter added: %s" description))))

(defun life-calendar-remove-chapter ()
  "Remove a life chapter from the current week.
If the week has multiple chapters, prompts to select which one to remove."
  (interactive)
  (let ((chapters (get-text-property (point) 'life-calendar-chapters)))
    (if (not chapters)
        (message "No life chapters at this week")
      (let* ((to-remove (if (= (length chapters) 1)
                            (car chapters)
                          (completing-read "Remove chapter: " chapters nil t)))
             (year (get-text-property (point) 'life-calendar-year))
             (week (get-text-property (point) 'life-calendar-week))
             (birthday (life-calendar--ensure-birthday))
             (birth-time (life-calendar--parse-date birthday)))
        (when (yes-or-no-p (format "Remove chapter \"%s\"? " to-remove))
          (life-calendar--save-to-file
           'life-calendar-chapters
           (seq-remove
            (lambda (chapter)
              (let ((chapter-year-week
                     (life-calendar--date-string-to-year-week
                      birth-time (car chapter))))
                (and chapter-year-week
                     (= (car chapter-year-week) year)
                     (= (cdr chapter-year-week) week)
                     (string= (format "%s: %s" (car chapter) (cdr chapter))
                              to-remove))))
            life-calendar-chapters))
          (life-calendar-refresh t)
          (message "Life chapter removed: %s" to-remove))))))

(defun life-calendar-mouse-set-point (event)
  "Move point to week square at mouse click EVENT.
If the click is not on a week square, beep."
  (interactive "e")
  (let* ((end (event-end event))
         (win (posn-window end))
         (pos (posn-point end)))
    (when (and (windowp win)
               (integer-or-marker-p pos))
      (select-window win)
      (if (life-calendar--on-week-p pos)
          (goto-char pos)
        (ding)))))

;;; Major Mode

(defvar life-calendar-mode-map
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map)
    (dolist (c '(narrow-to-defun narrow-to-page narrow-to-region
                 set-mark-command mark-defun mark-page mark-paragraph
                 mark-sexp mark-whole-buffer mark-word
                 downcase-region upcase-region
                 kill-line kill-region copy-region-as-kill
                 kill-sentence kill-word
                 capitalize-region write-region
                 scroll-down-command scroll-up-command
                 backward-paragraph forward-paragraph
                 move-to-window-line-top-bottom
                 tab-to-tab-stop back-to-indentation))
      (define-key map `[remap ,c] 'life-calendar-not-implemented))
    ;; Basic commands
    (define-key map (kbd "q") #'quit-window)
    (define-key map (kbd "g") #'life-calendar-refresh)
    ;; Week navigation (remap character movement)
    (define-key map (kbd "f") #'life-calendar-next-week)
    (define-key map (kbd "b") #'life-calendar-previous-week)
    (define-key map [remap forward-char] #'life-calendar-next-week)
    (define-key map [remap backward-char] #'life-calendar-previous-week)
    (define-key map [remap right-char] #'life-calendar-next-week)
    (define-key map [remap left-char] #'life-calendar-previous-week)
    ;; Row navigation (remap line movement)
    (define-key map (kbd "n") #'life-calendar-next-row)
    (define-key map (kbd "p") #'life-calendar-previous-row)
    (define-key map [remap next-line] #'life-calendar-next-row)
    (define-key map [remap previous-line] #'life-calendar-previous-row)
    ;; Year navigation
    (define-key map (kbd "]") #'life-calendar-next-year)
    (define-key map (kbd "[") #'life-calendar-previous-year)
    (define-key map [remap right-word] #'life-calendar-next-year)
    (define-key map [remap left-word] #'life-calendar-previous-year)
    (define-key map [remap forward-word] #'life-calendar-next-year)
    (define-key map [remap backward-word] #'life-calendar-previous-year)
    ;; First/last week of calendar (remap buffer beginning/end)
    (define-key map [remap beginning-of-buffer] #'life-calendar-first-week)
    (define-key map [remap end-of-buffer] #'life-calendar-last-week)
    ;; Beginning/end of row (remap line beginning/end)
    (define-key map [remap move-beginning-of-line] #'life-calendar-beginning-of-row)
    (define-key map [remap move-end-of-line] #'life-calendar-end-of-row)
    ;; Jump to current week
    (define-key map (kbd ".") #'life-calendar-goto-current-week)
    ;; Life chapters
    (define-key map (kbd "+") #'life-calendar-add-chapter)
    (define-key map (kbd "-") #'life-calendar-remove-chapter)
    ;; Mouse navigation
    (define-key map [down-mouse-1] #'ignore)
    (define-key map [drag-mouse-1] #'life-calendar-mouse-set-point)
    (define-key map [mouse-1] #'life-calendar-mouse-set-point)
    map)
  "Keymap for `life-calendar-mode'.")

(define-derived-mode life-calendar-mode special-mode "Life Calendar"
  "Major mode for viewing the life calendar.

\\{life-calendar-mode-map}"
  (setq buffer-read-only t)
  (setq truncate-lines t)
  (add-hook 'post-command-hook #'life-calendar--show-chapters-at-point nil t))

;;;###autoload
(defun life-calendar ()
  "Display your life in weeks.
Shows a grid where each cell represents one week of your life.
Past and current weeks are shown with a small filled square,
future weeks with a small empty square.  The current week is
highlighted with a distinct face.

On first use, you will be prompted to enter your birthday.
Press \\<life-calendar-mode-map>\\[quit-window] to close the calendar,
\\<life-calendar-mode-map>\\[life-calendar-refresh] to refresh."
  (interactive)
  ;; Load data from custom file if specified
  (life-calendar--load-data-file)
  (let* ((birthday (life-calendar--ensure-birthday))
         (birth-time (life-calendar--parse-date birthday))
         (age (life-calendar--current-age birth-time)))
    (switch-to-buffer (get-buffer-create "*Life Calendar*"))
    ;; Set mode first, before rendering, because `define-derived-mode'
    ;; calls `kill-all-local-variables' which would wipe buffer-local
    ;; variables set during rendering.
    (life-calendar-mode)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert (life-calendar--render-calendar birth-time life-calendar-years))
      ;; Go to current week, or first week if current week not found
      (let ((pos (life-calendar--find-week (car age) (cdr age))))
        (goto-char (or pos
                       (life-calendar--find-next-week (point-min))))))))

(provide 'life-calendar)

;;; life-calendar.el ends here
