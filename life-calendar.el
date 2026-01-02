;;; life-calendar.el --- Display your life in weeks  -*- lexical-binding: t; -*-

;; Copyright (c) 2025 Vadzim Shender

;; Author: Vadzim Shender
;; URL: https://github.com/vshender/emacs-life-calendar
;; Version: 0.1.0
;; Keywords: calendar, visualization
;; Package-Requires: ((emacs "27.1") (org "8.0"))
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
;; Customization:
;;   M-x customize-group RET life-calendar RET

;;; Code:

(require 'org)

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

;;; Internal Functions

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

(defun life-calendar--count-completed-weeks (start-week-dow from-date before-date)
  "Count weeks that complete in the interval [FROM-DATE, BEFORE-DATE).
START-WEEK-DOW is the day of week (0=Sunday, ..., 6=Saturday) when weeks start.
A week is counted if its completion day (last day) falls within the interval."
  (let* (;; Find the completion day (last day) of the week containing
         ;; `from-date'.  Since a week that completes before `from-date'
         ;; doesn't belong to the interval, this is the first `week-completion'
         ;; to consider.
         (first-week-start (life-calendar--last-week-start-on-or-before
                            from-date start-week-dow))
         (week-completion (life-calendar--add-days first-week-start 6))
         (weeks 0))
    (while (time-less-p week-completion before-date)
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

(defun life-calendar--current-age (birth-time)
  "Return (YEARS-LIVED WEEKS-LIVED) for today given BIRTH-TIME.
YEARS-LIVED is the number of complete years, WEEKS-LIVED is the number
of complete weeks within the current year.  Uses the configured week
start day for accurate calculation."
  (let* ((today (life-calendar--current-date))
         ;; Calculate years lived by decoding the time difference.
         ;; Subtracting the epoch year converts the decoded year to a duration.
         (years-lived (- (decoded-time-year
                          (life-calendar--decode-date (time-subtract today birth-time)))
                         (decoded-time-year
                          (life-calendar--decode-date 0))))
         (current-birthday (life-calendar--nth-birthday birth-time years-lived))
         (weeks-lived (life-calendar--count-completed-weeks
                       (life-calendar--effective-week-start-dow birth-time)
                       current-birthday
                       today)))
    (list years-lived weeks-lived)))

(defun life-calendar--render-week-header ()
  "Render the week number header line.
Shows week numbers at positions 1, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50."
  (let ((header (make-string 53 ?\s))
        (week-marks '(1 5 10 15 20 25 30 35 40 45 50)))
    (dolist (week week-marks)
      (let* ((pos (1- week))
             (num-str (number-to-string week))
             (num-len (length num-str)))
        (dotimes (i num-len)
          (aset header (+ pos i) (aref num-str i)))))
    (concat "     "
            (propertize header 'face 'life-calendar-age-face)
            "\n")))

(defun life-calendar--render-calendar (birth-time years)
  "Render the life calendar for BIRTH-TIME with YEARS total years.
Returns a string containing the rendered calendar."
  (pcase-let* ((`(,years-lived ,weeks-lived)
               (life-calendar--current-age birth-time))
               (years (max years (1+ years-lived)))
               (lines '()))
    ;; Header
    (push (propertize
           (format "Life Calendar - %d years, %d weeks old\n\n"
                   years-lived weeks-lived)
           'face 'life-calendar-header-face)
          lines)
    ;; Week numbers header
    (push (life-calendar--render-week-header) lines)
    ;; Calendar grid
    (dotimes (year years)
      (let* ((weeks-in-year (life-calendar--count-weeks-in-year birth-time year))
             (row-parts '()))
        ;; Age label
        (push (propertize (format "%3d: " year) 'face 'life-calendar-age-face)
              row-parts)
        ;; Weeks
        (dotimes (week weeks-in-year)
          (let* ((is-current-year (= year years-lived))
                 (is-past-year (< year years-lived))
                 (is-current-week (and is-current-year (= week weeks-lived)))
                 (is-past-week (or is-past-year
                                   (and is-current-year (< week weeks-lived))))
                 (char (cond
                        (is-current-week
                         (propertize life-calendar-current-char
                                     'face 'life-calendar-current-face))
                        (is-past-week
                         (propertize life-calendar-past-char
                                     'face 'life-calendar-past-face))
                        (t
                         (propertize life-calendar-future-char
                                     'face 'life-calendar-future-face)))))
            (push char row-parts)))
        (push (apply #'concat (nreverse row-parts)) lines)
        (push "\n" lines)))
    (apply #'concat (nreverse lines))))

(defun life-calendar--ensure-birthday ()
  "Ensure `life-calendar-birthday' is set.
If not set, prompt the user to enter their birthday using `org-read-date'
and save the value for future sessions."
  (unless life-calendar-birthday
    (let ((date (org-read-date nil nil nil "Enter your birthday: ")))
      (customize-save-variable 'life-calendar-birthday date)
      (message "Birthday saved: %s" date)))
  life-calendar-birthday)

;;; Major Mode

(defvar life-calendar-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") #'quit-window)
    (define-key map (kbd "g") #'life-calendar-refresh)
    map)
  "Keymap for `life-calendar-mode'.")

(define-derived-mode life-calendar-mode special-mode "Life Calendar"
  "Major mode for viewing the life calendar.

\\{life-calendar-mode-map}"
  (setq buffer-read-only t)
  (setq truncate-lines t))

;;; Commands

(defun life-calendar-refresh ()
  "Refresh the life calendar display."
  (interactive)
  (when (eq major-mode 'life-calendar-mode)
    (let* ((birthday (life-calendar--ensure-birthday))
           (birth-time (life-calendar--parse-date birthday))
           (inhibit-read-only t))
      (erase-buffer)
      (insert (life-calendar--render-calendar birth-time life-calendar-years))
      (goto-char (point-min)))))

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
  (let* ((birthday (life-calendar--ensure-birthday))
         (birth-time (life-calendar--parse-date birthday)))
    (switch-to-buffer (get-buffer-create "*Life Calendar*"))
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert (life-calendar--render-calendar birth-time life-calendar-years)))
    (goto-char (point-min))
    (life-calendar-mode)))

(provide 'life-calendar)

;;; life-calendar.el ends here
