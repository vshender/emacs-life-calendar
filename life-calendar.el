;;; life-calendar.el --- Display your life in weeks  -*- lexical-binding: t; -*-

;; Copyright (c) 2025 Vadzim Shender

;; Author: Vadzim Shender
;; URL: https://github.com/vshender/emacs-life-calendar
;; Version: 0.1.0
;; Keywords: calendar, visualization
;; Package-Requires: ((emacs "24.1") (org "8.0"))
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

(defun life-calendar--ensure-birthday ()
  "Ensure `life-calendar-birthday' is set.
If not set, prompt the user to enter their birthday using `org-read-date'
and save the value for future sessions."
  (unless life-calendar-birthday
    (let ((date (org-read-date nil nil nil "Enter your birthday: ")))
      (customize-save-variable 'life-calendar-birthday date)
      (message "Birthday saved: %s" date)))
  life-calendar-birthday)

(defun life-calendar--parse-date (date-string)
  "Parse DATE-STRING in YYYY-MM-DD format to a time value."
  (let ((parts (split-string date-string "-")))
    (encode-time 0 0 0
                 (string-to-number (nth 2 parts))
                 (string-to-number (nth 1 parts))
                 (string-to-number (nth 0 parts)))))

(defun life-calendar--weeks-since-birth (birthday)
  "Calculate the number of weeks since BIRTHDAY until now.
BIRTHDAY is a string in YYYY-MM-DD format."
  (let* ((birth-time (life-calendar--parse-date birthday))
         (now (current-time))
         (diff (float-time (time-subtract now birth-time)))
         (weeks (floor (/ diff (* 7 24 60 60)))))
    weeks))

(defun life-calendar--render-week-header ()
  "Render the week number header line.
Shows week numbers at positions 1, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50."
  (let ((header (make-string 52 ?\s))
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

(defun life-calendar--render-calendar (birthday years)
  "Render the life calendar for BIRTHDAY with YEARS total years.
Returns a string containing the rendered calendar."
  (let* ((total-weeks (* years 52))
         (weeks-lived (life-calendar--weeks-since-birth birthday))
         (years-lived (/ weeks-lived 52))
         (week-of-year (mod weeks-lived 52))
         (years (max years (1+ years-lived)))
         (lines '()))
    ;; Header
    (push (propertize
           (format "Life Calendar - %d years, %d weeks old\n\n"
                   years-lived week-of-year)
           'face 'life-calendar-header-face)
          lines)
    ;; Week numbers header
    (push (life-calendar--render-week-header) lines)
    ;; Calendar grid
    (dotimes (year years)
      (let ((row-parts '()))
        ;; Age label
        (push (propertize (format "%3d: " year) 'face 'life-calendar-age-face)
              row-parts)
        ;; Weeks
        (dotimes (week 52)
          (let* ((week-number (+ (* year 52) week))
                 (char (cond
                        ((< week-number weeks-lived)
                         (propertize life-calendar-past-char
                                     'face 'life-calendar-past-face))
                        ((= week-number weeks-lived)
                         (propertize life-calendar-current-char
                                     'face 'life-calendar-current-face))
                        (t
                         (propertize life-calendar-future-char
                                     'face 'life-calendar-future-face)))))
            (push char row-parts)))
        (push (apply #'concat (nreverse row-parts)) lines)
        (push "\n" lines)))
    (apply #'concat (nreverse lines))))

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
    (let ((birthday (life-calendar--ensure-birthday))
          (inhibit-read-only t))
      (erase-buffer)
      (insert (life-calendar--render-calendar birthday life-calendar-years))
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
  (let ((birthday (life-calendar--ensure-birthday)))
    (switch-to-buffer (get-buffer-create "*Life Calendar*"))
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert (life-calendar--render-calendar birthday life-calendar-years)))
    (goto-char (point-min))
    (life-calendar-mode)))

(provide 'life-calendar)

;;; life-calendar.el ends here
