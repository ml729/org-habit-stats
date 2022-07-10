;;; org-habit-stats.el --- compute info about habits  -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 null
;;
;; Author: ml729
;; Created: October 22, 2021
;; Modified: October 22, 2021
;; Version: 0.0.1
;; Keywords:
;; Homepage:
;; Package-Requires: ((emacs "24.4"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;
;;
;;; Code:

(require 'org-habit)
(require 'cl-lib)
(require 'seq)
;; (require 'org-habit-stats-chart)

;; (defvar org-habit-stats-list 1)
;; (defvar org-habit-stats-graph-drawer-name)
(defgroup org-habit-stats nil
  "Tempo templates/snippets with in-buffer field editing."
  :group 'org-progress
  :prefix "org-habit-stats-")

;;; Defcustoms
(defcustom org-habit-stats-insert-graph-in-file t
  "Whether or not to insert ascii graph of habit scores in file."
  :group 'org-habit-stats
  :type 'boolean)

(defcustom org-habit-stats-graph-drawer-name "GRAPH"
  "Name of drawer that stores habit graph."
  :group 'org-habit-stats
  :type 'string)

(defcustom org-habit-stats-graph-colors-for-light-list
  '("#ef7969"
    "#49c029"
    "#ffcf00"
    "#7090ff"
    "#e07fff"
    "#70d3f0")
  "Colors to use for bars of habit bar graph for light themes. The default colors are
Modus Vivendi's colors for graphs. The original value of
chart-face-color-list is unaffected.")

(defcustom org-habit-stats-graph-colors-for-dark-list
  '("#b52c2c"
    "#24bf00"
    "#f7ef00"
    "#2fafef"
    "#bf94fe"
    "#47dfea")
  "Colors to use for bars of habit bar graph for dark themes. The default colors are
Modus Vivendi's colors for graphs. The original value of
chart-face-color-list is unaffected.")

(defcustom org-habit-stats-graph-width 70
  "Width of x-axis of graph (in columns), not including origin.")

(defcustom org-habit-stats-graph-height 12
  "Height of y-axis of graph (in line numbers), not including origin.")

(defcustom org-habit-stats-graph-left-margin 5
  "Number of columns to the left of y-axis.")

(defcustom org-habit-stats-graph-min-num-bars 3
  "How many bars to shift left when the bar graph is truncated.")

(defcustom org-habit-stats-graph-current-offset 0
  "How many bars to shift left when the bar graph is truncated.")

(defcustom org-habit-stats-graph-number-months 5
  "How many months to display when graph's x-axis is months.")

(defcustom org-habit-stats-graph-number-weeks 10
  "How many weeks to display when graph's x-axis is weeks.")

(defcustom org-habit-stats-graph-number-days 15
  "How many days to display when graph's x-axis is days.")

(defcustom org-habit-stats-view-order '(statistics graph calendar)
  "Output from org-habit-parse-todo of currently viewed habit.")

(defcustom org-habit-stats-graph-data-horizontal-char ?-
  "Character used to draw horizontal lines for a graph's data.")

(defcustom org-habit-stats-graph-data-vertical-char ?|
  "Character used to draw vertical lines for a graph's data.")

(defcustom org-habit-stats-graph-axis-horizontal-char ?-
  "Character used to draw horizontal lines for a graph's axes.")

(defcustom org-habit-stats-graph-axis-vertical-char ?|
  "Character used to draw vertical lines for a graph's axes.")

(defcustom org-habit-stats-months-names-alist
  '(("Jan" . 1)
    ("Feb" . 2)
    ("Mar" . 3)
    ("Apr" . 4)
    ("May" . 5)
    ("Jun" . 6)
    ("Jul" . 7)
    ("Aug" . 8)
    ("Sep" . 9)
    ("Oct" . 10)
    ("Nov" . 11)
    ("Dec" . 12))
  "Month names used in graphs.")

(defcustom org-habit-stats-days-names-alist
  '(("Sun" . 1)
    ("Mon" . 2)
    ("Tue" . 3)
    ("Wed" . 4)
    ("Thu" . 5)
    ("Fri" . 6)
    ("Sat" . 7))
  "Day names used in graphs.")

(defcustom org-habit-stats-graph-date-format
  "%m/%d"
  "Date format used in graphs for dates in graphs.")

(defcustom org-habit-stats-stat-functions-alist
  '((org-habit-stats-exp-smoothing-list-score . "Strength")
     ;; (org-habit-stats-present-streak . "Current-Streak")
     ;; (org-habit-stats-present-unstreak . "Current Unstreak")
     ;; org-habit-stats-recent-unstreak
     ;; (org-habit-stats-record-streak . "Record Streak")
     (org-habit-stats-alltime-total . "Total Completions")
     (org-habit-stats-alltime-percentage . "Total Percentage"))
  "Alist mapping stat functions to their names. All stat
functions take in the original parsed habit data (outputted by
org-habit-parse-todo) and the full habit history (outputted by
org-habit-stats-get-full-history-new-to-old)")

(defcustom org-habit-stats-graph-functions-alist
  '((org-habit-stats-graph-completions-per-month . ("m"
                                                   "Monthly Completions"
                                                   "Months"
                                                   "Completions"
                                                   vertical
                                                   5))
    (org-habit-stats-graph-completions-per-week . ("w"
                                                   "Weekly Completions"
                                                   "Weeks"
                                                   "Completions"
                                                   vertical
                                                   10))
    (org-habit-stats-graph-completions-per-weekday . ("d"
                                                   "Completions by Day"
                                                   "Day"
                                                   "Completions"
                                                   vertical
                                                   7))
    (org-habit-stats-graph-daily-strength . ("s"
                                             "Daily Habit Strength"
                                             "Day"
                                             "Strength"
                                             vertical
                                             14)))
  "Alist mapping graph functions to a list containing: the key
that invokes the function, the title of the graph, the name of
the x-axis, the name of the y-axis, the graph direction, and the
max number of bars to show at a time.")

(defcustom org-habit-stats-graph-default-func 'org-habit-stats-graph-completions-per-week
  "Current graph function used in org habit stats buffer.")

(defcustom org-habit-stats-new-habit-message "A new habit."
  "Message to display when habit has 0 completions logged.")

;;; Defvars
(defvar org-habit-stats-buffer "*Org-Habit-Stats*"
  "Name of the buffer used for displaying stats, calendar, and graphs.")

(defvar org-habit-stats-calendar-buffer "*Org-Habit-Stats Calendar*"
  "Name of the buffer used for the calendar.")

(defvar org-habit-stats-displayed-month nil
  "Stores displayed-month value for calendar functions.")

(defvar org-habit-stats-displayed-year nil
  "Stores displayed-year value for calendar functions.")

(defvar org-habit-stats-graph-face-list nil
  "Faces used for bars in graphs, generated from org-habit-stats-graph-colors-for-light-list
or org-habit-stats-graph-colors-for-dark-list based on the background type.")

(defvar org-habit-stats-current-habit-data nil
  "Output from org-habit-parse-todo of currently viewed habit.")

(defvar org-habit-stats-graph-current-func nil
  "Current graph function used in org habit stats buffer.")

(defvar org-habit-stats-graph-text-alist
  '(org-habit-stats-graph-monthly-completions . ("Monthly Completions" "Months" "Completions"))
  "Alist mapping graph functions to a list containing the graph title,
x-axis name, y-axis name.")

;; (defvar org-habit-stats-graph-keys-alist
;;   "Alist mapping graph keys to graph functions.")

;;; Faces
(defface org-habit-stats-graph-label
  '((t (:inherit default)))
  "Face for a habit graph's axis labels."
  :group 'org-habit-stats)

(defface org-habit-stats-graph-name
  '((t (:weight bold)))
  "Face for a habit graph's axis name."
  :group 'org-habit-stats)

(defface org-habit-stats-graph-title
  '((t (:weight bold)))
  "Face for a habit graph's title."
  :group 'org-habit-stats)

(defface org-habit-stats-calendar-completed
  '((t (:foreground "#004c00" :background "#e0a3ff")))
  "Face for days in the calendar where the habit was completed."
  :group 'org-habit-stats)

(defface org-habit-stats-stat-value
  '((t (:foreground "#004c00" :background "#aceaac")))
  "Face for statistics value."
  :group 'org-habit-stats)

(defface org-habit-stats-stat-name
  '((t (:inherit org-agenda-structure-secondary)))
  "Face for statistics value."
  :group 'org-habit-stats)

(defface org-habit-stats-habit-name
  '((t (:inherit org-agenda-structure)))
  "Face for habit name."
  :group 'org-habit-stats)

(defface org-habit-stats-section-name
  '((t (:inherit default)))
  "Face for section name in org-habit-stats buffer."
  :group 'org-habit-stats)

;; Stats functions
(defun org-habit-stats-dates-to-binary (tasks)
  "Return binary version of TASKS from newest to oldest, where
TASKS is a list of all the past dates this habit was marked
closed. Assumes the dates logged for the habit are in order,
newest to oldest."
  (let* ((bin-hist '()))
    (while (> (length tasks) 1)
      (push 1 bin-hist)
      (let* ((next (pop tasks))
             (diff (- (nth 0 tasks) next)))
        (while (> diff 1)
          (push 0 bin-hist)
          (setq diff (- diff 1)))))
    (if (= (length tasks) 1) (push 1 bin-hist))
    bin-hist))

;; (defun org-habit-stats-dates-to-binary (history)
;;   (let* ((today (org-today))
;;          day (pop history)
;;          (bin-hist '()))
;;     (while history
;;     (push (cons day 1) bin-hist)
;;     (setq day (1+ day))
;;     (while (< day (car history))
;;       (push (cons day 0) bin-hist)
;;       (setq day (1+ day))))

;;     (let ((diff (- (car history) day)))
;;       (if (> diff 1)
;;           (dotimes (i (1- diff))
;;             (push (cons (+ day i 1) 0))))
;;     (if (> (- (car history) day) 1)
;;         (dotimes i (1- (-))
;;                  )))))
(defun org-habit-stats-get-full-history-new-to-old (history)
  (let* ((today (org-today))
         (history (add-to-list 'history (1+ today)))
         (bin-hist nil))
    (seq-reduce
     (lambda (a b)
       (push (cons a 1) bin-hist)
       (setq a (1+ a))
       (while (< a b)
         (push (cons a 0) bin-hist)
         (setq a (1+ a)))
       b)
     history
     (car history))
    bin-hist))
(defun org-habit-stats-get-full-history-old-to-new (history)
  (reverse (org-habit-stats-get-full-history-new-to-old history)))

(defun org-habit-stats--streak (h)
  (if (= (cdr (pop h)) 1)
      (1+ org-habit-stats--streak h)
    0))

(defun org-habit-stats-streak (history history-rev &optional habit-data)
  "Returns the current streak. If habit is completed today,
include it. If not, begin counting current streak from
yesterday."
  (if (= (cdr (pop history-rev)) 1)
      (1+ (org-habit-stats-streak history-rev))
    (org-habit-stats-streak history-rev)))

(defun org-habit-stats--record-streak-full (history history-rev &optional habit-data)
  "Returns (a b) where a is the record streak,
   b is the day the record streak occurred."
  (let ((record-streak 0)
        (record-day 0)
        (curr-streak 0)
        (curr-streak-start 0)
        (curr-day 0))
    (while history-rev
      (if (= (cdr (pop history-rev)) 1)
          (progn
            (when (= curr-streak 0)
              (setq curr-streak-start curr-day))
            (setq curr-streak (1+ curr-streak)))
        (setq curr-streak 0))
      (when (> curr-streak record-streak)
        (setq record-streak curr-streak)
        (setq record-day curr-streak-start))
      (setq curr-day (1+ curr-day)))
    (cons record-streak (org-date-to-gregorian (- (org-today) record-day)))))

(defun single-whitespace-only (s)
  (string-join
   (seq-filter (lambda (x) (if (> (length x) 0) t))
               (split-string s " "))
   " "))

(defun org-habit-stats-record-streak-format (history history-rev habit-data)
  (let* ((record-data (org-habit-stats--record-streak-full history-rev habit-data))
         (record-streak (car record-data))
         (record-day (cdr record-data)))
    (concat (number-to-string record-streak)
            ", on "
            (single-whitespace-only (org-agenda-format-date-aligned record-day)))))

(defun org-habit-stats-record-streak-days (history history-rev habit-data)
  (car (org-habit-stats--record-streak-full history history-rev habit-data)))

(defun org-habit-stats-record-streak-date (history history-rev habit-data)
  (cdr (org-habit-stats--record-streak-full history history-rev habit-data)))

(defun org-habit-stats--N-day-total (history history-rev N)
  (if (and (> N 0) history-rev)
      (if (= (cdr (pop history-rev)) 1)
          (1+ (org-habit-stats--N-day-total history history-rev (1- N)))
        (org-habit-stats--N-day-total history history-rev (1- N)))
    0))
(defun org-habit-stats--N-day-percentage (history history-rev N habit-data)
  (let ((repeat-len (nth 1 habit-data)))
  (/ (org-habit-stats--N-day-total history history-rev N) (/ (float N) repeat-len))))

(defun org-habit-stats-30-day-total (history history-rev habit-data)
  (org-habit-stats--N-day-percentage history history-rev 30))

(defun org-habit-stats-365-day-total (history history-rev habit-data)
  (org-habit-stats--N-day-percentage history history-rev 365))

(defun org-habit-stats-alltime-total (history history-rev habit-data)
  (length (nth 4 habit-data)))

(defun org-habit-stats-alltime-percentage (history history-rev habit-data)
  (let ((repeat-len (nth 1 habit-data)))
  (/ (length (nth 4 habit-data)) (/ (float (length history)) repeat-len))))

(defun org-habit-stats-exp-smoothing-list--full (history history-rev habit-data)
  "Returns score for a binary list HISTORY,
   computed via exponential smoothing. (Inspired by the open
   source Loop Habit Tracker app's score.)"
  (let* ((scores '(0))
         (freq 1.0)
         (alpha (expt 0.5 (/ (sqrt freq) 13))))
    (while history
      (push (+ (* alpha (nth 0 scores))
               (* (- 1 alpha) (cdr (pop history)))) scores))
    (setq scores (mapcar (lambda (x) (* 100 x)) scores))
    scores))
(defun org-habit-stats-exp-smoothing-list-score (history history-rev habit-data)
  (nth 0 (org-habit-stats-exp-smoothing-list--full history history-rev habit-data)))

(defun org-habit-stats-get-freq (seq &optional key-func value-func)
  "Return frequencies of elements in SEQ. If KEY-FUNC, use
KEY-FUNC to produce keys for hash table. Credit to
https://stackoverflow.com/a/6050245"
  (let ((h (make-hash-table :test 'equal))
        (freqs nil)
        (value-func (if value-func value-func (lambda (x) 1))))
    (dolist (x seq)
      (let ((key (if key-func (funcall key-func x) x)))
        (puthash key (+ (gethash key h 0) (funcall value-func x)) h)))
    (maphash #'(lambda (k v) (push (cons k v) freqs)) h)
    freqs))

(defun org-habit-stats-calculate-stats (history history-rev habit-data)
  (let ((statresults '()))
  (dolist (x org-habit-stats-stat-functions-alist)
    (let* ((statfunc (car x))
           (statname (cdr x))
           (statresult (if (fboundp statfunc) (funcall statfunc history history-rev habit-data))))
      (when statresult
        (push (cons statname statresult) statresults))))
    (reverse statresults)))

(defun org-habit-stats-transpose-pair-list (a)
  (cons (mapcar 'car a) (mapcar 'cdr a)))


;;; Calendar functions

(defun org-habit-stats-calendar-mark-habits (habit-data)
  (let ((completed-dates (nth 4 habit-data))
        (calendar-buffer org-habit-stats-calendar-buffer))
    (dolist (completed-day completed-dates nil)
      (let ((completed-day-gregorian (calendar-gregorian-from-absolute completed-day)))
        (when (calendar-date-is-visible-p completed-day-gregorian)
            (calendar-mark-visible-date completed-day-gregorian 'org-habit-stats-calendar-completed))))))

(defun org-habit-stats-get-calendar-contents ()
  (with-current-buffer org-habit-stats-calendar-buffer
    (buffer-string)))

(defun org-habit-stats-get-calendar-overlays ()
  (with-current-buffer org-habit-stats-calendar-buffer
    (let ((ol-list (overlay-lists)))
      (append (car ol-list) (cdr ol-list)))))

(defun org-habit-stats-apply-overlays (ol-list offset buffer)
  (dolist (ol ol-list)
     (move-overlay (copy-overlay ol)
                   (+ (overlay-start ol) offset)
                   (+ (overlay-end ol) offset)
                   buffer)))

;; create calendar buffer, inject text at top, mark custom dates, set so curr month on the right first
(defun org-habit-stats-make-calendar-buffer (habit-data)
  ;; (interactive "P")
  ;; (with-current-buffer
  (with-current-buffer
      (get-buffer-create org-habit-stats-calendar-buffer)
    (calendar-mode)
    (let* ((date (calendar-current-date))
           (month (1- (calendar-extract-month date)))
           (year (calendar-extract-year date))
           (completed-dates (nth 4 habit-data)))
      (calendar-generate-window month year)
      (setq org-habit-stats-displayed-month month)
      (setq org-habit-stats-displayed-year year)
      (org-habit-stats-calendar-mark-habits habit-data)
      )
    (run-hooks 'calendar-initial-window-hook)))

(defun org-habit-stats-refresh-calendar-buffer ()
  (with-current-buffer org-habit-stats-calendar-buffer
    (setq buffer-read-only nil)
    (erase-buffer)
    (let* ((date (calendar-current-date))
           (month org-habit-stats-displayed-month)
           (year org-habit-stats-displayed-year)
           (habit-data org-habit-stats-current-habit-data))
      (calendar-generate-window month year)
      (org-habit-stats-calendar-mark-habits habit-data)
      (setq buffer-read-only t))))

;;; Calender commands
(defun org-habit-stats-increment-displayed-month (n)
  "Increment displayed month by N, adjusting displayed-year
accordingly. N can be any integer."
  (if (> n 0)
      (while (> n 12)
        (setq n (- n 12))
        (setq org-habit-stats-displayed-year
              (1+ org-habit-stats-displayed-year)))
    (while (< n 0)
      (setq n (+ n 12))
      (setq org-habit-stats-displayed-year
            (1- org-habit-stats-displayed-year))
      ))
  (let ((sum (+ n org-habit-stats-displayed-month)))
    (if (> sum 12)
        (progn
          (setq org-habit-stats-displayed-month (mod sum 12))
                (setq org-habit-stats-displayed-year
                      (1+ org-habit-stats-displayed-year)))
      (setq org-habit-stats-displayed-month sum))))


(defun org-habit-stats-send-calendar-command (cal-cmd)
  (with-current-buffer org-habit-stats-calendar-buffer
    (let* ((displayed-month org-habit-stats-displayed-month)
           (displayed-year org-habit-stats-displayed-year))
      (funcall cal-cmd)
      (org-habit-stats-calendar-mark-habits org-habit-stats-current-habit-data)))
  (org-habit-stats-refresh-buffer))


(defun org-habit-stats--calendar-scroll (n)
  (org-habit-stats-increment-displayed-month n)
  (org-habit-stats-refresh-calendar-buffer)
  (org-habit-stats-refresh-calendar-section))

(defun org-habit-stats-calendar-scroll-right (arg)
  (interactive "p")
  (org-habit-stats--calendar-scroll arg))

(defun org-habit-stats-calendar-scroll-left (arg)
  (interactive "p")
  (org-habit-stats--calendar-scroll (- arg)))

(defun org-habit-stats-calendar-scroll-right-three-months (arg)
  (interactive "p")
  (org-habit-stats--calendar-scroll (* 3 arg)))

(defun org-habit-stats-calendar-scroll-left-three-months (arg)
  (interactive "p")
  (org-habit-stats--calendar-scroll (* -3 arg)))
(defun org-habit-stats-calendar-forward-year (arg)
  (interactive "p")
  (org-habit-stats--calendar-scroll (* 12 arg)))

(defun org-habit-stats-calendar-backward-year (arg)
  (interactive "p")
  (org-habit-stats--calendar-scroll (* -12 arg)))


;;; Graph data functions
(defun org-habit-stats-graph-count-per-category (history category-func predicate-func format-func)
  "For each date in HISTORY, get its category (e.g. which month,
week, day of the week, etc.) using CATEGORY-FUNC, get counts per
category, sort categories with PREDICATE-FUNC, and convert
categories to readable names with FORMAT-FUNC. Returns a pair of
two lists, the first containing names of the categories, the
second containing the corresponding counts per category."
  (org-habit-stats-transpose-pair-list
   (mapcar (lambda (x) (cons (funcall format-func (car x)) (cdr x)))
           (sort (org-habit-stats-get-freq
                  (mapcar (lambda (x) (cons (funcall category-func (car x)) (cdr x)))
                          history)
                  (lambda (x) (car x))
                  (lambda (x) (cdr x)))
                 (lambda (x y) (funcall predicate-func (car x) (car y)))))))

(defun org-habit-stats-graph-completions-per-month (history history-rev habit-data)
  "Returns a pair of lists (months . counts)."
  (org-habit-stats-graph-count-per-category
   history
   (lambda (d) (let ((day (calendar-gregorian-from-absolute d)))
                     (list (car day) (caddr day)))) ;; converts absolute date to list (month year)
   (lambda (m1 m2) (cond ((< (nth 1 m1) (nth 1 m2)) t)
                         ((= (nth 1 m1) (nth 1 m2)) (if (< (nth 0 m1) (nth 0 m2)) t nil))
                         (t nil)))
   (lambda (m) (car (rassoc (nth 0 m) org-habit-stats-months-names-alist)))))

(defun org-habit-stats--unix-from-absolute-time (abs-time)
  (- abs-time (org-habit-stats-days-to-time (calendar-absolute-from-gregorian '(12 31 1969)))))

(defun org-habit-stats-format-absolute-time-string (format-string &optional time zone)
  (format-time-string format-string
                      (org-habit-stats--unix-from-absolute-time time)
                      zone))

(defun org-habit-stats-graph-completions-per-week (history history-rev habit-data)
  "Returns a pair of lists (weeks . counts)."
  (org-habit-stats-graph-count-per-category
   history
   (lambda (d) (- d (mod d 7))) ;; converts absolute date to the sunday before or on; (month day year) format
   (lambda (d1 d2) (< d1 d2))
   (lambda (d) (let ((time (org-habit-stats-days-to-time d)))
                 (org-habit-stats-format-absolute-time-string org-habit-stats-graph-date-format
                                     time)))))

(defun org-habit-stats-graph-completions-per-weekday (history history-rev habit-data)
  "Returns a pair of lists (weeks . counts)."
  (org-habit-stats-graph-count-per-category
   history
   (lambda (d) (mod d 7))
   (lambda (m1 m2) (< m1 m2))
   (lambda (m) (car (rassoc (1+ m) org-habit-stats-days-names-alist)))))

(defun org-habit-stats-graph-daily-strength (history history-rev habit-data)
  "Returns a pair of lists (days . habit strengths)."
  (let* ((dayslst (mapcar (lambda (d) (org-habit-stats-format-absolute-time-string
                                   org-habit-stats-graph-date-format
                                   (org-habit-stats-days-to-time (car d))))
                        history)))
     (cons dayslst
           (reverse (org-habit-stats-exp-smoothing-list--full
                     history history-rev habit-data)))
    )
  )

(defun org-habit-stats-graph-completions-test (history)
  (org-habit-stats-graph-count-per-category
   history
   (lambda (d) 1)
   (lambda (m1 m2) t)
   (lambda (m) "hi")
   ))

;;; Chart functions
;; (defun org-habit-stats-color-brightness (hex)
;;   "Formula from https://alienryderflex.com/hsp.html"
;;   (let ((R (substring hex 1 3))
;;         (G (substring hex 3 5))
;;         (B (substring hex 5 7))))
;;   )

(defclass org-habit-stats-chart-sequence ()
  ((data :initarg :data
         :initform nil)
   (name :initarg :name
         :initform "Data"))
  "Class used for all data in different charts, originally defined in charts.el as 'chart-sequence'.
   Some earlier versions of Emacs the name of this class contains
   a typo ('chart-sequece') so we redefine it here under a new
   name.")
(defun org-habit-stats-graph-create-faces ()
  "TODO add terminal support"
  (let ((light-bg (if (equal (frame-parameter nil 'background-mode) 'light) t nil))
        (faces ())
        newface)
    (dolist (color (if light-bg org-habit-stats-graph-colors-for-light-list
                     org-habit-stats-graph-colors-for-dark-list))
      (setq newface (make-face
                (intern (concat "org-habit-chart-" color))))
            (set-face-background newface color)
            (set-face-foreground newface "black")
            (push newface faces))
    faces))

(defun org-habit-stats-chart-draw-line-custom-char (dir zone start end horizontal-char vertical-char)
  "Draw a line using line-drawing characters in direction DIR.
Use column or row ZONE between START and END."
  (chart-display-label
   (make-string (- end start) (if (eq dir 'vertical) vertical-char horizontal-char))
   dir zone start end))

(defun org-habit-stats-chart-draw-line-data (dir zone start end)
  (org-habit-stats-chart-draw-line-custom-char dir zone start end
                                   org-habit-stats-graph-data-horizontal-char
                                   org-habit-stats-graph-data-vertical-char))

(defun org-habit-stats-chart-draw-line-axis (dir zone start end)
  (org-habit-stats-chart-draw-line-custom-char dir zone start end
                                   org-habit-stats-graph-axis-horizontal-char
                                   org-habit-stats-graph-axis-vertical-char))

(setq org-habit-stats-graph-data-vertical-char ?|)
(setq org-habit-stats-graph-data-horizontal-char ?-)

(defun org-habit-stats-chart-draw-data (c)
  (unwind-protect
      (progn
      (advice-add 'chart-draw-line :override
                  'org-habit-stats-chart-draw-line-data
                  '((name . org-habit-stats-draw-line-override)))
    (chart-draw-data c))
    (advice-remove 'chart-draw-line 'org-habit-stats-draw-line-override)))

(defun org-habit-stats-chart-draw-axis (c)
  (unwind-protect
      (progn
      (advice-add 'chart-draw-line :override
                  'org-habit-stats-chart-draw-line-axis
                  '((name . org-habit-stats-draw-line-override)))
    (chart-draw-axis c))
    (advice-remove 'chart-draw-line 'org-habit-stats-draw-line-override)))

(defun org-habit-stats-chart-draw-title (c &optional align-left)
  "Draw a title of chart. By default, centered. If ALIGN-LEFT, align-left."
  (if align-left
      (chart-display-label (oref c title) 'horizontal
                           (- (oref c x-margin) 2)
                           (oref c y-margin) (+ (length (oref c title))
                                                (oref c y-margin))
                           (oref c title-face))
    (chart-display-label (oref c title) 'horizontal
                         (- (oref c x-margin) 3)
                         (oref c y-margin) (+ (oref c x-width)
                                              (oref c y-margin))
                         (oref c title-face))))

(defun org-habit-stats-chart-draw (c &optional buff)
  "Start drawing a chart object C in optional BUFF.
Begins at line LINE."
  (with-silent-modifications
    (save-excursion
      (let (
            ;; (chart-face-list org-habit-stats-chart-face-color-list)
            )
      (if buff (set-buffer buff))
      ;; (goto-line line)
      (insert (make-string (window-height (selected-window)) ?\n))
      ;; (insert (make-string ((oref c 100) ?\n)))
      ;; Start by displaying the axis
      (org-habit-stats-chart-draw-axis c)
      ;; Display title
      (org-habit-stats-chart-draw-title c)
      ;; Display data
      ;; (message "Rendering chart...")
      (sit-for 0)
      (org-habit-stats-chart-draw-data c)
      ;; (message "Rendering chart...done")
      ))))


(defun org-habit-stats--chart-trim-offset (seq max offset end)
  (let* ((newbeg (min offset (- (length seq) max)))
         (newend (min (+ offset max) (length seq))))
    (if (>= newbeg 0)
        (if end
            (subseq seq (- newend) (if (> newbeg 0) (- newbeg)))
          (subseq seq newbeg newend))
      seq)))

(cl-defmethod org-habit-stats-chart-trim-offset ((c chart) max offset end)
  "Trim all sequences in chart C to be MAX elements. Does nothing
if a sequence is less than MAX elements long. If END is nil, trim
offset elements, keep the next MAX elements, and trim the
remaining elements. If END is t, trimming begins at the end of
the sequence instead."
  (let ((s (oref c sequences))
        (nx (if (equal (oref c direction) 'horizontal)
                        (oref c y-axis) (oref c x-axis))))
    (dolist (x s)
      (oset x data (org-habit-stats--chart-trim-offset
                    (oref x data) max offset end))
    (oset nx items (org-habit-stats--chart-trim-offset
                    (oref nx items) max offset end)))))

(defun org-habit-stats-chart-bar-quickie-extended (dir title namelst nametitle numlst numtitle
                                                       &optional max sort-pred offset end width height
                                                       topmargin leftmargin titleface nameface labelface)
  "Modification of function chart-bar-quickie to support custom
graph dimensions, margins, and faces. Inserts graph into current
buffer, with width WIDTH, height HEIGHT, vertical margin of
TOPMARGIN from current line, horizontal margin of LEFTMARGIN,
face TITLEFACE for title, face NAMEFACE for axis names, face
LABELFACE for labels for values. Original Docstring: Wash over
the complex EIEIO stuff and create a nice bar chart. Create it
going in direction DIR [`horizontal' `vertical'] with TITLE using
a name sequence NAMELST labeled NAMETITLE with values NUMLST
labeled NUMTITLE. Optional arguments: Set the chart's max element
display to MAX, and sort lists with SORT-PRED if desired."
  (interactive)
  (let ((nc (make-instance 'chart-bar
                           :title title
                           :title-face titleface
                           :x-margin topmargin
                           :y-margin leftmargin
                           :direction dir
                           ))
        (iv (eq dir 'vertical)))
    ;; chart.el sets the widths to the window dimensions after instantiation
    ;; manually set the widths to our values here
    (oset nc x-width width)
    (oset nc y-width height)
    (chart-add-sequence nc
                        (make-instance 'org-habit-stats-chart-sequence
                                       :data namelst
                                       :name nametitle)
                        (if iv 'x-axis 'y-axis))
    (chart-add-sequence nc
                        (make-instance 'org-habit-stats-chart-sequence
                                       :data numlst
                                       :name numtitle)
                        (if iv 'y-axis 'x-axis))
    (oset (oref nc x-axis) name-face nameface)
    (oset (oref nc x-axis) labels-face labelface)
    (oset (oref nc y-axis) name-face nameface)
    (oset (oref nc y-axis) labels-face labelface)
    (if sort-pred (chart-sort nc sort-pred))
    (if (integerp max) (org-habit-stats-chart-trim-offset nc max offset end))
    ;; (print nc)
    (org-habit-stats-chart-draw nc)))


;;; Draw graph
(defun org-habit-stats--draw-graph (dir title namelst nametitle numlst numtitle max-bars)
  (let ((namediff (- org-habit-stats-graph-min-num-bars (length namelst)))
        (numdiff (- org-habit-stats-graph-min-num-bars (length numlst))))
    (if (> namediff 0)
        (dotimes (x namediff)
          (push "" namelst)))
    (if (> numdiff 0)
        (dotimes (x numdiff)
          (push 0 numlst)))
    (let ((chart-face-list org-habit-stats-graph-face-list))
      (org-habit-stats-chart-bar-quickie-extended
       dir
       title
       namelst
       nametitle
       numlst
       numtitle
       max-bars
       nil
       org-habit-stats-graph-current-offset
       t
       org-habit-stats-graph-width
       org-habit-stats-graph-height
       (line-number-at-pos)
       org-habit-stats-graph-left-margin
       'org-habit-stats-graph-title
       'org-habit-stats-graph-name
       'org-habit-stats-graph-label))))

(defun org-habit-stats-draw-graph (history history-rev habit-data)
  (let* ((graph-start (point))
         (func org-habit-stats-graph-current-func)
         (func-info (cdr (assoc func org-habit-stats-graph-functions-alist)))
         (graph-title (nth 1 func-info))
         (x-name (nth 2 func-info))
         (y-name (nth 3 func-info))
         (dir (nth 4 func-info))
         (max-bars (nth 5 func-info))
         (graph-data-names (funcall func history history-rev habit-data))
         (graph-names (car graph-data-names))
         (graph-data (cdr graph-data-names)))
    (insert (make-string 3 ?\n))
    (org-habit-stats--draw-graph
     dir
     graph-title
     graph-names
     x-name
     graph-data
     y-name
     max-bars)
  (setq org-habit-stats-graph-bounds (cons graph-start (point-max)))))


;;; Graph commands
(defun org-habit-stats-switch-graph (graph-func)
  (setq org-habit-stats-graph-current-func graph-func)
  (setq org-habit-stats-graph-current-offset 0)
  (org-habit-stats-refresh-graph-section))

;;; Insert sections
(defun org-habit-stats-days-to-time (days)
  "Convert number of days DAYS to number of seconds."
  (* days 86400))
(defun org-habit-stats-insert-habit-info (habit-data habit-name habit-description)
  (let ((habit-repeat-period (nth 1 habit-data))
        (habit-repeat-string (nth 5 habit-data))
        (habit-next-scheduled (nth 0 habit-data)))
    (insert (propertize habit-name 'face 'org-habit-stats-habit-name)
            "\n")
    (if habit-description
        (insert habit-description "\n"))
    ;; insert habit repeat data, next due date
    (insert (format "Repeats every %s%d days"
                    habit-repeat-string habit-repeat-period)
            "\n")
    (insert (org-habit-stats-format-absolute-time-string "Next Scheduled: %A, %B %d, %Y"
                                                         (org-habit-stats-days-to-time habit-next-scheduled))
            "\n")

    ))

(defun org-habit-stats-format-one-stat (statname statdata)
  (let* ((fdata (cond ((integerp statdata) (format "%d" statdata))
                      ((floatp statdata) (format "%.3f" statdata))
                      (t statdata)))
         (numspaces (- 39 (+ (length statname) (length fdata)))))
    (concat (propertize statname 'face 'org-habit-stats-stat-name)
            " "
            (propertize fdata 'face 'org-habit-stats-stat-value)
            (if (> numspaces 0)
                (make-string numspaces 32)))))

(defun org-habit-stats-insert-stats (habit-data history history-rev)
  ;; insert habit stats
  (let* ((i 0)
         (stats-start (point))
           (statresults (org-habit-stats-calculate-stats history history-rev habit-data)))
      (dolist (x statresults)
        (insert (org-habit-stats-format-one-stat (car x)
                                                 (cdr x)))
        (setq i (1+ i))
        (when (and (> i 0) (= (mod i 2) 0))
          (insert "\n")))
      (insert "\n")
      (setq org-habit-stats-stat-bounds (cons stats-start (point)))))
(defun org-habit-stats-insert-calendar (habit-data)
  (let ((cal-start (point))
        (cal-start-line (line-number-at-pos))
        (cal-offset-for-overlay (1- (point))))
    (insert (org-habit-stats-get-calendar-contents))
    (org-habit-stats-apply-overlays (org-habit-stats-get-calendar-overlays)
                                    cal-offset-for-overlay
                                    (current-buffer))
    (let ((calendar-height (- (line-number-at-pos) cal-start-line)))
      (when (< calendar-height 8)
        (insert (make-string (- 7 calendar-height) ?\n)))
    (insert (make-string 1 ?\n))
    (setq org-habit-stats-calendar-bounds (cons cal-start (point))))))

;;; Refresh sections
(defun org-habit-stats-refresh-graph-section ()
  (let* ((graph-bounds org-habit-stats-graph-bounds)
         (graph-start (car graph-bounds))
         (graph-end (cdr graph-bounds))
         (completed-days (nth 4 org-habit-stats-current-habit-data))
         (history-rev (org-habit-stats-get-full-history-new-to-old completed-days))
         (history (reverse history-rev)))
    (save-excursion
      (goto-char graph-start)
      (delete-region graph-start graph-end)
      (org-habit-stats-draw-graph history history-rev org-habit-stats-current-habit-data))
    (set-buffer-modified-p nil)))

(defun org-habit-stats-refresh-calendar-section ()
  (let* ((cal-bounds org-habit-stats-calendar-bounds)
         (cal-start (car cal-bounds))
         (cal-end (cdr cal-bounds)))
    (save-excursion
      (goto-char cal-start)
      (delete-region cal-start cal-end)
      (org-habit-stats-insert-calendar org-habit-stats-current-habit-data))
    (set-buffer-modified-p nil)))

;;; Create org-habit-stats buffer
(defun org-habit-stats--insert-divider ()
  (insert (make-string (max 80 (window-width)) org-agenda-block-separator))
  (insert (make-string 1 ?\n)))

(defun org-habit-stats-insert-section-header (name)
  (insert (propertize name 'face 'org-habit-stats-section-name)
          "\n"))

(defun org-habit-stats-create-habit-buffer (habit-data habit-name habit-description)
  "Creates buffer displaying:
   - Calendar where days habit is done are marked
   - Graph of habit score or histogram of habit totals monthly/weekly
   - Various habit statistics"
  (setq org-habit-stats-current-habit-data habit-data)
  (let* ((buff (current-buffer))
         (completed-days (nth 4 habit-data))
         (history-rev (org-habit-stats-get-full-history-new-to-old completed-days))
         (history (reverse history-rev)))
    (switch-to-buffer (get-buffer-create org-habit-stats-buffer))
    (org-habit-stats-mode)
    (org-habit-stats-insert-habit-info habit-data habit-name habit-description)
    ;; write a function org-habit-stats--
    (org-habit-stats--insert-divider)
    (if (= 0 (length completed-days))
        (insert org-habit-stats-new-habit-message)
      (org-habit-stats-insert-section-header "Statistics")
      ;; (insert (make-string 1 ?\n))
      (org-habit-stats-insert-stats habit-data history history-rev)
      ;; (insert (make-string 1 ?\n))
      (org-habit-stats--insert-divider)
      (org-habit-stats-insert-section-header "Days Completed")
      (insert (make-string 1 ?\n))
;;; create calendar
      (org-habit-stats-make-calendar-buffer habit-data)
      (org-habit-stats-insert-calendar habit-data)
      (org-habit-stats--insert-divider)
      (org-habit-stats-insert-section-header "Graph")
;;; create graph
      (org-habit-stats-draw-graph history history-rev habit-data)
      )
    (set-buffer-modified-p nil)
    ))

;;; Old functions
;; (defun org-habit-stats-graph-completions-per-week (history)
;;   "Returns a pair of lists (months . counts)."
;;   (org-habit-stats-graph-count-per-category
;;    history
;;    (lambda (d) (let ((day (calendar-gregorian-from-absolute d))
;;                      (list (car d) (caddr d))))) ;; converts absolute date to list (month year)
;;    (lambda (m1 m2) (cond ((> (nth 1 m1) (nth 1 m2)) t)
;;                          ((= (nth 1 m1) (nth 1 m2)) (if (> (nth 0 m1) (nth 0 m2)) t nil))
;;                          (t nil)))
;;    (lambda (m) (rassoc (nth 0 m) parse-time-months))))




;; Create temp gnu plot file

;; Send gnu plot file to gnu plot and get graph in current buffer


;; Create org habit stats display buffer
(defun org-habit-stats-update-graph (history)
  (interactive)
  (let* ((gnuplot-buf (generate-new-buffer "*Org Habit Stats*"))
         (data-file (make-temp-file "org-habit-stats-score-monthly-data"))
         (output-file (make-temp-file "org-habit-stats-graph-output")))
    ;;insert
    (with-temp-file data-file
      (insert
       (let ((enum 0))
         (string-join
          (mapcar (lambda (x) (progn (setq enum (1+ enum)) (format "%d %d" enum x)))
                    (reverse (org-habit-stats-exp-smoothing-list--full history)))
          "\n")))
      (insert "\n"))
    (with-current-buffer gnuplot-buf
      (gnuplot-mode)
      (insert "set term dumb\n")
      (insert (format "set output '%s'\n" output-file))
      (insert (format "plot '%s' w dots\n" data-file))
      (save-window-excursion (gnuplot-send-buffer-to-gnuplot)))
    ;; read from output file
    (let ((graph-content (concat "#+BEGIN_SRC\n"
                                 (substring (org-file-contents output-file) 1)
                                 "\n#+END_SRC\n")))
            (org-habit-stats-insert-drawer org-habit-stats-graph-drawer-name graph-content))
      (delete-file data-file)
      (delete-file output-file)
      (kill-buffer gnuplot-buf)))



;;; Set stats as properties
(defun org-habit-stats-number-to-string-maybe (x)
  (cond ((integerp x) (format "%d" x))
        ((floatp x) (format "%.5f" x))
        (t x)))

(defun org-habit-stats-format-property-name (s)
  "Replace spaces with underscores in string S."
  (replace-regexp-in-string "[[:space:]]" "_" s))

(defun org-habit-stats-update-properties ()
  (interactive)
  (when (org-is-habit-p (point))
    (let* ((habit-data (org-habit-parse-todo (point)))
           (history-rev (org-habit-stats-get-full-history-new-to-old (nth 4 habit-data)))
           (history (reverse history-rev))
           (statresults (org-habit-stats-calculate-stats history history-rev habit-data)))
      (dolist (x statresults)
        (org-set-property (org-habit-stats-format-property-name (car x))
                          (org-habit-stats-number-to-string-maybe (cdr x)))))))

(add-hook 'org-after-todo-state-change-hook 'org-habit-stats-update-properties)
;; (advice-add 'org-todo :after (lambda (x) (org-habit-stats-update-score-2)))
(advice-add 'org-store-log-note :after 'org-habit-stats-update-properties)

;;; org-habit-stats commands
(defun org-habit-stats-view-habit-at-point ()
  (interactive)
  (let ((habit-name (org-element-property :raw-value (org-element-at-point)))
        (habit-data (org-habit-parse-todo (point)))
        (habit-description (org-entry-get (point) "DESCRIPTION")))
    (org-habit-stats-create-habit-buffer habit-data habit-name habit-description)))

(defun org-habit-stats-exit ()
  (interactive)
  (if (bufferp org-habit-stats-calendar-buffer)
      (kill-buffer org-habit-stats-calendar-buffer))
  (kill-buffer org-habit-stats-buffer))

(defun org-habit-stats-test-1-make-buffer ()
  (interactive)
  (org-habit-stats-make-calendar-buffer (org-habit-parse-todo (point))))
(defun org-habit-stats-test-make-buffer ()
  (interactive)
  (org-habit-stats-create-habit-buffer (org-habit-parse-todo (point))))


;;; Major mode
(defvar org-habit-stats-mode-map
  (let ((map (make-keymap)))
    (suppress-keymap map)
    (define-key map "q"   'org-habit-stats-exit)
    (define-key map "<"   'org-habit-stats-calendar-scroll-left)
    (define-key map ">"   'org-habit-stats-calendar-scroll-right)
    (define-key map "C-v"   'org-habit-stats-calendar-scroll-left-three-months)
    (define-key map "M-v"   'org-habit-stats-calendar-scroll-right-three-months)
    (define-key map "C-x ]"   'org-habit-stats-calendar-forward-year)
    (define-key map "C-x ["   'org-habit-stats-calendar-backward-year)
    (dolist (x org-habit-stats-graph-functions-alist)
      (let ((graph-func (car x))
            (graph-key (cadr x)))
        (define-key map (kbd graph-key) (lambda () (interactive) (org-habit-stats-switch-graph graph-func)))
        )
      )
    map)
  "Keymap for `org-habit-stats-mode'.")
(define-derived-mode org-habit-stats-mode special-mode "Org-Habit-Stats"
  "A major mode for the org-habit-stats window.
\\<org-habit-stats-mode-map>\\{org-habit-stats-mode-map}"
  (setq buffer-read-only nil
        buffer-undo-list t
        indent-tabs-mode nil)
  (make-local-variable 'current-org-habit)
  (make-local-variable 'org-habit-stats-stat-bounds)
  (make-local-variable 'org-habit-stats-calendar-bounds)
  (make-local-variable 'org-habit-stats-graph-bounds)
  (make-local-variable 'org-habit-stats-current-habit-data)
  (setq org-habit-stats-graph-current-offset 0)
  (if org-habit-stats-graph-default-func
        (setq org-habit-stats-graph-current-func org-habit-stats-graph-default-func)
    (setq org-habit-stats-graph-current-func (caar org-habit-stats-graph-functions-alist)))
  (setq org-habit-stats-graph-face-list (org-habit-stats-graph-create-faces))
  )


(provide 'org-habit-stats)
;;; org-habit-stats.el ends here
