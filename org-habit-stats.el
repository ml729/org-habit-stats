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
;; Package-Requires: ((emacs "24.3"))
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

;; User can choose which stats to compute
(defvar org-habit-stats-list)
;; (defvar org-habit-stats-graph-drawer-name)

(defcustom org-habit-stats-insert-graph-in-file t
  "Whether or not to insert ascii graph of habit scores in file."
  :group 'org-habit-stats
  :type 'boolean)

(defcustom org-habit-stats-graph-drawer-name "GRAPH"
  "Name of drawer that stores habit graph."
  :group 'org-habit-stats
  :type 'string)

(defconst org-habit-stats-buffer "*Org-Habit-Stats*"
  "Name of the buffer used for displaying stats, calendar, and graphs.")

(defconst org-habit-stats-calendar-buffer "*Org-Habit-Stats Calendar*"
  "Name of the buffer used for the calendar.")

(defvar org-habit-stats-chart-face-color-list '("red" "green" "blue"
                                "cyan" "yellow" "purple")
  "Colors to use for bars of habit bar graph. The original value of
chart-face-color-list is unaffected.")

(defvar org-habit-stats-current-habit-data nil
  "Output from org-habit-parse-todo of currently viewed habit.")

(defvar org-habit-stats-graph-width 70
  "Width of x-axis of graph (in columns), not including origin.")

(defvar org-habit-stats-graph-height 10
  "Height of y-axis of graph (in line numbers), not including origin.")

(defvar org-habit-stats-graph-left-margin 5
  "Number of columns to the left of y-axis.")

(defvar org-habit-stats-graph-current-offset 0
  "How many bars to shift left when the bar graph is truncated.")

(defvar org-habit-stats-view-order '(statistics graph calendar)
  "Output from org-habit-parse-todo of currently viewed habit.")

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
  '((t (:background "#006800")))
  "Face for days in the calendar where the habit was completed."
  :group 'org-habit-stats)

(defun org-habit-stats-dates-to-binary (tasks)
  "Return binary version of TASKS from newest to oldest, where
TASKS is a list of all the past dates this habit was marked closed.
Assumes the dates logged for the habit are in order, newest to oldest."
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

;; Stats
(defun org-habit-stats-streak (history)
  "Returns the current streak."
  (if (= (pop history) 1)
      (1+ (org-habit-stats-streak history))
    0))
(defun org-habit-stats--record-streak-full (history)
  "Returns (a b) where a is the record streak,
   b is the day the record streak occurred."
  (let ((record-streak 0)
        (record-day 0)
        (curr-streak 0)
        (curr-streak-start 0)
        (curr-day 0))
    (while history
      (if (= (pop history) 1)
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

(defun org-habit-stats-record-streak-format (history)
  (let* ((record-data (org-habit-stats--record-streak-full history))
         (record-streak (car record-data))
         (record-day (cdr record-data)))
    (concat (number-to-string record-streak)
            ", achieved on "
            (single-whitespace-only (org-agenda-format-date-aligned record-day)))))

(defun org-habit-stats-N-day-total (history N)
  (if (and (> N 0) history)
      (if (= (pop history) 1)
          (1+ (org-habit-stats-N-day-total history (1- N)))
        (org-habit-stats-N-day-total history (1- N)))
    0))
(defun org-habit-stats-N-day-percentage (history N)
  (/ (org-habit-stats-N-day-total history N) (float N)))

(defun org-habit-stats-30-day-total (history)
  (org-habit-stats-N-day-percentage history 30))

(defun org-habit-stats-365-day-total (history)
  (org-habit-stats-N-day-percentage history 365))

(defun org-habit-stats-exp-smoothing-list--full (history)
  "Returns score for a binary list HISTORY,
   computed via exponential smoothing. (Inspired by the open source Loop Habit Tracker app's score.)"
  (let* ((history (reverse history))
         (scores '(0))
         (freq 1.0)
         (alpha (expt 0.5 (/ (sqrt freq) 13))))
    (while history
      (push (+ (* alpha (nth 0 scores))
               (* (- 1 alpha) (pop history))) scores))
    (setq scores (mapcar (lambda (x) (* 100 x)) scores))
    scores))
(defun org-habit-stats-exp-smoothing-list-score (history)
  (nth 0 (org-habit-stats-exp-smoothing-list--full history)))

(defun org-habit-stats-update-score ()
  (interactive)
  (when (org-is-habit-p (point))
    (let ((history (org-habit-stats-dates-to-binary
                                 (nth 4 (org-habit-parse-todo (point))))))
    (org-set-property "SCORE"
                      (number-to-string
                       (org-habit-stats-exp-smoothing-list-score
                        history)))
    (org-set-property "STREAK"
                      (number-to-string
                       (org-habit-stats-streak
                        history)))
    (org-set-property "MONTHLY"
                      (number-to-string
                       (org-habit-stats-30-day-total
                        history)))
    (org-set-property "YEARLY"
                      (number-to-string
                       (org-habit-stats-365-day-total
                        history))))))

(defun number-to-string-maybe (x)
  (cond ((integerp x) (format "%d" x))
        ((floatp x) (format "%.5f" x))
        (t x)))

(defun org-habit-stats-update-score-2 ()
  "Update score, streak, monthly, and yearly properties of a habit task
   with the corresponding statistics, and update graph of habit score."
  (interactive)
  (when (org-is-habit-p (point))
    (let ((history (org-habit-stats-dates-to-binary
                                 (nth 4 (org-habit-parse-todo (point))))))
      (mapcar (lambda (prop-func)
                ;; (print (cdr prop-func))
                (org-set-property (car prop-func)
                                  (number-to-string-maybe
                                   (funcall (cdr prop-func) history))))
              '(("SCORE" . org-habit-stats-exp-smoothing-list-score)
                ("CURRENT_STREAK" . org-habit-stats-streak)
                ("MONTHLY" . org-habit-stats-30-day-total)
                ("YEARLY" . org-habit-stats-365-day-total)
                ("RECORD_STREAK" . org-habit-stats-record-streak-format)))
      (org-habit-stats-update-graph history))))

;; (add-hook 'org-after-todo-state-change-hook 'org-habit-stats-update-score)
;; (advice-add 'org-todo :after (lambda (x) (org-habit-stats-update-score-2)))
(advice-add 'org-store-log-note :after 'org-habit-stats-update-score-2)
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

(defun org-habit-stats--find-drawer-bounds (drawer-name)
  "Finds and returns the start and end positions of the first drawer of the
   current heading with name DRAWER-NAME."
  (save-excursion
  (let* ((heading-pos (progn (org-back-to-heading) (point)))
         (graph-beg-pos (progn
                          (search-forward-regexp (format ":%s:" drawer-name) nil t)
                          (match-beginning 0)))
         (graph-end-pos (search-forward ":END:"))
         (graph-beg-pos-verify (progn
                                 (search-backward-regexp ":GRAPH:" nil t)
                                 (match-beginning 0)))
         (heading-pos-verify (progn (org-back-to-heading) (point))))
    (when (and heading-pos heading-pos-verify
               graph-beg-pos graph-beg-pos-verify graph-end-pos)
      (when (and (= heading-pos heading-pos-verify)
                 (= graph-beg-pos graph-beg-pos-verify))
        (cons graph-beg-pos graph-end-pos))))))
(defun org-habit-stats--remove-drawer (drawer-name)
  (let ((bounds (org-habit-stats--find-drawer-bounds drawer-name)))
    (when bounds
      (delete-region (car bounds) (cdr bounds))
      t)))

(defun org-habit-stats--skip-property-drawer ()
  (let* ((property-pos (search-forward-regexp ":PROPERTIES:" nil t)))
         (when property-pos
           (search-forward-regexp ":END:")
           (forward-line))))

(defun org-habit-stats-insert-drawer (drawer-name drawer-contents)
  "Inserts drawer DRAWER-NAME with contents DRAWER-CONTENTS.
   It is placed after the property drawer if it exists."
  (org-with-wide-buffer
   (org-habit-stats--remove-drawer drawer-name)
   (if (or (not (featurep 'org-inlinetask)) (org-inlinetask-in-task-p))
       (org-back-to-heading-or-point-min t)
     (org-with-limited-levels (org-back-to-heading-or-point-min t)))
   (if (org-before-first-heading-p)
       (while (and (org-at-comment-p) (bolp)) (forward-line))
     (progn
       (forward-line)
       (when (looking-at-p org-planning-line-re) (forward-line))
       (org-habit-stats--skip-property-drawer)))
   (when (and (bolp) (> (point) (point-min))) (backward-char))
   (let ((begin (if (bobp) (point) (1+ (point))))
         (inhibit-read-only t))
     (unless (bobp) (insert "\n"))
     (insert (format ":%s:\n%s:END:" drawer-name drawer-contents))
     (org-flag-region (line-end-position 0) (point) t 'outline)
     (when (or (eobp) (= begin (point-min))) (insert "\n"))
     (org-indent-region begin (point))
     (org-hide-drawer-toggle))))

;; mode
(define-derived-mode org-habit-stats-mode special-mode "Org-Habit-Stats"
  "A major mode for the org-habit-stats window.
\\<org-habit-stats-mode-map>\\{org-habit-stats-mode-map}"
  (setq buffer-read-only t
        buffer-undo-list t
        indent-tabs-mode nil)
  (make-local-variable 'current-org-habit))
(defvar org-habit-stats-mode-map
  (let ((map (make-keymap)))
    (suppress-keymap map)
    (define-key map "q"   'org-habit-stats-exit)
    )
  "Keymap for `org-habit-stats-mode'.")
;; creating the habit buffer
(defun org-habit-stats--insert-divider ()
    (insert (make-string (window-width) org-agenda-block-separator))
    (insert (make-string 1 ?\n)))
(defun org-habit-stats-create-habit-buffer (habit-info)
  "Creates buffer displaying:
   - Calendar where days habit is done are marked
   - Graph of habit score or histogram of habit totals monthly/weekly
   - Various habit statistics"
  (let ((buff (current-buffer)))
    (switch-to-buffer (get-buffer-create org-habit-stats-buffer))
    (org-habit-stats-mode)
    ;;; inject habit data
    (insert
     (propertize "Run a mile\n" 'face 'bold))
    (insert "Score: 5\tCurrent Streak: 25\t Total Completions: 50\n")
    (insert (make-string 1 ?\n))
    (org-habit-stats--insert-divider)
    (insert "Days Completed")
    (insert (make-string 2 ?\n))
    ;;; create calendar
    (org-habit-stats-make-calendar-buffer habit-info)
    (let ((cal-offset-for-overlay (1- (point))))
      (insert (org-habit-stats-get-calendar-contents))
      (org-habit-stats-apply-overlays (org-habit-stats-get-calendar-overlays)
                                      cal-offset-for-overlay
                                      (current-buffer)))
    (insert (make-string 2 ?\n))
    (org-habit-stats--insert-divider)
    (insert "Graph")
    (insert (make-string 2 ?\n))
    ;;; create graph
    (org-habit-stats-chart-bar-quickie-extended
     'vertical
     "Monthly Completions"
     '("Jan" "Feb" "Mar" "Apr" "Jun" "asdf" "asdf" "asdf" "asdf" "asdf")
     "Month"
     '(20 10 30 28 27 26 26 26 26 26)
     "Completions"
     org-habit-stats-graph-width
     org-habit-stats-graph-height
     (line-number-at-pos)
     org-habit-stats-graph-left-margin
     'org-habit-stats-graph-title
     'org-habit-stats-graph-name
     'org-habit-stats-graph-label)))

;;; Graph helpers

(defclass org-habit-stats-chart-sequence ()
  ((data :initarg :data
         :initform nil)
   (name :initarg :name
         :initform "Data")
   )
  "Class used for all data in different charts, originally defined in charts.el as 'chart-sequence'.
   Some earlier versions of Emacs the name of this class contains a typo ('chart-sequece') so we redefine it here under a new name.")
;; (setq test-instance (make-instance 'chart-sequece))
;; (oset test-instance lmao 1)
;; (oref test-instance lmao)
(defun org-habit-stats-chart-draw-title (c &optional align-left)
  "Draw a title of chart. By default, centered. If ALIGN-LEFT, align-left."
  (if align-left
      (chart-display-label (oref c title) 'horizontal
                           (- (oref c x-margin) 2)
                           (oref c y-margin) (+ (length (oref c title))
                                                (oref c y-margin))
                           (oref c title-face))
    (chart-display-label (oref c title) 'horizontal
                         (- (oref c x-margin) 2)
                         (oref c y-margin) (+ (oref c x-width)
                                              (oref c y-margin))
                         (oref c title-face))))

(defun org-habit-stats-chart-draw (c &optional buff)
  "Start drawing a chart object C in optional BUFF.
Begins at line LINE."
  (with-silent-modifications
    (save-excursion
      (if buff (set-buffer buff))
      ;; (goto-line line)
      (insert (make-string (window-height (selected-window)) ?\n))
      ;; (insert (make-string ((oref c 100) ?\n)))
      ;; Start by displaying the axis
      (chart-draw-axis c)
      ;; Display title
      (org-habit-stats-chart-draw-title c)
      ;; Display data
      ;; (message "Rendering chart...")
      (sit-for 0)
      (chart-draw-data c)
      ;; (message "Rendering chart...done")
      )))
(defun org-habit-stats-chart-bar-quickie-extended (dir title namelst nametitle numlst numtitle
                                 &optional max sort-pred width height topmargin leftmargin titleface nameface labelface)
  "Modification of function chart-bar-quickie to support custom graph
dimensions, margins, and faces. Inserts graph into current buffer,
with width WIDTH, height HEIGHT, vertical margin of TOPMARGIN from current
line, horizontal margin of LEFTMARGIN, face TITLEFACE for title, face
NAMEFACE for axis names, face LABELFACE for labels for values.
Original Docstring:
Wash over the complex EIEIO stuff and create a nice bar chart.
Create it going in direction DIR [`horizontal' `vertical'] with TITLE
using a name sequence NAMELST labeled NAMETITLE with values NUMLST
labeled NUMTITLE.
Optional arguments:
Set the chart's max element display to MAX, and sort lists with
SORT-PRED if desired."
  (interactive)
  (let ((nc (make-instance 'chart-bar
                           :title title
                           :title-face titleface
                           :x-width width
                           :y-width height
                           :x-margin topmargin
                           :y-margin leftmargin
                           :key-label "8-m"  ; This is a text key pic
                           :direction dir
                           ))
        (iv (eq dir 'vertical)))
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
    (if (integerp max) (chart-trim nc max))
    (org-habit-stats-chart-draw nc)))

;;; Calendar helpers
;; create calendar buffer, inject text at top, mark custom dates, set so curr month on the right first
(defun org-habit-stats-make-calendar-buffer (habit-info)
  ;; (interactive "P")
  ;; (with-current-buffer
  (with-current-buffer
   (get-buffer-create org-habit-stats-calendar-buffer)
  (calendar-mode)
  (let* ((date (calendar-current-date))
         (month (calendar-extract-month date))
         (year (calendar-extract-year date))
         (current-month-align-right-offset 1)
         (completed-dates (nth 4 habit-info)))
    (calendar-increment-month month year (- current-month-align-right-offset))
    (calendar-generate-window month year)
    (org-habit-stats-calendar-mark-habits habit-info))
  (run-hooks 'calendar-initial-window-hook)))
(defun org-habit-stats-calendar-mark-habits (habit-info)
  (let ((completed-dates (nth 4 habit-info))
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
                   buffer
                   )))


(defun org-habit-stats-test-1-make-buffer ()
  (interactive)
  (org-habit-stats-make-calendar-buffer (org-habit-parse-todo (point))))
(defun org-habit-stats-test-make-buffer ()
  (interactive)
  (org-habit-stats-create-habit-buffer (org-habit-parse-todo (point))))


;; create a calender buffer with a custom name, don't open it



;; create a new mode

;; insert the calender buffer's contents into the current buffer



(provide 'org-habit-stats)
;;; org-habit-stats.el ends here
