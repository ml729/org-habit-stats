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

;; User can choose which stats to compute
(defvar org-habit-stats-list)
(defvar org-habit-stats-graph-drawer-name)

(setq org-habit-stats-graph-drawer-name "Graph")

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
          (setq diff (- diff 1))))
      )
    (if (= (length tasks) 1) (push 1 bin-hist))
    bin-hist))

;; Stats
(defun org-habit-stats-streak (history)
  (if (= (pop history) 1)
      (1+ (org-habit-stats-streak history))
    0))
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
(defun org-habit-stats-update-score-2 ()
  (interactive)
  (when (org-is-habit-p (point))
    (let ((history (org-habit-stats-dates-to-binary
                                 (nth 4 (org-habit-parse-todo (point))))))
      (mapcar (lambda (prop-func)
                ;; (print (cdr prop-func))
                (org-set-property (car prop-func)
                                  (number-to-string
                                   (funcall (cdr prop-func) history)))
                )
              '(("SCORE" . org-habit-stats-exp-smoothing-list-score)
                ("STREAK" . org-habit-stats-streak)
                ("MONTHLY" . org-habit-stats-30-day-total)
                ("YEARLY" . org-habit-stats-365-day-total)))
      )))

;; (add-hook 'org-after-todo-state-change-hook 'org-habit-stats-update-score)
;; (advice-add 'org-todo :after (lambda (x) (org-habit-stats-update-score-2)))
(advice-add 'org-store-log-note :after 'org-habit-stats-update-score-2)
;; Create temp gnu plot file

;; Send gnu plot file to gnu plot and get graph in current buffer


;; Create org habit stats display buffer
(defun org-habit-stats-create-buffer ()
  (interactive)
  (let* ((gnuplot-buf (generate-new-buffer "*Org Habit Stats*"))
         (data-file (make-temp-file "org-habit-stats-score-monthly-data"))
         (output-file (make-temp-file "org-habit-stats-graph-output")))
         ;;insert
         (with-temp-file data-file
                           (insert "1 1\n")
                           (insert "2 4\n")
                           (insert "3 9\n"))
         (with-current-buffer gnuplot-buf
           (gnuplot-mode)
           (insert "set term dumb\n")
           (insert (format "set output '%s'\n" output-file))
           (insert (format "plot '%s' w dots\n" data-file))
           (save-window-excursion (gnuplot-send-buffer-to-gnuplot))
           )
         ;; (switch-to-buffer gnuplot-buf)

    ))
(defun org-habit-stats-find-drawer-bounds (drawer-name)
  "Finds and returns the start and end positions of the first drawer of the
   current heading with name DRAWER-NAME."
  (let* ((heading-pos (org-back-to-heading-or-point-min t))
         (graph-beg-pos (progn
                          (search-forward-regexp (format ":%s:" drawer-name))
                          (match-beginning 0)))
         (graph-end-pos (search-forward ":END:"))
         (graph-beg-pos-verify (progn
                                 (search-forward-regexp ":words:")
                                 (match-beginning 0)))
         (heading-pos-verify (org-back-to-heading-or-point-min t)))
    (if (and (= heading-pos heading-pos-verify)
             (= graph-beg-pos graph-beg-pos-verify))
        '(graph-beg-pos graph-end-pos))))
(defun org-habit-stats-remove-drawer (drawer-name)
"Remove drawer with name DRAWER-NAME from task at point if it exists."
;; figure out how to search for whitespace
(let* ((heading-pos (org-back-to-heading-or-point-min t))
       (graph-end-pos (search-forward-regexp (format ":%s:[whitespace]:END:" drawer-name) nil nil))
       (graph-beg-pos (match-beginning 0))
       (new-heading-pos (org-back-to-heading-or-point-min t)))
  (if (and graph-end-pos (= (heading-pos) (new-heading-pos)))
      (delete-region (- graph-beg-pos (length drawer-name)) graph-end-pos))))
(defun org-habit-stats-insert-drawer (drawer-name drawer-contents)
  (org-with-wide-buffer
   ;;Set point to the position where the drawer should be inserted.
   ;; (org-habit-stats-remove-drawer (drawer-name))
   (if (or (not (featurep 'org-inlinetask)) (org-inlinetask-in-task-p))
       (org-back-to-heading-or-point-min t)
     (org-with-limited-levels (org-back-to-heading-or-point-min t)))
   (if (org-before-first-heading-p)
       (while (and (org-at-comment-p) (bolp)) (forward-line))
     (progn
       (forward-line)
       (when (looking-at-p org-planning-line-re) (forward-line))))

   (when (and (bolp) (> (point) (point-min))) (backward-char))
   (let ((begin (if (bobp) (point) (1+ (point))))
         (inhibit-read-only t))
     (unless (bobp) (insert "\n"))
     (insert (format ":%s:\n%s:END:" drawer-name drawer-contents))
     (org-flag-region (line-end-position 0) (point) t 'outline)
     (when (or (eobp) (= begin (point-min))) (insert "\n"))
     (org-indent-region begin (point))
     (org-hide-drawer-toggle)
     )))
(defun org-habit-stats-insert-drawer-3 ()
  (interactive)
  (org-habit-stats-insert-drawer "hi" "1\n1\n1\n1\n")
  )


(provide 'org-habit-stats)
;;; org-habit-stats.el ends here
