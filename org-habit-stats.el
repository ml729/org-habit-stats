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
;; (defun org-habit-stats-create-buffer ()
;;   (interactive)
;;   (switch-to-buffer (generate-new-buffer "*Org Habit Stats*"))
;;   (let* ((data-file (make-temp-file "org-habit-stats-score-monthly-data"))
;;          ;;insert
;;          (with-temp-buffer (data-file)
;;                            (insert "hi 1")
;;                            (insert "bye 2")
;;                            (insert " 2")
;;          )

;;     )
;;   )

(provide 'org-habit-stats)
;;; org-habit-stats.el ends here
