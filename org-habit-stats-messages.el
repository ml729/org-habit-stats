;;; org-habit-stats-messages.el -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2022 null
;;
;; Author: null
;; Maintainer: null <null>
;; Created: June 21, 2022
;; Modified: June 21, 2022
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

(defcustom org-habit-stats-show-messages t)

(defcustom org-habit-stats-stat-to-message-func-alist
  '()
  "Alist mapping stat functions to message functions. A message
function takes the output of a stat function and returns a
message or nil.
TODO merge with the original func alist.
add another parameter indicating if this stat should
be displayed")

(defcustom org-habit-stats-score-message-alist
  '(1 . "")
  )

(defcustom org-habit-stats-unstreak-message-alist
  '((1 . "Missing once is an accident.
 Missing twice is the start of a new habit. - James Clear")
    (4 . "")
    )
  )
(provide 'org-habit-stats-messages)
;;; org-habit-stats-messages.el ends here
