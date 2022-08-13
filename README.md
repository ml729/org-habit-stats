
# org-habit-stats.el

View statistics, a calendar, and bar graphs of your habits in Emacs org-mode.

# Screenshots
![Screenshot](screenshot.png)

# Dependencies
- Emacs 25.1

# Installation
This package is not yet available on (M)ELPA.

## Manual Installation
Clone this repo to `~/.emacs.d/site-lisp/`.
Then add this to your config:
``` emacs-lisp
(load "org-habit-stats")
```

## Doom Emacs:
``` emacs-lisp
(package! org-habit-stats :recipe (:host github :repo "ml729/org-habit-stats/"))
```

# Usage
Use `org-habit-stats-view-habit-at-point` inside org-mode files to view the stats of a habit.
Use `org-habit-stats-view-habit-at-point-agenda` inside org-agenda buffers.
Bind these to keys of your choosing.
``` emacs-lisp
(define-key org-agenda-mode-map (kbd "C-c h") 'org-habit-stats-view-habit-at-point)
(define-key org-agenda-mode-map (kbd "H") 'org-habit-stats-view-habit-at-point-agenda)
```

The built-in keybindings for the org-habit-stats buffer are
| Key   | Command                                            |
|-------+----------------------------------------------------|
| q     | org-habit-stats-exit                               |
| <     | org-habit-stats-calendar-scroll-left               |
| >     | org-habit-stats-calendar-scroll-left               |
| C-v   | 'org-habit-stats-calendar-scroll-left-three-months |
| M-v   | org-habit-stats-calendar-scroll-right-three-months |
| C-x ] | org-habit-stats-calendar-forward-year              |
| C-x [ | org-habit-stats-calendar-backward-year             |
| [     | org-habit-stats-scroll-graph-left                  |
| ]     | org-habit-stats-scroll-graph-right                 |
| }     | org-habit-stats-scroll-graph-left-big              |
| {     | org-habit-stats-scroll-graph-right-big             |
| gm    | Switch to monthly graph view                       |
| gw    | Switch to weekly graph view                        |
| gd    | Switch to weekday graph view                       |
| gs    | Switch to daily habit strength graph view          |

To automatically add habit stats as properties of the habit item, use the following:
``` emacs-lisp
(add-hook 'org-after-todo-state-change-hook 'org-habit-stats-update-properties)
```

# Customization
TODO
## Adding your own functions
You can add your own statistics, graph, and message functions.
All three types of functions must have the same function signature:
``` emacs-lisp
(defun your-new-function (history history-rev habit-data))
```
- `history` contains a list of pairs (date . completed) where date is represented as the number of days since December 31, 1 BC (as is used by many org-mode functions) and completed is 1 if the habit was completed that day, and 0 otherwise.
- `history-rev` is the reverse of `history`.
- `habit-data` is the result of running `org-habit-parse-todo` on a habit. From the docstring for `org-habit-parse-todo`:

### Adding statistics functions
Statistics functions must return a number or string.
### Adding message functions
Message functions must return a string or nil. If it returns nil, no message will be displayed. This allows messages to be used for rewards, encouraging messages, etc.
### Adding graph functions
Graph functions must return a pair `(LABELS . VALUES)`, where LABELS is a list of strings to be used as bar labels and VALUES is a list of numbers to be used as the bar sizes.

## The Habit Strength score
The Habit Strength score $S_n$ uses a modified form of exponential smoothing (inspired by Loop Habit Tracker's score).
The formula is

$$S_0 = 0$$

$$S_{n} = \begin{cases}
(1-\alpha)S_{n-1} + \alpha, & \text{if the habit was completed on Day n} \\
(1-\beta)S_{n-1}, & \text{otherwise}
\end{cases}$$

where $\alpha, \beta \in [0,1]$.

$\alpha$ determines (roughly) how much each successful completion contributes to the score. At the extremes, $\alpha=0$ means completing the habit has no effect on the score and $\alpha=1$ means completing the habit gives the maximum score of $100$.

$\beta$ is exactly how much the score decreases (in percent) for each miss. For instance, $\beta = 0.2$ means the score decreases by $20\%$ each day you miss the habit. At the extremes, $\beta = 0$ means misses don't affect the score at all, and $\beta=1$ means misses resets the score to $0$.

By default, $\alpha = 0.052$ is calibrated so that $66$ consecutive completions reaches a habit strength of $97$ (based on the idea that it takes 66 days to form a habit).

To calibrate it such that $N$ consecutive completions reaches a habit strength of $S$, use the following formula:

$$\alpha = 1 - \sqrt[n]{1 - \frac{S}{100}}$$

# Contribution
I might try to get this package on ELPA, so any significant contributions (at least 15 LOC) requires copyright assignment to the FSF.

# Acknowledgements
Powered by the wonderful built-in packages calendar.el and chart.el.

Inspired by the awesome free and open source app [Loop Habit Tracker](https://github.com/iSoron/uhabits).
