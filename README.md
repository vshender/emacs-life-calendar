# Life Calendar for Emacs

[![MELPA](https://melpa.org/packages/life-calendar-badge.svg)](https://melpa.org/#/life-calendar)

Display your life in weeks — a grid visualization showing all weeks of your life, with past weeks, the current week, and future weeks visually distinguished.  Mark significant moments with life chapters to see your journey at a glance.

Inspired by the "Your Life in Weeks" concept from [Wait But Why](https://waitbutwhy.com/2014/05/life-weeks.html).

![Life Calendar Screenshot](img/life-calendar.png)

## Installation

### MELPA

The package is available on [MELPA](https://melpa.org/#/life-calendar).

```
M-x package-install RET life-calendar RET
```

Or with `use-package`:

```elisp
(use-package life-calendar
  :ensure t)
```

### Manual

Clone this repository and add this to your init file:

```elisp
(add-to-list 'load-path "/path/to/emacs-life-calendar")
(require 'life-calendar)
```

### use-package (with vc)

Emacs 29+ has built-in support for installing packages from Git repositories:

```elisp
(use-package life-calendar
  :vc (:url "https://github.com/vshender/emacs-life-calendar")
  :ensure t)
```

Note: By default, `:vc` installs the last "release" — the most recent commit that changed the `Version:` header in the elisp file.  To install the latest commit instead, add `:rev :newest`:

```elisp
(use-package life-calendar
  :vc (:url "https://github.com/vshender/emacs-life-calendar"
       :rev :newest)
  :ensure t)
```

## Usage

```
M-x life-calendar
```

On first use, you will be prompted to enter your birthday (in YYYY-MM-DD format).  The birthday is saved automatically and remembered for future sessions.

### Data Storage

Your birthday and life chapters are saved as Emacs custom variables (`life-calendar-birthday` and `life-calendar-chapters`).

By default, these are stored in your `custom-file` (often `~/.emacs.d/custom.el` or within your init file).

#### Custom Data File

You can specify a dedicated file for storing life calendar data by setting `life-calendar-data-file`:

```elisp
(setq life-calendar-data-file "~/.emacs.d/life-calendar-data.el")
```

When this variable is set, your birthday and life chapters will be saved to the specified file instead of the `custom-file`. This keeps your life calendar data separate from other Emacs customizations.

To change your birthday or edit life chapters manually:
- Use `M-x customize-variable RET life-calendar-birthday RET`, or
- Edit the saved values directly in your custom file (or `life-calendar-data-file` if configured)

### Keybindings

#### Navigation

| Key | Action |
|-----|--------|
| `f`, `b`, `C-f`, `C-b`, arrows | Move by week |
| `n`, `p`, `C-n`, `C-p`, up/down | Move by row (multiple years in multi-column view) |
| `]`, `[`, `M-f`, `M-b` | Move by year |
| `M-<`, `M->` | Jump to first/last week |
| `C-a`, `C-e`, Home, End | Beginning/end of row |
| `.` | Jump to current week |

#### Life Chapters

Mark significant dates in your life (birthdays, graduations, jobs, etc.):

| Key | Action |
|-----|--------|
| `+` | Add a life chapter |
| `-` | Remove a life chapter from current week |

Chapters are displayed with a distinct character (`★` by default) and shown in the echo area when you navigate to them.

#### Other

| Key | Action |
|-----|--------|
| `q` | Close the calendar |
| `g` | Refresh the display |
| `?` | Show help (keybindings) |

## Customization

Run `M-x customize-group RET life-calendar RET` to customize all options, or set them in your init file:

### Variables

```elisp
;; Your birthday (set automatically on first use, or set manually).
(setq life-calendar-birthday "1990-05-15")

;; File to store life calendar data (default: nil, uses custom-file).
;; Set this to a specific file to keep data separate from custom-file.
(setq life-calendar-data-file "~/.emacs.d/life-calendar-data.el")

;; Number of years to display (default: 90).
(setq life-calendar-years 100)

;; When to count a new week (default: 'birthday).
;; 'birthday = day of week you were born (each week starts on that day)
;; Or use a day symbol: 'sunday, 'monday, 'tuesday, etc.
(setq life-calendar-week-start-day 'monday)

;; Number of columns to display (default: nil).
;; nil = auto-fit as many columns as the window allows
;; Or set to a specific number for fixed columns.
(setq life-calendar-columns nil)   ; auto-fit to window
(setq life-calendar-columns 2)     ; always show 2 columns

;; Characters for different week states.
(setq life-calendar-past-char "●")
(setq life-calendar-current-char "●")
(setq life-calendar-future-char "○")
(setq life-calendar-chapter-char "★")
```

### Faces

The package uses faces that inherit from built-in Emacs faces, so they automatically adapt to your theme:

| Face | Inherits from | Description |
|------|---------------|-------------|
| `life-calendar-past-face` | `shadow` | Past weeks |
| `life-calendar-current-face` | `warning` | Current week |
| `life-calendar-future-face` | `default` | Future weeks |
| `life-calendar-chapter-face` | `success` | Weeks with life chapters |
| `life-calendar-age-face` | `font-lock-comment-face` | Age labels |
| `life-calendar-header-face` | `bold` | Header text |

You can still customize them if needed:

```elisp
(custom-set-faces
 '(life-calendar-current-face ((t :inherit success :weight bold :inverse-video t))))
```

## How Weeks Are Counted

The life calendar counts **completed weeks**.  A week is marked as lived once its last day has passed.  Think of it as filling in a square at the start of each new week, representing the week you just finished.

### Week Boundaries

By default, your weeks start on the same day of the week as your birthday.  If you were born on a Wednesday, each new week of your life begins on Wednesday.  This means you complete a week of life every Wednesday, and that's when the calendar fills in a new square.

However, some people prefer to align their life weeks with calendar weeks.  If you think of weeks as starting on Monday (common in Europe and the ISO standard) or Sunday (common in the US), you might want your life calendar to follow the same convention.  Set `life-calendar-week-start-day` to `'monday` or `'sunday` to match your mental model of when a week begins.

### Why Some Years Have 53 Weeks

A year of life runs from one birthday to the next.  A week is counted if it **completes** within that interval — specifically, if its last day falls on or after your birthday but before your next birthday.

Since a year has about 365.25 days and a week has 7 days, most years contain 52 complete weeks (364 days) with one or two extra days.  These extra days accumulate, and occasionally a 53rd week fits entirely within a year.

The exact count depends on the alignment between your birthday's day-of-week and the calendar.  Over a lifetime, the distribution averages out, but individual years may vary.

## Requirements

- Emacs 27.1+

## License

ISC
