# Life Calendar for Emacs

Display your life in weeks — a grid visualization showing all weeks of your life, with past weeks, the current week, and future weeks visually distinguished.

Inspired by the "Your Life in Weeks" concept from [Wait But Why](https://waitbutwhy.com/2014/05/life-weeks.html).

## Installation

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

### use-package (with straight.el)

```elisp
(use-package life-calendar
  :straight (:host github :repo "vshender/emacs-life-calendar"))
```

### Doom Emacs

Add to `packages.el`:

```elisp
(package! life-calendar
  :recipe (:host github :repo "vshender/emacs-life-calendar"))
```

## Usage

```
M-x life-calendar
```

On first use, you will be prompted to enter your birthday using org-mode's date picker.  The birthday is saved automatically and remembered for future sessions.

### Keybindings

| Key | Action |
|-----|--------|
| `q` | Close the calendar |
| `g` | Refresh the display |

## Customization

Run `M-x customize-group RET life-calendar RET` to customize all options, or set them in your init file:

### Variables

```elisp
;; Your birthday (set automatically on first use, or set manually).
(setq life-calendar-birthday "1990-05-15")

;; Number of years to display (default: 90).
(setq life-calendar-years 100)

;; When to count a new week (default: 'birthday).
;; 'birthday = day of week you were born (each week starts on that day)
;; Or use a day symbol: 'sunday, 'monday, 'tuesday, etc.
(setq life-calendar-week-start-day 'monday)

;; Characters for different week states.
(setq life-calendar-past-char "●")
(setq life-calendar-current-char "●")
(setq life-calendar-future-char "○")
```

### Faces

The package uses faces that inherit from built-in Emacs faces, so they automatically adapt to your theme:

| Face | Inherits from | Description |
|------|---------------|-------------|
| `life-calendar-past-face` | `shadow` | Past weeks |
| `life-calendar-current-face` | `warning` | Current week |
| `life-calendar-future-face` | `default` | Future weeks |
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
- org-mode 8.0+ (for date picker)

## License

ISC
