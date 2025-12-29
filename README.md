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

## Requirements

- Emacs 24.1+
- org-mode 8.0+ (for date picker)

## License

ISC
