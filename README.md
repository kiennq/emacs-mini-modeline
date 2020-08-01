[![MELPA](https://melpa.org/packages/mini-modeline-badge.svg)](https://melpa.org/#/mini-modeline)
[![MELPA Stable](https://stable.melpa.org/packages/mini-modeline-badge.svg)](https://stable.melpa.org/#/mini-modeline)
[![License: GPL v3](https://img.shields.io/badge/License-GPL%20v3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)

mini-modeline
============

Allows to display mode line information in minibuffer.

![mini-modeline in action](screenshots/mini-modeline.gif)

# Install
## Using [Melpa](https://melpa.org/#/mini-modeline)
## Using [`Quelpa`](https://github.com/quelpa/quelpa)

Example:

``` elisp
;; I'm activating mini-modeline after smart-mode-line
(use-package mini-modeline
  :quelpa (mini-modeline :repo "kiennq/emacs-mini-modeline" :fetcher github)
  :after smart-mode-line
  :config
  (mini-modeline-mode t))
```

## Manually
Just clone this repo and put it in your `load-path`.

# Usage
`mini-modeline` comes with a global minor-mode `mini-modeline-mode`.
You can toggle it by `M-x mini-modeline-mode`.

## Features
1. Display mode line in minibuffer.
By default you can set anything that compatible with `mode-line-format`, even your current mode line and it will be nicely kept in minibuffer.

2. Support left/right align display.
You can set left side and right side part of `mini-modeline` separately.
Just assign a `mode-line-format` compatible list to either `mini-modeline-l-format` or `mini-modeline-r-format`, and it will be left/right aligned accordingly.

3. Support multiple lines display in `minibuffer`

## Important variables
You can customize those variable for better experiences.

1. `mini-modeline-l-format`
   Left part of mini-modeline, same format with `mode-line-format`.

2. `mini-modeline-r-format`
   Right part of mini-modeline, same format with `mode-line-format`.

3. `mini-modeline-face-attr`
    Plist of face attribute/value pair for mini-modeline.

4. `mini-modeline-enhance-visual`
   Enhance minibuffer and window's visibility. This will enable `window-divider-mode` since without the mode line, two continuous windows are nearly indistinguishable.

5. `mini-modeline-echo-duration`
   Duration to keep display echo. `mini-modeline` will display the message which has been echoed to echo area as part of mode line. Those echo will be automatically clear after this interval.
   Check out the gif to see it in action.

6. `mini-modeline-update-interval`
   The minimum interval to update `mini-modeline`.
   If you found `mini-modeline` is being updated to frequently, you can customize this variable.

7. `mini-modeline-frame`
   Frame to display mini-modeline on. `nil` means current selected frame.

8. `mini-modeline-truncate-p`
   Truncates the `mini-modeline` to fit in one line.

9. `mini-modeline-right-padding`
   Padding to use in the right side. Set this to the minimal value that doesn't cause truncation.

10. `mini-modeline-display-gui-line`
    Display thin line at the bottom of each window.

# Why?

Echo area or `minibuffer` which reside in it is a big waste most of the time.
It's empty (mostly) and unnecessarily consume an additional line of your editor.
Look at `VsCode`, it has only one line at the bottom to display the status, even more, its message can even be nicely integrated into the status line.
**That's the sleek UI that I want for my editor.**

At first, I've tried to hide `minibuffer` and only raise it when needed.
Those efforts not bode well, even when the `minibuffer` is displayed in separate frame, it still has title bar, which is ugly since the `minibuffer` is just one line.
Trying to display `minibuffer` in mini frame is not working well too. Well, I've never succeeded at that.

Then, finally another idea comes, if I cannot hide the `minibuffer`, I can just hide the `mode-line`, and display its information in `minibuffer` instead.
Hence that why this package is born.
