mini-modeline
============

Allows to display mode line information in minibuffer.

# Why?

Echo area or `minibuffer` which reside in it is a big waste most of the time.
It's empty (mostly) and unnecessarily consume an additional line of your editor.
Look at `VsCode`, it has only one line at the bottom to display the status, even more, its message can even be nicely integrated into the status line.
**That's the sleak UI that I want for my editor.**

At first, I've tried to hide `minibuffer` and only raise it when needed.
Those efforts not bode well, even when the `minibuffer` is displayed in separate frame, it stil has title bar, which is ugly since the `minibuffer` is just one line.
Trying to display `minibuffer` in mini frame is not working well too. Well, I've never succeeded at that.

Then, finally another idea comes, if I cannot hide the `minibuffer`, I can just hide the `mode-line`, and display its information in `minibuffer` instead.
Hence that why this package is born.
