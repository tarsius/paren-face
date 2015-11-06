# A face dedicated to lisp parentheses

This library defines a face named `parenthesis` used just for
parentheses.  The intended purpose of this face is to make parentheses
less visible by dimming them.

We lispers probably don't need to be constantly made aware of the
existence of the parentheses.  Dimming them might be even more
useful for people new to lisp who have not yet learned to
subconsciously blend out the parentheses.

![how we see parentheses](parentheses.png)

To use the `parenthesis` face, turn on `global-paren-face-mode`.
The option `paren-face-modes` controls in what buffers the minor
mode `paren-face-mode` is turned on.

The parenthesis at or before point, as well as the parenthesis at
the other end of the s-expression should actually stand out, but
that is beyond the scope of the mode defined here.  Instead use one
of the modes dedicated to that, e.g. the builtin `show-paren-mode`.

While this face is intended to be used with Lisp modes, it also
works with other major modes, just add the mode to the value of
`paren-face-modes`.

By default only parentheses are dimmed, customize option
`paren-face-regexp` if you also want to dim brackets or braces.
If you want to use a different regexp in different major-modes,
then use a the mode hook to set the buffer local value.

## History

Dave Pearson's `parenface.el` implements the same basic idea.
Unfortunately that library doesn't use the appropriate Emacs
interfaces correctly, so I wrote this as a replacement.

