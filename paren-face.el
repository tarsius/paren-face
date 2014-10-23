;;; paren-face.el --- a face for parentheses in lisp modes

;; Copyright (C) 2013  Jonas Bernoulli

;; Maintainer: Jonas Bernoulli <jonas@bernoul.li>
;; Homepage: http://github.com/tarsius/paren-face

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This library defines a face named `parenthesis' used just for
;; parentheses and that only in lisp buffers.  The intended purpose
;; of this face is to make parentheses less visible by dimming them.

;; We lispers probably don't need to be constantly made aware of the
;; existence of the parentheses.  Dimming them might be even more
;; useful for people new to lisp who have not yet learned to
;; subconsciously blend out the parentheses.

;; To use the `parenthesis' face, turn on `global-paren-face-mode'.
;; The option `paren-face-modes' controls in what buffers the minor
;; mode `paren-face-mode' is turned on.

;; The parenthesis at or before point, as well as the parenthesis at
;; the other end of the s-expression should actually stand out, but
;; that is beyond the scope of the mode defined here.  Instead use one
;; of the modes dedicated to that, e.g. the builtin `show-paren-mode'.

;;; History:

;; Dave Pearson's `parenface.el' implements the same basic idea.
;; Unfortunately that library doesn't use the appropriate Emacs
;; interfaces correctly, so I wrote this as a replacement.

;;; Code:

(defgroup paren-face nil
  "Face for parentheses in lisp modes."
  :group 'font-lock-extra-types
  :group 'faces)

(defface parenthesis '((t (:foreground "DimGray")))
  "Face for parentheses in lisp modes.
This face is only used if `paren-face-mode' is turned on.
See `global-paren-face-mode' for an easy way to do so."
  :group 'paren-face)

(defcustom paren-face-modes
  '(lisp-mode emacs-lisp-mode lisp-interaction-mode ielm-mode
    scheme-mode slime-repl-mode clojure-mode nrepl-mode)
  "Major modes in which `paren-face-mode' should be turned on.
When `global-paren-face-mode' is turned on, the buffer-local mode
is turned on in all buffers whose major mode is or derives from
one of the modes listed here."
  :group 'paren-face)

(defcustom paren-face-regexp "[()]"
  "Regular expression to match parentheses."
  :group 'paren-face)
;;(make-variable-buffer-local 'paren-face-regexp)

(defvar paren-face-mode-lighter "")

;;;###autoload
(define-minor-mode paren-face-mode
  "Use a dedicated face just for parentheses."
  :lighter paren-face-mode-lighter
  (let ((keywords `((,paren-face-regexp 0 'parenthesis))))
    (if paren-face-mode
        (font-lock-add-keywords  nil keywords)
      (font-lock-remove-keywords nil keywords))
    (when (called-interactively-p 'any)
      (font-lock-fontify-buffer))))

;;;###autoload
(define-globalized-minor-mode global-paren-face-mode
  paren-face-mode turn-on-paren-face-mode-if-desired
  :group 'paren-face)

(defun turn-on-paren-face-mode-if-desired ()
  (when (apply 'derived-mode-p paren-face-modes)
    (paren-face-mode 1)))

(provide 'paren-face)
;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;; paren-face.el ends here
