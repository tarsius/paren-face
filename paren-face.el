;;; paren-face.el --- A face for parentheses in lisp modes  -*- lexical-binding:t -*-

;; Copyright (C) 2013-2024 Jonas Bernoulli

;; Author: Jonas Bernoulli <emacs.paren-face@jonas.bernoulli.dev>
;; Homepage: https://github.com/tarsius/paren-face
;; Keywords: faces lisp

;; Package-Version: 1.1.2
;; Package-Requires: ((emacs "26.1") (compat "30.0.0.0"))

;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation, either version 3 of the License,
;; or (at your option) any later version.
;;
;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This library defines a face named `parenthesis' used just for
;; parentheses.  The originally intended purpose of this face is
;; to make parentheses less visible in Lisp code by dimming them.

;; We lispers probably don't need to be constantly made aware of
;; the existence of the parentheses.  Dimming them might be even
;; more useful for people new to lisp who have not yet learned to
;; subconsciously blend out the parentheses.

;; To use the `parenthesis' face, turn on `global-paren-face-mode'.
;; The option `paren-face-modes' controls in what buffers the minor
;; mode `paren-face-mode' is turned on.

;; The parenthesis at or before point, as well as the parenthesis at
;; the other end of the s-expression, should actually stand out, but
;; that is beyond the scope of the mode defined here.  Instead use one
;; of the modes dedicated to that, e.g., the builtin `show-paren-mode'.

;; While this face is intended to be used with Lisp modes, it also
;; works with other major-modes, just add the mode to the value of
;; `paren-face-modes'.  Likewise, while this face is intended for
;; dimming, it can also be used for highlighting.

;; By default parentheses and brackets are dimmed, customize option
;; `paren-face-regexp' if you also want to dim braces or don't want
;; to dim brackets.  If you want to use a different regexp in different
;; major-modes, then use the major-mode hook to set the buffer local
;; value.

;;; Code:

(require 'compat)

(defgroup paren-face nil
  "Face for parentheses in lisp modes."
  :group 'font-lock-extra-types
  :group 'faces)

(defface parenthesis '((t (:inherit shadow)))
  "Face for parentheses in lisp modes.
This face is only used if `paren-face-mode' is turned on.
See `global-paren-face-mode' for an easy way to do so."
  :group 'paren-face)

(defcustom paren-face-modes
  '(lisp-mode
    emacs-lisp-mode lisp-interaction-mode ielm-mode
    scheme-mode inferior-scheme-mode
    clojure-mode cider-repl-mode nrepl-mode
    arc-mode inferior-arc-mode)
  "Major modes in which `paren-face-mode' should be turned on.
When `global-paren-face-mode' is turned on, the buffer-local mode
is turned on in all buffers whose major mode is or derives from
one of the modes listed here."
  :type '(repeat symbol)
  :group 'paren-face)

(defcustom paren-face-regexp "[][()]"
  "Regular expression to match parentheses."
  :type 'regexp
  :group 'paren-face)

(defcustom paren-face-mode-lighter ""
  "String to display in the mode line when `paren-face-mode' is active."
  :type 'string
  :group 'paren-face)

;;;###autoload
(define-minor-mode paren-face-mode
  "Use a dedicated face just for parentheses."
  :lighter paren-face-mode-lighter
  (let ((keywords `((,paren-face-regexp 0 'parenthesis))))
    (if paren-face-mode
        (font-lock-add-keywords  nil keywords)
      (font-lock-remove-keywords nil keywords)))
  (when font-lock-mode
    (save-restriction
      (widen)
      (font-lock-flush)
      (font-lock-ensure))))

;;;###autoload
(define-globalized-minor-mode global-paren-face-mode
  paren-face-mode turn-on-paren-face-mode-if-desired
  :group 'paren-face)

(defun turn-on-paren-face-mode-if-desired ()
  (when (apply #'derived-mode-p paren-face-modes)
    (paren-face-mode 1)))

;;; _
(provide 'paren-face)
;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; paren-face.el ends here
