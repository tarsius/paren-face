;;; bracket-face.el --- A face for brackets  -*- lexical-binding:t -*-

;; Copyright (C) 2025-2026 Jonas Bernoulli

;; Author: Jonas Bernoulli <emacs.paren-face@jonas.bernoulli.dev>
;; Homepage: https://github.com/tarsius/paren-face
;; Keywords: faces lisp

;; Package-Version: 1.2.3
;; Package-Requires: ((emacs "30.1"))

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

;; This package defines a face named `bracket-face'.

;; Enable `bracket-face-mode' to use that face for all brackets
;; in buffers whose major-mode is listed in `bracket-face-modes' or
;; which derives from a listed mode.

;; Alternatively, to enable use of that face for only a certain mode,
;; without affecting derived modes, use something like:
;;
;;   (font-lock-add-keywords 'emacs-lisp-mode bracket-face-keywords)

;; You might also be interested in the `parenthesis-face' package.

;;; Code:

(defface bracket-face '((t (:inherit bold)))
  "Face used for brackets by `bracket-face-mode'."
  :group 'font-lock-extra-types)

(defcustom bracket-face-modes
  '(lisp-data-mode scheme-mode clojure-mode)
  "Modes in which `bracket-face-mode' uses `bracket-face'.
This also affects modes deriving from the listed modes."
  :group 'font-lock-extra-types
  :type '(repeat symbol))

(defvar bracket-face-keywords '(("[][]" 0 'bracket-face)))

(defun bracket-face--add-keywords ()
  (font-lock-add-keywords nil bracket-face-keywords))

(define-minor-mode bracket-face-mode
  "Use `bracket-face' in modes listed in `bracket-face-modes'.
The face is also used in modes that derive from the listed modes."
  :global t
  :lighter ""
  :group 'font-lock-extra-types
  (dolist (mode bracket-face-modes)
    (let ((hook (intern (format "%s-hook" mode))))
      (if bracket-face-mode
          (add-hook  hook #'bracket-face--add-keywords)
        (remove-hook hook #'bracket-face--add-keywords))))
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (derived-mode-p bracket-face-modes)
        (if bracket-face-mode
            (font-lock-add-keywords  nil bracket-face-keywords)
          (font-lock-remove-keywords nil bracket-face-keywords))
        (font-lock-flush)))))

(provide 'bracket-face)
;;; bracket-face.el ends here
