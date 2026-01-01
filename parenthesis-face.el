;;; parenthesis-face.el --- A face for parentheses  -*- lexical-binding:t -*-

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

;; This package defines a face named `parenthesis-face'.

;; Enable `parenthesis-face-mode' to use that face for all parentheses
;; in buffers whose major-mode is listed in `parenthesis-face-modes' or
;; which derives from a listed mode.

;; Alternatively, to enable use of that face for only a certain mode,
;; without affecting derived modes, use something like:
;;
;;   (font-lock-add-keywords 'emacs-lisp-mode parenthesis-face-keywords)

;; You might also be interested in the `bracket-face' package.

;;; Code:

(defface parenthesis-face '((t (:inherit shadow)))
  "Face used for parentheses by `parenthesis-face-mode'."
  :group 'font-lock-extra-types)

(defcustom parenthesis-face-modes
  '(lisp-data-mode scheme-mode clojure-mode)
  "Modes in which `parenthesis-face-mode' used `parenthesis-face'.
This also affects modes that derive from the listed modes."
  :group 'font-lock-extra-types
  :type '(repeat symbol))

(defvar parenthesis-face-keywords '(("[()]" 0 'parenthesis-face)))

(defun parenthesis-face--add-keywords ()
  (font-lock-add-keywords nil parenthesis-face-keywords))

(define-minor-mode parenthesis-face-mode
  "Use `parenthesis-face' in modes listed in `parenthesis-face-modes'.
The face is also used in modes that derive from the listed modes."
  :global t
  :lighter ""
  :group 'font-lock-extra-types
  (dolist (mode parenthesis-face-modes)
    (let ((hook (intern (format "%s-hook" mode))))
      (if parenthesis-face-mode
          (add-hook  hook #'parenthesis-face--add-keywords)
        (remove-hook hook #'parenthesis-face--add-keywords))))
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (derived-mode-p parenthesis-face-modes)
        (if parenthesis-face-mode
            (font-lock-add-keywords  nil parenthesis-face-keywords)
          (font-lock-remove-keywords nil parenthesis-face-keywords))
        (font-lock-flush)))))

(provide 'parenthesis-face)
;;; parenthesis-face.el ends here
