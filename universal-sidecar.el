;;; universal-sidecar.el --- A universal sidecar buffer for Emacs -*- lexical-binding: t -*-

;; Copyright (C) 2023 Samuel W. Flint <me@samuelwflint.com>

;; Author: Samuel W. Flint <me@samuelwflint.com>
;; SPDX-License-Identifier: GPL-3.0-or-later
;; URL: https://git.sr.ht/~swflint/emacs-universal-sidecar
;; Version: 0.0.1
;; Package-Requires: ((emacs "26.1") (magit-section "3.0.0") (cl-lib "0.5"))

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.


;;; Commentary:
;;

;;; Code:

(require 'cl-lib)
(require 'magit-section)


;;; Customizations

(defgroup universal-sidecar nil
  "Customization for universal-sidecar, showing relevant information for the focused buffer."
  :group 'convenience
  :prefix "universal-sidecar-"
  :link '(url-link :tag "Sourcehut" "https://git.sr.ht/~swflint/universal-sidecar"))

(defcustom universal-sidecar-buffer-name-format "*sidecar* (%F)"
  "Format for universal sidecar name.

Must contain %F, which is a string describing the current frame."
  :group 'universal-sidecar
  :type 'string)

(defcustom universal-sidecar-sections (list)
  "A list of sections that may be shown in the universal sidecar buffer.

Sections are functions that take a minimum of two arguments: a
BUFFER-FOR-SIDECAR (the buffer the sidecar is shown for), and
SIDECAR (the buffer holding sidecar sections).

A section may be described as either a function or a function and
arguments to be passed after the BUFFER-FOR-SIDECAR and SIDECAR
arguments."
  :group 'universal-sidecar
  :type '(repeat (choice (function :tag "Function")
                         (list :tag "Function with arguments"
                               (function :tag "Function")
                               (repeat :tag "Arguments"
                                       :inline t
                                       (sexp :tag "Argument"))))))


;;; Sidecar Buffer Mode

(defvar-local universal-sidecar-buffer-frame nil
  "Frame containing current sidecar.")

(defvar universal-sidecar-buffer-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map magit-section-mode-map)
    map)
  "Keymap for Sidecar Buffers.")

(define-derived-mode universal-sidecar-buffer-mode magit-section-mode "Sidecar"
  "Major mode for displaying information relevant to the current buffer."
  :group 'universal-sidecar)


;;; Opening Sidecars

(defun universal-sidecar-get-name (&optional frame)
  "Get the name of the sidecar buffer for FRAME.

If FRAME is nil, use `selected-frame'."
  (let ((frame (or frame (selected-frame)))
        (id (frame-parameter frame 'window-id)))
    (format-spec universal-sidecar-buffer-name-format (list (cons ?F id)))))

(defun universal-sidecar-get-buffer (&optional frame)
  "Get the sidecar buffer for FRAME."
  (get-buffer (universal-sidecar-get-name frame)))

(defun universal-sidecar-get-buffer-create (&optional frame)
  "Get or create a sidecar buffer for FRAME."
  (let ((frame (or frame (selected-frame))))
    (or (universal-sidecar-get-buffer frame)
        (with-current-buffer (get-buffer-create (universal-sidecar-get-name frame))
          (universal-sidecar-buffer-mode)
          (setq-local universal-sidecar-buffer-frame frame)
          (current-buffer)))))


;;; Defining Sidecar Sections

(cl-defun universal-sidecar--generate-major-modes-expression (major-modes &optional (buffer 'buffer-for-sidecar))
  "Generate the expression to check if the selected BUFFER is one of MAJOR-MODES."
  `(with-current-buffer ,buffer
     (derived-mode-p ,@(mapcar #'(lambda (mode) `',mode)
                               (or (and (listp major-modes)
                                        major-modes)
                                   (list major-modes))))))

(cl-defun universal-sidecar--generate-predicate (major-modes predicate &optional (buffer 'buffer-for-sidecar))
  "Generate predicate expression for MAJOR-MODES and PREDICATE.

Use BUFFER as the checked buffer."
  (cond
   ((and predicate major-modes)
    `(and
      ,(universal-sidecar--generate-major-modes-expression major-modes buffer)
      ,predicate))
   (predicate predicate)
   (major-modes (universal-sidecar--generate-major-modes-expression major-modes buffer))))

(cl-defmacro universal-sidecar-define-section (name (&rest args-list)
                                                    (&key predicate major-modes &allow-other-keys)
                                                    &body body
                                                    &aux
                                                    (docstring (and (stringp (car body))
                                                                    (car body))))
  "Define a sidecar section NAME with ARGS-LIST (implicit &key).

BODY is wrapped in PREDICATE if present, including checking
MAJOR-MODES.

The arguments BUFFER-FOR-SIDECAR and SIDECAR are bound in BODY.

If BODY has a string as the first element, this is used as the
DOCSTRING for the generated function."
  (let* ((body-no-docstring (if docstring
                                (cdr body)
                              body))
         (generated-predicate (universal-sidecar--generate-predicate major-modes predicate))
         (body-with-predicate (if generated-predicate
                                  `(when ,generated-predicate ,@body-no-docstring)
                                `(progn ,body-no-docstring))))
    `(cl-defun ,name (buffer-for-sidecar sidecar &key ,@args-list &allow-other-keys)
       ,docstring
       ,body-with-predicate)))


;;; Section generation utilities

(provide 'universal-sidecar)

;;; universal-sidecar.el ends here
