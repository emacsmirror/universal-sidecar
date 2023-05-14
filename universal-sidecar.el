;;; universal-sidecar.el --- A universal sidecar buffer -*- lexical-binding: t -*-

;; Copyright (C) 2023 Samuel W. Flint <me@samuelwflint.com>

;; Author: Samuel W. Flint <me@samuelwflint.com>
;; SPDX-License-Identifier: GPL-3.0-or-later
;; URL: https://git.sr.ht/~swflint/emacs-universal-sidecar
;; Version: 0.5.0
;; Package-Requires: ((emacs "26.1") (magit-section "3.0.0"))

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
;;; Installation
;;
;; This package has one main requirement: `magit', for
;; `magit-section'.  Assuming this package is satisfied, the
;; `universal-sidecar.el' file may be placed on the load path and
;; `require'd.
;;
;;; Usage and Configuration
;;
;; There are two main direct configuration points: the buffer name
;; format, and the sections list.
;;
;; The buffer name format is what allows multiple sidecar buffers to
;; be shown, presently, one per frame.  This is a format string which
;; must contain `%F'.
;;
;; The sections list, on the otherhand, is more complex.  This is how
;; you determine what is shown in a sidecar for a buffer.  Generally
;; speaking, this is a list of functions which take a minimum of two
;; arguments: the buffer and the sidecar.  Using information in the
;; buffer, the sidecar will be populated.  Each section function will
;; be run in the order of the list: it's important to carefully
;; consider what order you put these in.  More about defining sections
;; is written below.
;;
;; Additionally, the sidecar buffer is shown using `display-buffer'.
;; The author's recommended configuration is shown below.
;;
;;     (add-to-list 'display-buffer-alist
;;                  '("\\*sidecar\\*"
;;                    (display-buffer-in-side-window)
;;                    (slot . 0)
;;                    (window-width . 0.2)
;;                    (window-height . 0.2)
;;                    (preserve-size t . t)
;;                    (window-parameters . ((no-other-window . t)
;;                                          (no-delete-other-windows . t)))))
;;
;; Finally, advice is used to help ensure that the sidecar buffer gets
;; updated appropriately.  This can be done semi-automatically using
;; the `universal-sidecar-advise-commands' function.  This will
;; automatically advise functions in the `universal-sidecar-advise'
;; list with after advice, which calls `universal-sidecar-refresh'.

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

(defcustom universal-sidecar-advise-commands
  (list 'switch-to-buffer
        'other-window)
  "A list of commands which should be advised to update the sidecar buffer."
  :group 'universal-sidecar
  :type '(list symbol))


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

(defun universal-sidecar-visible-p (&optional frame)
  "Determine visibility of current sidecar visibility in FRAME."
  (when-let ((buffer (universal-sidecar-get-buffer frame)))
    (windowp (get-buffer-window buffer))))

(defun universal-sidecar-toggle (&optional frame)
  "Toggle showing sidecar buffer for FRAME."
  (interactive)
  (if (universal-sidecar-visible-p frame)
      (quit-window nil (get-buffer-window (universal-sidecar-get-buffer frame)))
    (display-buffer (universal-sidecar-get-buffer-create frame))
    (universal-sidecar-refresh)))


;;; Sidecar Display

(defun universal-sidecar-set-title (title &optional sidecar)
  "Set the header TITLE in SIDECAR."
  (with-current-buffer (or sidecar
                           (universal-sidecar-get-buffer))
    (setq-local header-line-format (concat (propertize " " 'display '(space :align-to 0))
                                           title))))

(defun universal-sidecar-refresh (&optional buffer sidecar)
  "Refresh sections for BUFFER in SIDECAR.

If BUFFER is non-nil, use the currently focused buffer.
If SIDECAR is non-nil, use sidecar for the current frame."
  (interactive)
  (when (universal-sidecar-visible-p)
    (let ((buffer (or buffer
                      (window-buffer (selected-window))))
          (sidecar (or sidecar
                       (universal-sidecar-get-buffer))))
      (with-current-buffer sidecar
        (let ((inhibit-read-only t))
          (universal-sidecar-buffer-mode)
          (erase-buffer)
          (universal-sidecar-set-title (propertize (buffer-name buffer) 'face 'bold) sidecar)
          (dolist (section universal-sidecar-sections)
            (pcase section
              ((pred functionp)
               (funcall section buffer sidecar))
              (`(,section . ,args)
               (apply section (append (list buffer sidecar)
                                      args)))
              (_
               (user-error "Invalid section definition `%S' in `universal-sidecar-sections'" section))))

          (goto-char 0))))))


;;; Updating the Sidecar
(defun universal-sidecar-after-command-function (&rest _)
  "After certain commands are run, refresh the sidecar."
  (universal-sidecar-refresh))

(defun universal-sidecar-advise-commands ()
  "Automatically advise functions to update the sidecar buffer."
  (mapcan (lambda (cmd)
            (advice-add cmd :after #'universal-sidecar-after-command-function))
          universal-sidecar-insinuate-commands))

(defun universal-sidecar-unadvise-commands ()
  "Unadvise commands that update the sidecar buffer."
  (mapcan (lambda (cmd)
            (advice-remove cmd #'universal-sidecar-after-command-function))
          universal-sidecar-insinuate-commands))


;;; Defining Sidecar Sections

(cl-defun universal-sidecar--generate-major-modes-expression (major-modes &optional (buffer 'buffer))
  "Generate the expression to check if the selected BUFFER is one of MAJOR-MODES."
  `(with-current-buffer ,buffer
     (derived-mode-p ,@(mapcar #'(lambda (mode) `',mode)
                               (or (and (listp major-modes)
                                        major-modes)
                                   (list major-modes))))))

(cl-defun universal-sidecar--generate-predicate (major-modes predicate &optional (buffer 'buffer))
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

The arguments BUFFER and SIDECAR are bound in BODY.

If BODY has a string as the first element, this is used as the
DOCSTRING for the generated function."
  (declare (indent 3))
  (let* ((body-no-docstring (if docstring
                                (cdr body)
                              body))
         (generated-predicate (universal-sidecar--generate-predicate major-modes predicate))
         (body-with-predicate (if generated-predicate
                                  `(when ,generated-predicate ,@body-no-docstring)
                                `(progn ,body-no-docstring))))
    `(cl-defun ,name (buffer sidecar &key ,@args-list &allow-other-keys)
       ,docstring
       ,body-with-predicate)))


;;; Section generation utilities

(defvar universal-sidecar-section-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map universal-sidecar-buffer-mode-map)
    map))

(defclass universal-sidecar-section (magit-section)
  ((keymap :initform 'universal-sidecar-section-map)))

(cl-defmacro universal-sidecar-insert-section (name header &body body)
  "Insert section NAME as generated by running BODY with HEADER.

Note, this macro ensures that a separating double-newline is inserted by default."
  (declare (indent 2))
  `(magit-insert-section ,name (universal-sidecar-section)
     (magit-insert-heading ,header)
     ,@body
     (insert "\n\n")))

(provide 'universal-sidecar)

;;; universal-sidecar.el ends here
