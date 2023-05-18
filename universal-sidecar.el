;;; universal-sidecar.el --- A universal sidecar buffer -*- lexical-binding: t -*-

;; Copyright (C) 2023 Samuel W. Flint <me@samuelwflint.com>

;; Author: Samuel W. Flint <me@samuelwflint.com>
;; SPDX-License-Identifier: GPL-3.0-or-later
;; URL: https://git.sr.ht/~swflint/emacs-universal-sidecar
;; Version: 1.0.0
;; Package-Requires: ((emacs "25.1") (magit-section "3.0.0"))

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
;;; Usage
;;
;; The `universal-sidecar-toggle' command will bring up a per-frame
;; "sidecar" buffer.  These sidecar buffers are used to show
;; information about or related to the focused buffer.  Information is
;; shown in *sections*, which are configured using the
;; `universal-sidecar-sections' variable.  The behavior of this
;; variable, and expected interface is described below in
;; configuration.

;; Additionally, to make sure that the sidecar buffer is updated, it's
;; necessary to advise several functions.  This can be done
;; automatically using the `universal-sidecar-update-insinuate'
;; function, which will advise functions listed in
;; `universal-sidecar-insinuate-commands'.  This may be undone with
;; `universal-sidecar-unadvise-commands'.
;;
;;;; Configuration
;;
;; The name of the sidecar buffer is configured using
;; `universal-sidecar-buffer-name-format', which must contain `%F', a
;; representation of the description of the frame.
;;
;; Which sections are shown is configured using
;; `universal-sidecar-sections', which is a list of functions or
;; functions-with-arguments.  For example, let's consider the section
;; `buffer-git-status', which shows git status.  This section allows a
;; keyword argument, `:show-renames', which defaults to t.  If we want
;; the default behavior, we would configure it using
;;
;;     (add-to-list 'universal-sidecar-sections 'buffer-git-status)
;;
;; However, if we want the opposite behavior (don't show renames),
;; we'd configure it as shown below.
;;
;; (add-to-list 'universal-sidecar-sections '(buffer-git-status :show-renames t))
;;
;; Note that using `add-to-list' is generally bad practice, as the
;; sections will be run in the order they're present in the list.
;;
;;
;; Finally, sidecar buffers are displayed using `display-window'.
;; This means that how the buffer is displayed is easily configurable
;; from `display-buffer-alist'.  The author's configuration is shown
;; below as an example.  In particular, using the
;; `display-buffer-in-side-window' display action is suggested, as
;; it's generally not helpful to select the sidecar window through
;; normal window motion commands
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
;;; Section Functions
;;
;; The basic installation of `universal-sidecar' does not include any
;; section functions.  This is to reduce the number of dependencies of
;; the package itself so that it may be used as a library for others,
;; or to help integrate multiple packages.  A
;; `universal-sidecar-sections.el' package is available as well, which
;; will have simple section definitions that may be of use.

;; However, implementation of functions is generally straight-forward.
;; First, sections are simply functions which take a minimum of two
;; arguments, `buffer', or the buffer we're generating a sidecar for,
;; and `sidecar', the sidecar buffer.  When writing these section
;; functions, it is recommended to avoid writing content to `sidecar'
;; until it's verified that the information needed is available.  That
;; is **don't write sections without bodies**.

;; To aid in defining sections, the `universal-sidecar-define-section'
;; and `universal-sidecar-insert-section' macros are available.  The
;; first defines a section which can be added to
;; `universal-sidecar-sections'.  The second simplifies writing
;; sections by adding proper separators and headers to the sidecar
;; buffer.  We will demonstrate both below.
;;
;;     (universal-sidecar-define-section fortune-section (file title)
;;                                       (:major-modes org-mode
;;                                                     :predicate (not (buffer-modified-p)))
;;       (let ((title (or title
;;                        (and file
;;                             (format "Fortune: %s" file))
;;                        "Fortune"))
;;             (fortune (shell-command-to-string (format "fortune%s"
;;                                                       (if file
;;                                                           (format " %s" file)
;;                                                         "")))))
;;         (universal-sidecar-insert-section fortune-section title
;;           (insert fortune))))
;;
;; Note: the arguments (`file' and `title') are *keyword* arguments.
;; Additionally, you specify that this section only applies when
;; `buffer' is a descendent of `:major-modes' which can be either a
;; symbol or a list of symbols.  `:predicate' is used to specify a
;; somewhat more complex predicate to determine if the section should
;; be generated.
;;
;; This section could be added in any of the following ways:
;;
;;     (add-to-list 'universal-sidecar-sections 'fortune-section)
;;     (add-to-list 'universal-sidecar-sections '(fortune-section :file "definitions"))
;;     (add-to-list 'universal-sidecar-sections '(fortune-section :title "O Fortuna!"))
;;     (add-to-list 'universal-sidecar-sections '(fortune-section :file "definitions" :title "Random Definition"))

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

(defvar-local universal-sidecar-current-buffer nil
  "What is the current buffer for the sidecar, before refresh?")

(defvar universal-sidecar-buffer-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map magit-section-mode-map)
    (define-key map (kbd "g") #'universal-sidecar-refresh)
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
    (let* ((sidecar (or sidecar
                        (universal-sidecar-get-buffer)))
           (buffer (or buffer
                       (if-let ((buf (window-buffer (selected-window)))
                                (_buffer-is-sidecar-p (equal buf sidecar)))
                           (with-current-buffer sidecar universal-sidecar-current-buffer)
                         buf))))
      (with-current-buffer sidecar
        (let ((inhibit-read-only t))
          (universal-sidecar-buffer-mode)
          (erase-buffer)
          (setq-local universal-sidecar-current-buffer buffer)
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

(cl-defun universal-sidecar--generate-major-modes-expression (major-modes)
  "Generate the expression to check if the selected BUFFER is one of MAJOR-MODES."
  `(derived-mode-p ,@(mapcar #'(lambda (mode) `',mode)
                             (or (and (listp major-modes)
                                      major-modes)
                                 (list major-modes)))))

(cl-defun universal-sidecar--generate-predicate (major-modes predicate &optional (buffer 'buffer))
  "Generate predicate expression for MAJOR-MODES and PREDICATE.

Use BUFFER as the checked buffer."
  `(with-current-buffer ,buffer
     ,(cond
       ((and predicate major-modes)
        `(and
          ,(universal-sidecar--generate-major-modes-expression major-modes)
          ,predicate))
       (predicate predicate)
       (major-modes (universal-sidecar--generate-major-modes-expression major-modes)))))

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
