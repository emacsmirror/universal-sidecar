;;; universal-sidecar-sections.el --- Simple sections for universal-sidecar -*- lexical-binding: t -*-

;; Copyright (C) 2023 Samuel W. Flint <me@samuelwflint.com>

;; Author: Samuel W. Flint <me@samuelwflint.com>
;; SPDX-License-Identifier: GPL-3.0-or-later
;; URL: https://git.sr.ht/~swflint/emacs-universal-sidecar
;; Version: 0.6.0

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
;; This is a collection of miscellaneous sections for `universal-sidecar'.

;;; Code:

(require 'universal-sidecar)


;;; Basic "tail of buffer" section

(defun universal-sidecar-sections-resolve-buffer (buffer)
  "Resolve BUFFER to an actual buffer object.

If BUFFER is a buffer, return, if it's a string, use
`get-buffer', if a symbol, get its value, and if a function, call
it."
  (cond
   ((bufferp buffer) buffer)
   ((stringp buffer) (get-buffer buffer))
   ((symbolp buffer) (symbol-value buffer))
   ((functionp buffer) (funcall buffer))))

(defun universal-sidecar-sections-buffer-tail (buffer n)
  "Get the last N lines from BUFFER, return nil if BUFFER is empty."
  (unless (= 0 (buffer-size buffer))
    (with-current-buffer buffer
      (goto-char (point-max))
      (forward-line (- n))
      (beginning-of-line)
      (string-trim (buffer-substring (point) (point-max))))))

(universal-sidecar-define-section universal-sidecar-tail-buffer-section (shown-buffer n-lines title) ()
  "Show N-LINES of SHOWN-BUFFER in a sidecar with TITLE.

Note: SHOWN-BUFFER may be a buffer, string, or function."
  (when (and (stringp title)
             (integerp n-lines))
    (when-let* ((shown-buffer (universal-sidecar-sections-resolve-buffer shown-buffer))
                (contents (universal-sidecar-sections-buffer-tail shown-buffer n-lines)))
      (universal-sidecar-insert-section tail-buffer-section title
        (with-current-buffer sidecar
          (insert contents))))))


;;; Org Clock Section

(universal-sidecar-define-section universal-sidecar-org-clock-section ()
                                  (:predicate (org-clock-is-active))
  "Show a display of current `org-clock' data.

In particular, show the total completed today, the time for the
current task, and the time spent on the task today.

If you use this, it is helpful to
include (universal-sidecar-org-clock-insinuate) somewhere in your
config."
  (cl-destructuring-bind (total-today task-today)
      (with-current-buffer (org-clock-is-active)
        (let ((org-clock-report-include-clocking-task t))
          (list (org-duration-from-minutes (org-clock-sum-today))
                (save-excursion
                  (save-restriction
                    (goto-char org-clock-marker)
                    (org-narrow-to-subtree)
                    (setq org-clock-today-subtree-time
                          (org-clock-today--total-minutes)))))))
    (universal-sidecar-insert-section universal-sidecar-org-clock-section
        (format "Org Clock (%s today)" total-today)
      (insert (format "%s (%s out of %s today)"
                      org-clock-heading
                      (org-duration-from-minutes (org-clock-get-clocked-time))
                      task-today)))))

(defun universal-sidecar-org-clock-insinuate ()
  "Insinuate sidecar updating to `org-clock' commands."
  (universal-sidecar-advise-commands '(org-clock-in
                                       org-clock-out
                                       org-clock-cancel
                                       org-clock-in-last)))


;;; imenu section

(defun universal-sidecar--imenu-to-menu (depth item)
  "Convert a tree (defined by ITEM) into a text representation.

DEPTH is taken as the depth for the present level of the tree."
  (cond
   ((not (listp item))
    (format "%s - %s" (make-string (* 2 depth) 32) item))
   ((not (listp (cdr item)))
    (format "%s - %s" (make-string (* 2 depth) 32) (car item)))
   ((not (listp (car item)))
    (string-join (cons (format "%s - %s" (make-string (* 2 depth) 32) (car item))
                       (mapcar (apply-partially #'universal-sidecar--imenu-to-menu (1+ depth))
                               (cdr item)))
                 "\n"))
   (t
    (string-join (mapcar (apply-partially #'universal-sidecar--imenu-to-menu depth)
                         item)
                 "\n"))))

(universal-sidecar-define-section universal-sidecar-imenu-section ((header "Outline")) ()
  "Show the imenu items for the current BUFFER.

HEADER allows you to override the section title (default \"Outline\")."
  (when-let* ((items
               (condition-case condition
                   (with-current-buffer buffer
                     (save-excursion
                       (without-restriction
                         (let ((imenu-flatten nil))
                           (funcall imenu-create-index-function)))))
                 (imenu-unavailable nil)))
              (section-text (universal-sidecar--imenu-to-menu 0 items)))
    (with-current-buffer sidecar
      (universal-sidecar-insert-section universal-sidecar-imenu-section header
        (insert
         (universal-sidecar-fontify-as org-mode ((org-fold-core-style 'overlays)
                                                 (org-inhibit-startup t)
                                                 (org-agenda-files nil))
           section-text))))))

;;; TODO: Implement some (more) generic sidecars

(provide 'universal-sidecar-sections)

;;; universal-sidecar-sections.el ends here
