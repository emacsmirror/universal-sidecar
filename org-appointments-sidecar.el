;;; org-appointments-sidecar.el --- Sidecar to show upcoming appointments  -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Samuel W. Flint

;; Author: Samuel W. Flint <me@samuelwflint.com>
;; SPDX-License-Identifier: GPL-3.0-or-later
;; Homepage: https://git.sr.ht/~swflint/emacs-universal-sidecar
;; Keywords: calendar
;; Version: 0.5.0
;; Package-Requires: ((emacs "29.1") (universal-sidecar "1.5.0") (ts "0.3") (org-ql "0.8"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; This package displays upcoming appointments/events from the Org
;; Agenda in the Universal Sidecar.  It's based on
;; `org-upcoming-modeline'
;; (https://github.com/unhammer/org-upcoming-modeline/) but allows the
;; display of multiple upcoming (and active) events.  Basic
;; configuration is as simple as adding `org-appointments-sidecar' to
;; `universal-sidecar-sections' at the appropriate spot.
;;
;; More advanced customization is available as follows:
;;
;;  - To ignore events based on their TODO states or tags, customize
;;    the variables `org-appointments-sidecar-ignored-states' and
;;    `org-appointments-ignored-tags', respectively.  These are simply
;;    lists of ignored states/tags.
;;  - The duration of the considered period is controlled by
;;    `org-appointments-sidecar-look-back' and
;;    `org-appointments-sidecar-look-ahead' which define the start and
;;    end of the period relative to NOW respectively.  They are cons
;;    cells of the form (DURATION . UNIT), where DURATION is an
;;    integer, and UNIT is a symbol in (second minute hour day).
;;  - Display of events is controlled by
;;    `org-appointments-sidecar-format-function', the default of which
;;    is `org-appointments-sidecar-format-appointment'.  In general,
;;    this function should take an `org-element' heading, and return
;;    org-formatted text.  For further customization (ex, developing
;;    ones own formatting function), see the docstring.
;;  - If using the default format function, the variable
;;    `org-appointments-sidecar-show-properties' is used to select and
;;    display various properties from the heading.  It should be a
;;    list of either strings naming properties, or cons cells of
;;    property name/display name pairs.

;;; Code:

(require 'ts)
(require 'cl-lib)
(require 'org-ql)
(require 'universal-sidecar)


;;; Customization

(defgroup org-appointments-sidecar (list)
  "Use the Universal Sidecar to show upcoming Org Agenda events."
  :group 'org
  :group 'universal-sidecar
  :link '(url-link :tag "Sourcehut" "https://git.sr.ht/~swflint/emacs-universal-sidecar")
  :link '(emacs-library-link :tag "Library Source" "org-appointments-sidecar.el"))

(defcustom org-appointments-sidecar-show-events 5
  "Number of upcoming events to show."
  :group 'org-appointments-sidecar
  :type 'integer)

(defcustom org-appointments-sidecar-ignored-states nil
  "List of TODO states to ignore."
  :group 'org-appointments-sidecar
  :type '(repeat string))

(defcustom org-appointments-sidecar-ignored-tags nil
  "List of tags to ignore."
  :group 'org-appointments-sidecar
  :type '(repeat string))

(defcustom org-appointments-sidecar-look-back '(15 . minute)
  "How far to look back.

Format is a cons-cell of integer and unit, where unit is one of
the symbols \\='second, \\='minute, \\='hour, or \\='day."
  :group 'org-appointments-sidecar
  :type '(cons :tag "Period"
               (integer :tag "Length")
               (choice :tag "Unit"
                       (const :tag "Seconds" second)
                       (const :tag "Minutes" minute)
                       (const :tag "Hours" hour)
                       (const :tag "Days" day))))

(defcustom org-appointments-sidecar-look-ahead '(1 . day)
  "How far to look ahead.

Format is a cons-cell of integer and unit, where unit is one of
the symbols \\='second, \\='minute, \\='hour, or \\='day."
  :group 'org-appointments-sidecar
  :type '(cons :tag "Period"
               (integer :tag "Length")
               (choice :tag "Unit"
                       (const :tag "Seconds" second)
                       (const :tag "Minutes" minute)
                       (const :tag "Hours" hour)
                       (const :tag "Days" day))))

(defcustom org-appointments-sidecar-show-properties
  '(("LOCATION" . "Location")
    ("ZOOM" . "Zoom Room"))
  "List of properties to show if present.

Properties are listed as either a string (simply a name), or a
cons cell (a property name, then a title)."
  :group 'org-appointments-sidecar
  :type '(repeat (choice (string :tag "Property")
                         (cons :tag "Property with Title"
                               (string :tag "Property")
                               (string :tag "Title")))))

(defcustom org-appointments-sidecar-format-function
  #'org-appointments-sidecar-format-appointment
  "How should upcoming appointments be formatted?

Default function is
`org-appointments-sidecar-format-appointment'.  In general, this
should take an `org-element' heading, and return a string of
org-formatted text.

For those wishing to write their own format function, the
following functions are available:

 - `org-appointments-sidecar--appointment-title'
 - `org-appointments-sidecar--display-properties'
 - `org-appointments-sidecar--scheduling-data'
 - `org-appointments-sidecar--format-time-until'"
  :group 'org-appointments-sidecar
  :type '(choice (function-item :tag "Default Formatter (list-like)"
                                org-appointments-sidecar-format-appointment)
                 (function :tag "Custom Formatter")))


;;; Adjust Times

(defun org-appointments-sidecar--start-end-times ()
  "Determine the start and end times of the search period.

Obey `org-appointments-sidecar-look-back' and
`org-appointments-sidecar-look-ahead'."
  (let* ((now (ts-now))
         (start (ts-adjust (cdr org-appointments-sidecar-look-back)
                           (- (car org-appointments-sidecar-look-back))
                           now))
         (end (ts-adjust (cdr org-appointments-sidecar-look-ahead)
                         (car org-appointments-sidecar-look-ahead)
                         now)))
    (list start end)))


;;; Find Events

(defun org-appointments-sidecar-find-events ()
  "Find upcoming events/appointments.

Events considered are no more than
`org-appointments-sidecar-look-back' before NOW, and no more than
`org-appointments-sidecar-look-ahead' from NOW.  Additionally,
any events with a TODO state in
`org-appointments-sidecar-ignored-states' are ignored, as are
events matching any tags in
`org-appointments-sidecar-ignored-tags'."
  (cl-destructuring-bind (start-time end-time) (org-appointments-sidecar--start-end-times)
    (when-let ((org-files (org-agenda-files)))
      (org-ql-select org-files
        `(and (or (planning :from ,start-time :to ,end-time)
                  (ts-active :from ,start-time :to ,end-time))
              (not ,(if org-appointments-sidecar-ignored-states
                        `(todo ,@org-appointments-sidecar-ignored-states)))
              (not ,(if org-appointments-sidecar-ignored-tags
                        `(tags ,@org-appointments-sidecar-ignored-tags))))
        :action 'element-with-markers
        :sort '(date reverse)))))


;;; Formatting Tools

(defun org-appointments-sidecar--appointment-title (appointment)
  "Get formatted title of APPOINTMENT."
  (substring-no-properties (cl-find-if #'stringp (org-element-property :title appointment))))

(defun org-appointments-sidecar--display-properties (appointment)
  "Get displayed properties for APPOINTMENT.

Displayed properties are determined by
`org-appointments-sidecar-show-properties'."
  (delq nil (mapcar (lambda (property)
                      (when-let ((property-name (if (consp property)
                                                    (car property)
                                                  property))
                                 (property-title (if (consp property)
                                                     (cdr property)
                                                   property))
                                 (property-symbol (intern (format ":%s" (upcase property-name))))
                                 (value (org-element-property property-symbol appointment)))
                        (cons property-title value)))
                    org-appointments-sidecar-show-properties)))

(defun org-appointments-sidecar--scheduling-data (appointment)
  "Get the upcoming time for APPOINTMENT.

The upcoming time is the timestamp (including deadline or
scheduled) which starts no earlier than the look-back period (see
`org-appointments-sidecar-look-back')."
  (cl-destructuring-bind (start-time _end-time) (org-appointments-sidecar--start-end-times)
    (car (sort (cl-remove-if
                (lambda (ts) (or (null ts) (ts<= ts start-time)))
                (mapcar
                 #'ts-parse-org-element
                 (delq nil (append
                            (org-element-map appointment 'timestamp #'identity)
                            (list (org-element-property :deadline appointment)
                                  (org-element-property :scheduled appointment))))))
               #'ts<))))

(defun org-appointments-sidecar--format-time-until (timestamp)
  "Format time until TIMESTAMP."
  (if (ts<= timestamp (ts-now))
      "now"
    (let* ((seconds-until (ts-difference timestamp (ts-now)))
           (time-until (ts-human-duration seconds-until))
           (hours (plist-get time-until :hours))
           (minutes (plist-get time-until :minutes))
           (seconds (plist-get time-until :seconds))
           (display-minutes (+ minutes (if (>= 30 seconds)
                                           1
                                         0))))
      (format "in %s%s"
              (if (> hours 0)
                  (format "%dh" hours)
                "")
              (if (> display-minutes 0)
                  (format "%dm" display-minutes)
                "")))))


;;; Default Formatter

(defun org-appointments-sidecar-format-appointment (appointment)
  "Format APPOINTMENT for display."
  (when-let* ((title-data  (org-element-property :title appointment))
              (schedule-data (org-appointments-sidecar--scheduling-data appointment))
              (formatted-time-until (org-appointments-sidecar--format-time-until schedule-data)))
    (with-temp-buffer
      (insert (format " - *%s* (%s)%s"
                      (org-appointments-sidecar--appointment-title appointment)
                      formatted-time-until
                      (if-let ((tags (org-element-property :tags appointment)))
                          (format " %s" tags)
                        "")))
      (mapcan (lambda (prop)
                (insert (format "\n    + *%s*: %s" (car prop) (cdr prop))))
              (org-appointments-sidecar--display-properties appointment))
      (buffer-string))))


;;; Define the Sidecar

(universal-sidecar-define-section org-appointments-sidecar ((header "Upcoming Appointments")) ()
  "Show upcoming appointments.

HEADER allows changing the section title.

No more than `org-appointments-sidecar-show-events' events will
be shown from `org-appointments-sidecar-look-back' before NOW to
`org-appointments-sidecar-look-ahead' from NOW.

Formatting of events is performed by
`org-appointments-sidecar-format-function'."
  (when-let* ((events (org-appointments-sidecar-find-events)))
    (with-current-buffer sidecar
      (universal-sidecar-insert-section org-appointments-sidecar header
        ;; (setf temp-events events)
        (cl-dolist (event (take org-appointments-sidecar-show-events events))
          (insert
           (universal-sidecar-fontify-as org-mode ((org-fold-core-style 'overlays))
             (funcall org-appointments-sidecar-format-function event)))
          (insert "\n"))
        (insert "\n")))))

(provide 'org-appointments-sidecar)

;;; org-appointments-sidecar.el ends here
