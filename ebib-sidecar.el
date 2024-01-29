;;; ebib-sidecar.el --- Sidecar to show formatted reference of current Ebib Entry  -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Samuel W. Flint

;; Author: Samuel W. Flint <me@samuelwflint.com>
;; SPDX-License-Identifier: GPL-3.0-or-later
;; Homepage: https://git.sr.ht/~swflint/emacs-universal-sidecar
;; Keywords: bib
;; Version: 1.0.0
;; Package-Requires: ((emacs "28.1") (citeproc "0.9.4") (universal-sidecar "1.5.0") (universal-sidecar-citeproc "1.0.0") (ebib "2.39"))

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
;; This package can be used to show a formatted reference to the bib
;; entry at point in `ebib'.  This is done using the `citeproc'
;; library and can be flexibly configured.  Handling of locale and
;; style management is performed using `universal-sidecar-citeproc',
;; and its variables (`universal-sidecar-citeproc-styles' and
;; `universal-sidecar-citeproc-locales') must be configured.
;;
;; A minimum configuration is shown as follows:
;;
;;     ;; set to your directories for locale and style data
;;     (setopt universal-sidecar-citeproc-locales "~/.emacs.d/csl-data/locales/"
;;             universal-sidecar-citeproc-styles "~/.emacs.d/csl-data/styles/")
;;     (add-to-list 'universal-sidecar-sections 'org-cite-sidecar)
;;
;; Additionally, there are two arguments to the section which are not
;; exposed as customization variables:
;;
;; - `:style' allows you to select a prefered CSL style to override
;;   `universal-sidecar-citeproc-default-style'.
;; - `:header' allows you to change the header of the section from the
;;   default "References".

;;; Code:

(require 'universal-sidecar)
(require 'universal-sidecar-citeproc)
(require 'ebib)


;;; Define the sidecar

(universal-sidecar-define-section ebib-sidecar (style (header "Reference:"))
                                  (:major-modes (ebib-index-mode ebib-entry-mode))
  "Show a formatted reference for the current ebib entry in SIDECAR.

Select reference style by naming a CSL file in STYLE to override
`universal-sidecar-citeproc-default-style' (which see), and
section title using HEADER."
  (when-let* ((db-file (let ((file-name (cdr (assoc 'filename ebib--cur-db))))
                         (and (stringp file-name)
                              (file-exists-p file-name)
                              file-name)))
              (key (ebib--get-key-at-point))
              (processor (universal-sidecar-citeproc-get-processor db-file :style style)))
    (citeproc-add-uncited (list key) processor)
    (with-current-buffer sidecar
      (universal-sidecar-insert-section ebib-sidecar header
        (insert (universal-sidecar-citeproc-org-output processor))))))

(provide 'ebib-sidecar)

;;; ebib-sidecar.el ends here
