;;; org-cite-sidecar.el --- Sidecar to show references in Org Mode  -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Samuel W. Flint

;; Author: Samuel W. Flint <me@samuelwflint.com>
;; SPDX-License-Identifier: GPL-3.0-or-later
;; Homepage: https://git.sr.ht/~swflint/emacs-universal-sidecar
;; Keywords: bib, org
;; Version: 1.1.0
;; Package-Requires: ((emacs "28.1") (citeproc "0.9.4") (universal-sidecar "1.5.0") (universal-sidecar-citeproc "1.0.0"))

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
;; *Notice:* This package is now deprectated and will be removed from
;; MELPA around 18 March 2024.  Please use `org-cite-overlay-sidecar'
;; (https://git.sr.ht/~swflint/org-cite-overlay) in its place, which
;; does roughly the same thing, but in a more flexible way, and can
;; utilize information from the `org-cite-overlay' package (same URL).
;;
;; This package can be used to show an Org-Mode document's citations
;; in `universal-sidecar'.  This is done through the use of `citeproc'
;; and can be shown quite flexibly.  A minimum configuration is as
;; follows:
;;
;; (setq universal-sidecar-citeproc-locales "~/.emacs.d/csl-data/locales/"
;;       ;; set to your directories for locale and style data
;;       universal-sidecar-citeproc-styles "~/.emacs.d/csl-data/styles/")
;; (add-to-list 'universal-sidecar-sections 'org-cite-sidecar)
;;
;; It is important to set the `universal-sidecar-citeproc-locales' and
;; `universal-sidecar-citeproc-styles' variables to the directories
;; where you have cloned the CSL locale and style data repositories
;; (see docstrings for links).
;;
;; Additionally, there are two arguments to the section which are not
;; exposed as customization variables:
;;
;; - `:style' allows you to select a prefered CSL style within
;;   `universal-sidecar-citeproc-styles'.  Default is
;;   `universal-sidecar-citeproc-default-style'.
;; - `:header' allows you to change the header of the section from the
;;   default "References".


;;; Code:

(require 'citeproc)
(require 'universal-sidecar)
(require 'universal-sidecar-citeproc)
(require 'org)
(require 'org-element)
(require 'oc)
(require 'ol)


;;; Define the sidecar

(defvar org-cite-sidecar-deprecation-warning-shown nil
  "Has the `org-cite-sidecar' deprecation warning been shown?")

(defvar org-cite-sidecar-ignore-deprecation-warning nil
  "Ignore `org-cite-sidecar' deprecation warning.

This must be either nil or your username as determined by
the function `user-login-name'.")

(universal-sidecar-define-section org-cite-sidecar (style
                                                    (header "References"))
                                  (:major-modes (org-mode))
  "Show current buffer citations in Sidecar.

Select reference style by naming a CSL file in STYLE to override
`universal-sidecar-citeproc-default-style' (which see), and
section title using HEADER."
  (unless (or org-cite-sidecar-deprecation-warning-shown
              (string= org-cite-sidecar-ignore-deprecation-warning (user-login-name)))
    (display-warning 'org-cite-sidecar "`org-cite-sidecar' is deprecated, see commentary ((find-library \"org-cite-sidecar.el\"))")
    (setf org-cite-sidecar-deprecation-warning-shown t))
  (when-let* ((data-sources (mapcar #'expand-file-name (org-cite-list-bibliography-files)))
              (references (org-element-map (with-current-buffer buffer
                                             (org-element-parse-buffer))
                              'citation-reference
                            (lambda (citation)
                              (org-element-property :key citation))))
              (processor (universal-sidecar-citeproc-get-processor data-sources :style style)))
    (citeproc-add-uncited references processor)
    (with-current-buffer sidecar
      (universal-sidecar-insert-section org-cite-sidecar header
        (insert (universal-sidecar-fontify-as org-mode ((org-fold-core-style 'overlays))
                  (car (citeproc-render-bib processor 'org 'auto 'nil))
                  (save-match-data
                    (goto-char (point-min))
                    (while (re-search-forward org-target-regexp nil t)
                      (replace-match "")))))))))


(provide 'org-cite-sidecar)
;;; org-cite-sidecar.el ends here
