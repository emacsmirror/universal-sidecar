;;; ebib-sidecar.el --- Sidecar to show formatted reference of current Ebib Entry  -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Samuel W. Flint

;; Author: Samuel W. Flint <me@samuelwflint.com>
;; SPDX-License-Identifier: GPL-3.0-or-later
;; Homepage: https://git.sr.ht/~swflint/emacs-universal-sidecar
;; Keywords: bib
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1") (citeproc "0.9.4") (universal-sidecar "1.5.0"))

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
;; library and can be shown flexibly.  A minimum configuration is
;; shown below:
;;
;; (setq ebib-sidecar-locales "~/.emacs.d/csl-data/locales/" ;set to your directories for locale and style data
;;       ebib-sidecar-styles "~/.emacs.d/csl-data/styles/")
;; (add-to-list 'universal-sidecar-sections 'ebib-sidecar)
;;
;; It is important to set the `ebib-sidecar-locales' and
;; `ebib-sidecar-styles' variables to the directories where you have
;; cloned the CSL locale and style data repositories (see docstrings
;; for links).
;;
;; Additionally, there are two arguments to the section which are not
;; exposed as customization variables:
;;
;; - `:style' allows you to select a prefered CSL style within
;;   `ebib-sidecar-styles'.  Default is `ieee.csl'.
;; - `:header' allows you to change the header of the section from the
;;   default "References".

;;; Code:

(require 'universal-sidecar)
(require 'ol)
(require 'ebib)
(require 'citeproc)


;;; Customization

(defgroup ebib-sidecar '()
  "Show org citations in sidecar."
  :group 'ebib
  :group 'universal-sidecar
  :prefix "ebib-sidecar-"
  :link '(url-link :tag "Sourcehut" "https://git.sr.ht/~swflint/emacs-universal-sidecar")
  :link '(emacs-library-link :tag "Library Source" "ebib-sidecar.el"))

(defcustom ebib-sidecar-locales "~/citeproc/locales/"
  "Path to the directory containing CSL locales data.

Citeproc locales data can be found at
https://github.com/citation-style-language/locales."
  :link '(url-link :tag "CSL Locales Repo" "https://github.com/citation-style-language/locales")
  :type 'directory
  :group 'ebib-sidecar)

(defcustom ebib-sidecar-styles "~/citeproc/styles/"
  "Path to the directory containing CSL style files.

Citeproc locales data can be found at
https://github.com/citation-style-language/styles."
  :link '(url-link :tag "CSL Locales Repo" "https://github.com/citation-style-language/styles")
  :type 'directory
  :group 'ebib-sidecar)


;;; Define the sidecar

(universal-sidecar-define-section ebib-sidecar ((style "ieee.csl")
                                                (header "Reference:"))
                                  (:major-modes (ebib-index-mode ebib-entry-mode))
  "Show a formatted reference for the current ebib entry in SIDECAR.

Select reference style by naming a CSL file in STYLE (must be
found in `ebib-sidecar-styles'), and section title using HEADER.

Note: It is necessary to also customize the locatino of locales
data, `ebib-sidecar-locales'."
  (when-let* ((db-file (let ((file-name (cdr (assoc 'filename ebib--cur-db))))
                         (and (stringp file-name)
                              (file-exists-p file-name)
                              file-name)))
              (key (ebib--get-key-at-point))
              (item-getter (citeproc-itemgetter-from-bibtex db-file ))
              (locale-getter (citeproc-locale-getter-from-dir ebib-sidecar-locales))
              (processor (citeproc-create (file-name-concat ebib-sidecar-styles style)
                                          item-getter locale-getter)))
    (citeproc-add-uncited (list key) processor)
    (with-current-buffer sidecar
      (universal-sidecar-insert-section ebib-sidecar header
        (insert (universal-sidecar-fontify-as org-mode ((org-fold-core-style 'overlays))
                  (car (citeproc-render-bib processor 'org 'auto 'nil))
                  (save-match-data
                    (goto-char (point-min))
                    (while (re-search-forward org-target-regexp nil t)
                      (replace-match "")))))))))

(provide 'ebib-sidecar)

;;; ebib-sidecar.el ends here
