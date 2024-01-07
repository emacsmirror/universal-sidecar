;;; org-cite-sidecar.el --- Sidecar to show references in Org Mode  -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Samuel W. Flint

;; Author: Samuel W. Flint <me@samuelwflint.com>
;; SPDX-License-Identifier: GPL-3.0-or-later
;; Homepage: https://git.sr.ht/~swflint/emacs-universal-sidecar
;; Keywords: bib, org
;; Version: 0.5.0
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
;; This package can be used to show an Org-Mode document's citations
;; in `universal-sidecar'.  This is done through the use of `citeproc'
;; and can be shown quite flexibly.  A minimum configuration is as
;; follows:
;;
;; (setq org-cite-sidecar-locales "~/.emacs.d/csl-data/locales/"
;;       ;; set to your directories for locale and style data
;;       org-cite-sidecar-styles "~/.emacs.d/csl-data/styles/")
;; (add-to-list 'universal-sidecar-sections 'org-cite-sidecar)
;;
;; It is important to set the `org-cite-sidecar-locales' and
;; `org-cite-sidecar-styles' variables to the directories where you
;; have cloned the CSL locale and style data repositories (see
;; docstrings for links).
;;
;; Additionally, there are two arguments to the section which are not
;; exposed as customization variables:
;;
;; - `:style' allows you to select a prefered CSL style within
;;   `org-cite-sidecar-styles'.  Default is `ieee.csl'.
;; - `:header' allows you to change the header of the section from the
;;   default "References".


;;; Code:

(require 'universal-sidecar)
(require 'org)
(require 'org-element)
(require 'oc)
(require 'ol)
(require 'citeproc)


;;; Customization

(defgroup org-cite-sidecar '()
  "Show org citations in sidecar."
  :group 'org-cite
  :group 'universal-sidecar
  :prefix "org-cite-sidecar-"
  :link '(url-link :tag "Sourcehut" "https://git.sr.ht/~swflint/emacs-universal-sidecar")
  :link '(emacs-library-link :tag "Library Source" "org-cite-sidecar.el"))

(defcustom org-cite-sidecar-locales "~/citeproc/locales/"
  "Path to the directory containing CSL locales data.

Citeproc locales data can be found at
https://github.com/citation-style-language/locales."
  :link '(url-link :tag "CSL Locales Repo" "https://github.com/citation-style-language/locales")
  :type 'directory
  :group 'org-cite-sidecar)

(defcustom org-cite-sidecar-styles "~/citeproc/styles/"
  "Path to the directory containing CSL style files.

Citeproc locales data can be found at
https://github.com/citation-style-language/styles."
  :link '(url-link :tag "CSL Locales Repo" "https://github.com/citation-style-language/styles")
  :type 'directory
  :group 'org-cite-sidecar)


;;; Define the sidecar

(universal-sidecar-define-section org-cite-sidecar ((style "ieee.csl")
                                                    (header "References"))
                                  (:major-modes (org-mode))
  "Show current buffer citations in Sidecar.

Select reference style by naming a CSL file in STYLE (must be
found in `org-cite-sidecar-styles'), and the section title using
HEADER.

Note: It is necessary to also customize the location of locales
data, `org-cite-sidecar-locales'."
  (when-let* ((data-sources (org-cite-list-bibliography-files))
              (references (org-element-map (with-current-buffer buffer
                                             (org-element-parse-buffer))
                              'citation-reference
                            (lambda (citation)
                              (org-element-property :key citation))))
              (item-getter (citeproc-hash-itemgetter-from-any data-sources))
              (locale-getter (citeproc-locale-getter-from-dir org-cite-sidecar-locales))
              (processor (citeproc-create (file-name-concat org-cite-sidecar-styles style)
                                          item-getter locale-getter)))
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
