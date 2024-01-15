;;; universal-sidecar-citeproc.el --- Centralise Citeproc Support for Universal Sidecar  -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Samuel W. Flint

;; Author: Samuel W. Flint <me@samuelwflint.com>
;; SPDX-License-Identifier: GPL-3.0-or-later
;; Homepage: https://git.sr.ht/~swflint/emacs-universal-sidecar
;; Keywords: bib, convenience
;; Version: 1.0.0
;; Package-Requires: ((emacs "28.1") (citeproc "0.9.4"))

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
;;  The `universal-sidecar-citeproc' library simplifies the use of
;; `citeproc.el' to format citation data.  To this end, the
;; `universal-cidecar-citeproc' customization group allows
;; citeproc-related paths to be set once and easily reused (in
;; particular, it is important to remind users to set the
;; `universal-sidecar-citeproc-locales' and
;; `universal-sidecar-citeproc-styles' variables).  Additionally, it
;; provides a single place for users to set their default preferred
;; citation style (as an absolute path, or relative to
;; `universal-sidecar-citeproc-styles').
;;
;; Users are encouraged to customize in the following pattern.
;;
;;    (setopt universal-sidecar-citeproc-locales "~/citeproc/locales/"
;;            universal-sidecar-citeproc-styles "~/citeproc/styles/"
;;            universal-sidecar-citeproc-default-style "ieee.csl")
;;
;; For developers, there are two main functions,
;; `universal-sidecar-citeproc-get-processor' and
;; `universal-sidecar-citeproc-org-output'.  The former will create a
;; citation processor (see `citeproc' library documentation) from a
;; list of data sources, and style/locale information.  The latter
;; will take a processor and generate an `org-mode' formatted (and
;; fontified) string for insertion into a sidecar.  These may be used
;; as follows:
;;
;;     (defun demo-citeproc-formatter (keys bibliography)
;;       (let ((processor (universal-sidecar-citeproc-get-processor bibliography)))
;;         (citeproc-add-uncited keys processor)
;;         (universal-sidecar-citeproc-org-output processor)))

;;; Code:

(require 'citeproc)
(require 'cl-lib)
(require 'ol)


;;; Customization

(defgroup universal-sidecar-citeproc '()
  "Helpers for integration of citations into `universal-sidecar'."
  :group 'universal-sidecar
  :link '(url-link :tag "Sourcehut" "https://git.sr.ht/~swflint/emacs-universal-sidecar")
  :link '(emacs-library-link :tag "Library Source" "universal-sidecar-citeproc.el"))

(defcustom universal-sidecar-citeproc-locales "~/citeproc/locales/"
  "Path to the directory containing CSL locales data.

Citeproc locales data can be found at
https://github.com/citation-style-language/locales."
  :link '(url-link :tag "CSL Locales Repo" "https://github.com/citation-style-language/locales")
  :type 'directory
  :group 'universal-sidecar-citeproc)

(defcustom universal-sidecar-citeproc-styles "~/citeproc/styles/"
  "Path to the directory containing CSL style files.

Citeproc locales data can be found at
https://github.com/citation-style-language/styles."
  :link '(url-link :tag "CSL Locales Repo" "https://github.com/citation-style-language/styles")
  :type 'directory
  :group 'universal-sidecar-citeproc)

(defcustom universal-sidecar-citeproc-default-style "chicago.csl"
  "Path to default citation style.

If relative, it will be expanded with respect to
`universal-sidecar-citeproc-styles' (which see).  Otherwise, it
will be treated as an absolute path."
  :type 'string
  :group 'universal-sidecar-citeproc)


;;; Utility Functions: Locale Getter, Processor

(defun universal-sidecar-citeproc-locale-getter ()
  "Get a locale getter from `universal-sidecar-citeproc-locales'."
  (citeproc-locale-getter-from-dir universal-sidecar-citeproc-locales))

(cl-defun universal-sidecar-citeproc-get-processor (data-sources &key style locale-getter locale force-locale)
  "Return a citeproc processor.

Most important is DATA-SOURCES, a list of CSL JSON or BibTeX
databases from which citation data can be collected.

 - STYLE, a CSL filename, which defaults to
   `universal-sidecar-citeproc-default-style' which see.
 - LOCALE-GETTER, to get the necessary locale data (default is a
   fresh locale getter from
   `universal-sidecar-citeproc-locale-getter').
 - LOCALE, the name of a locale to use (i.e., a language code).
   If FORCE-LOCALE, then it will be used no matter what."
  (let* ((style (or style universal-sidecar-citeproc-default-style))
         (locale-getter (or locale-getter (universal-sidecar-citeproc-locale-getter)))
         (style-file (expand-file-name style universal-sidecar-citeproc-styles))
         (item-getter (citeproc-hash-itemgetter-from-any data-sources)))
    (citeproc-create style-file item-getter locale-getter locale force-locale)))


;;; Utility Functions: Generate Org Output

(defun universal-sidecar-citeproc-org-output (processor)
  "Generate `org-mode' formatted (and fontfied) output from PROCESSOR."
  (with-temp-buffer
    (org-mode)
    (setq-local org-fold-core-style 'overlays)
    (insert (car (citeproc-render-bib processor 'org 'auto 'nil)))
    (save-match-data
      (goto-char (point-min))
      (while (re-search-forward org-target-regexp nil t)
        (replace-match "")))
    (font-lock-ensure)
    (buffer-string)))

(provide 'universal-sidecar-citeproc)
;;; universal-sidecar-citeproc.el ends here
