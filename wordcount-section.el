;;; wordcount-section.el --- Universal Sidecar Section to show Word Counts  -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Samuel W. Flint

;; Author: Samuel W. Flint <me@samuelwflint.com>
;; SPDX-License-Identifier: GPL-3.0-or-later
;; Homepage: https://git.sr.ht/~swflint/emacs-universal-sidecar
;; Keywords: text, convenience
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1") (compat "29.1") (universal-sidecar "1.5.1"))

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
;; Word count information for a buffer can be shown using
;; `wordcount-section'.  Basic installation is as follows.
;;
;;     (add-to-list 'universal-sidecar-sections #'wordcount-section)
;;
;; The wordcount display can be customized in three ways:
;;
;;  - The `:header' keyword argument can be used to change the section
;;    header.
;;  - The format of word count information is controlled by the
;;    variable `wordcount-section-format', which supports the
;;    following percent escapes:
;;    | %w | Word Count      |
;;    | %s | Sentence Count  |
;;    | %l | Line Count      |
;;    | %c | Character Count |
;;  - The `wordcount-section-modes' variable controls the modes that
;;    have wordcounts shown.  This list is passed to `derived-mode-p'
;;    in the target buffer.

;;; Code:

(require 'universal-sidecar)


;;; Customization

(defgroup wordcount-section '()
  "Customization for `wordcount-section' for `universal-sidecar'."
  :group 'universal-sidecar
  :group 'text
  :prefix "wordcount-section-"
  :link '(url-link :tag "Sourcehut" "https://git.sr.ht/~swflint/emacs-universal-sidecar")
  :link '(emacs-library-link :tag "Library Source" "wordcount-section.el"))

(defcustom wordcount-section-format "%w words, %s sentences, %l lines, %c characters"
  "Format for wordcount display.

The following percent metacharacters are available:

| %w | Word Count      |
| %s | Sentence Count  |
| %l | Line Count      |
| %c | Character Count |

See also: `count-words', `count-lines', and `count-sentences'."
  :group 'wordcount-section
  :type 'string)

(defcustom wordcount-section-modes '(text-mode)
  "Which modes (and derived modes thereof) should show counts?

This list is passed to `derived-mode-p', which see."
  :group 'wordcount-section
  :type '(repeat (function :tag "Mode")))


;;; Section Definition

(defun wordcount-section--wordcounts (buffer)
  "Get counts for BUFFER as an alist."
  (with-current-buffer buffer
    (let ((start (point-min))
          (end (point-max)))
      (if (= start end)
          `((words . 0)
            (lines . 0)
            (sentences . 0)
            (characters . 0))
        `((words . ,(count-words start end))
          (lines . ,(count-lines start end))
          (sentences . ,(count-sentences start end))
          (characters . ,(- end start)))))))

(universal-sidecar-define-section wordcount-section ((header "Wordcount:"))
                                  (:predicate (apply #'derived-mode-p wordcount-section-modes))
  "Display a wordcount for BUFFER in SIDECAR.

The HEADER is customizable via keyword argument, and the format
of the wordcount data is controllable by
`wordcount-section-format', which see.

Additionally, the modes that have wordcounts shown are
controllable by `wordcount-section-modes'."
  (let-alist (wordcount-section--wordcounts buffer)
    (universal-sidecar-insert-section wordcount-section header
      (insert (format-spec wordcount-section-format
                           `((?w . ,.words)
                             (?s . ,.sentences)
                             (?l . ,.lines)
                             (?c . ,.characters)))))))

(provide 'wordcount-section)

;;; wordcount-section.el ends here
