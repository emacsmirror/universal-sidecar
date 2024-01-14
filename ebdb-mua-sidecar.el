;;; ebdb-mua-sidecar.el --- EBDB Integration for Universal Sidecar  -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Samuel W. Flint

;; Author: Samuel W. Flint <me@samuelwflint.com>
;; SPDX-License-Identifier: GPL-3.0-or-later
;; Homepage: https://git.sr.ht/~swflint/emacs-universal-sidecar
;; Keywords: mail, convenience
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1") (universal-sidecar "1.5.1") (ebdb "0.8.20"))

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
;; EBDB records for MUA buffers can be displayed using Universal
;; Sidecar.  As of now, it only supports `message-mode',
;; `gnus-article-mode' and `gnus-summary-mode' (patches welcome to
;; support others, though I suspect it's just the major mode names
;; that need set).  To use this section, add `ebdb-mua-sidecar' to
;; your `universal-sidecar-sections', and have
;; `ebdb-mua-sidecar-insinuate' run somewhere in your init.
;;
;; The following options are available:
;;
;;  - `:header' allows you to change the header of the section from
;;    the default "Addressees:".
;;  - `:updatep' (default 'existing), determines how records are
;;    updated (see also `ebdb-update-records').
;;  - `:formatter' (default `ebdb-default-multiline-formatter')
;;    determines how records are formatted.  This should be an
;;    instance of `ebdb-record-formatter'.

;;; Code:

(require 'universal-sidecar)
(require 'ebdb-mua)

(universal-sidecar-define-section ebdb-mua-sidecar ((header "Addressees:")
                                                    (updatep 'existing)
                                                    (formatter ebdb-default-multiline-formatter))
                                  (:major-modes (message-mode gnus-article-mode gnus-summary-mode))
  "Show EBDB entries for BUFFER.

Applies only when MODE is one of `message-mode',
`gnus-article-mode', `gnus-summary-mode'.

Use HEADER to denote start of EBDB entries.  Record updates
follow UPDATEP (see `ebdb-update-records').  Entries are
formatted according to FORMATTER (should be an instance of
`ebdb-record-formatter', default is
`ebdb-default-multiline-formatter')."
  (when-let* ((records (with-current-buffer buffer (ebdb-update-records (ebdb-get-address-components) updatep))))
    (universal-sidecar-insert-section ebdb-message-sidecar header
      (dolist (record records)
        (insert (ebdb-fmt-record formatter record))))))

(defun ebdb-mua-sidecar-insinuate ()
  "Insinuate sidecar updating to MUA commands."
  (universal-sidecar-advise-commands '(message-tab
                                       gnus-summary-scroll-up
                                       gnus-summary-prev-unread-article
                                       gnus-summary-next-unread-article)))

(provide 'ebdb-mua-sidecar)

;;; ebdb-mua-sidecar.el ends here
