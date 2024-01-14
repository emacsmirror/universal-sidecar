;; SPDX-FileCopyrightText: 2024 Samuel W. Flint <me@samuelwflint.com>
;;
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; init.el ---                                      -*- lexical-binding: t; -*-

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(defun build/package-install-prereqs ()
  (package-refresh-contents)
  (dolist (pkg '(magit-section bibtex-completion elfeed elfeed-score org-roam citeproc ebib package-lint package-build ebdb))
    (package-install pkg)))

(defun build/build-packages ()
  (require 'package-build)
  (setf package-build-archive-dir (expand-file-name "~/package-archive/")
        package-build-recipes-dir (expand-file-name "~/emacs-universal-sidecar/build/recipes/"))
  (package-build-all))
