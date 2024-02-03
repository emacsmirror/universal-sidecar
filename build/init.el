;;; init.el ---                                      -*- lexical-binding: t; -*-

;; SPDX-FileCopyrightText: 2024 Samuel W. Flint <me@samuelwflint.com>
;;
;; SPDX-License-Identifier: GPL-3.0-or-later

(defvar build/required-packages
  '(;; Linting, Style Checking, Packaging
    package-lint
    package-build

    ;; Dependencies
    magit-section

    org-roam

    bibtex-completion
    citeproc
    ebib

    elfeed
    elfeed-score

    ebdb)
  "Packages required for building the various recipes.")

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(defun build/package-install-prereqs ()
  (package-refresh-contents)
  (dolist (pkg build/required-packages)
    (package-install pkg)))

(defun build/build-packages ()
  (require 'package-build)
  (setf package-build-archive-dir (expand-file-name "~/package-archive/")
        package-build-recipes-dir (expand-file-name "~/emacs-universal-sidecar/build/recipes/"))
  (package-build-all))
