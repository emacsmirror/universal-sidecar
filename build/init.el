;;; init.el ---                                      -*- lexical-binding: t; -*-

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(defun build/package-install-prereqs ()
  (package-refresh-contents)
  (dolist (pkg '(magit-section bibtex-completion elfeed elfeed-score org-roam citeproc ebib))
    (package-install pkg)))
