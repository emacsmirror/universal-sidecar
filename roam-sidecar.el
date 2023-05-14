;;; roam-sidecar.el --- Integrate universal-sidecar and org-roam -*- lexical-binding: t -*-

;; Copyright (C) 2023 Samuel W. Flint <me@samuelwflint.com>

;; Author: Samuel W. Flint <me@samuelwflint.com>
;; SPDX-License-Identifier: GPL-3.0-or-later
;; URL: https://git.sr.ht/~swflint/emacs-universal-sidecar
;; Version: 0.5.0
;; Package-Requires: ((emacs "26.1") (universal-sidecar "0.3.0"))

;;; Commentary:
;;

(require 'org-roam)
(require 'universal-sidecar)

;;; Code:

(defun universal-sidecar-roam-section (buffer sidecar roam-section &rest args)
  "Run ROAM-SECTION with ARGS for BUFFER in SIDECAR."
  (when-let ((node (with-current-buffer buffer (org-roam-node-at-point nil))))
    (universal-sidecar-set-title (propertize (org-roam-node-title node) 'font-lock-face 'bold))
    (with-current-buffer sidecar
      (apply roam-section (cons node args)))))

(defun universal-sidecar-convert-roam-sections (sections-definition)
  "Convert SECTIONS-DEFINITION to `universal-sidecar-org-section'."
  (mapcar #'(lambda (defn)
              (if (listp defn)
                  (cons 'universal-sidecar-roam-section defn)
                (list 'universal-sidecar-roam-section defn)))
          sections-definition))

(provide 'roam-sidecar)

;;; roam-sidecar.el ends here
