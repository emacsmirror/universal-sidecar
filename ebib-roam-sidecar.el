(require 'universal-sidecar)

(universal-sidecar-define-section ebib-current-shown-section () ()
  (let* ((current-db ebib--cur-db)
         (current-key (ebib--db-get-current-entry-key current-db))
         (node-ids (org-roam-db-query [:select [refs:node-id]
                                               :from refs
                                               :where (= refs:ref $s1)]
                                      current-key))
         (node (org-roam-node-from-id (car (car node-ids)))))
    (with-current-buffer sidecar
      (insert (format "%s\n\n" current-key)))
    ;; (when node
    ;;   (with-current-buffer sidecar
    ;;     (org-roam-reflinks-section node)))
    ))

(insert (format "\n(setf universal-sidecar-sections '%S)" universal-sidecar-sections))

(setf universal-sidecar-sections '(elfeed-score-section
                                   elfeed-related-works
                                   
                                   (tail-buffer-section :show-buffer appt-sidecar-buffer :n-lines 3 :title "Upcoming Appointments")
                                   (universal-sidecar-roam-section org-roam-show-attachments)
                                   (universal-sidecar-roam-section org-roam-ebdb-section)
                                   (universal-sidecar-roam-section orb-section-reference)
                                   (universal-sidecar-roam-section orb-section-abstract)
                                   (universal-sidecar-roam-section org-roam-backlinks-section :unique t :show-backlink-p my/org-roam-backlinks-filter)
                                   (universal-sidecar-roam-section org-roam-reflinks-section)))
