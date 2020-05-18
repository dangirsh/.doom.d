(defun neurosys/open-config-file ()
  (interactive)
  (find-file "/home/dan/repos/neurosys/README.org"))

(map!
   :map org-mode-map
   :localleader
   (:prefix ("s" . "neurosys")
     :desc "Deploy" "d" #'neurosys/deploy-to-host))
