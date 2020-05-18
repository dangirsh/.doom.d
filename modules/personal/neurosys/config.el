(map!
   :map org-mode-map
   :localleader
   (:prefix ("s" . "neurosys")
     :desc "Deploy" "d" #'neurosys/deploy-to-host))
