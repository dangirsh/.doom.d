(setq neurosys/base-dir "/home/dan/repos/neurosys/")

(defun neurosys/deploy-to-host (host)
  (interactive "sHost:")
  (let* ((host-root (format "/ssh:%s:/" host)))
    (save-window-excursion
      (org-babel-tangle)
      (my/run-in-fresh-compilation
       (format (concat neurosys/base-dir "rsync.sh")
               " %s" host))
      ;; TODO: Is there cleaner way to compile over TRAMP?
      (find-file host-root)
      (compile "nixos-rebuild switch")))
  (switch-to-buffer-other-window "*compilation*"))

(defun neurosys/open-config-file ()
  (interactive)
  (find-file (concat neurosys/base-dir "README.org")))

(map!
   :map org-mode-map
   :localleader
   (:prefix ("s" . "neurosys")
     :desc "Deploy" "d" #'neurosys/deploy-to-host))
