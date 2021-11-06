(setq neurosys/base-dir "/home/dan/repos/neurosys/")

(defun neurosys/deploy-to-host (host host-home-raw)
  (interactive "sHost: \nsHost home: ")
  (let ((host-root (format "/ssh:%s:/" host))
        ;; mind the trailing slash, since we're passing it to rsync
        (host-home (file-name-as-directory host-home-raw)))
    (save-window-excursion
      (org-babel-tangle)
      (my/run-in-fresh-compilation
       (format (concat neurosys/base-dir "rsync.sh %s %s") host host-home) neurosys/base-dir)
      ;; TODO: Is there cleaner way to compile over TRAMP?
      (find-file host-root)
      (compile "nixos-rebuild switch --show-trace")))
  (switch-to-buffer-other-window "*compilation*"))

(defun neurosys/deploy-to-nixos-dev ()
  (interactive)
  (neurosys/deploy-to-host "root@nixos-dev" "/home/dan/"))

(defun neurosys/open-config-file ()
  (interactive)
  (find-file (concat neurosys/base-dir "README.org")))

(map!
 :leader
 :prefix ("j" . "neurosys")
 :desc "deploy" "D" #'neurosys/deploy-to-host
 :desc "deploy to nixos-dev" "d" #'neurosys/deploy-to-nixos-dev)
