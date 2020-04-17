;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el
(package! aggressive-indent)
(package! ace-window)
(package! key-chord)
(package! helpful)
(package! phi-search)
(package! undo-tree)
(package! dired-narrow)
(package! deadgrep)
(package! dmenu)
(package! company-posframe)
(package! org-noter)
(package! org-roam
          :recipe (:host github :repo "jethrokuan/org-roam"))
(package! company-org-roam
  :recipe (:host github :repo "jethrokuan/company-org-roam"))

(package! smartscan)
(package! org-ref)
(package! google-this)
(package! wrap-region)
