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
(package! super-save)
(package! org-ref)
(package! google-this)
(package! wrap-region)

;; Julia
(package! julia-mode :pin "1c122f1dff")
(package! julia-snail)

;; Use the branch that supports the vterm backend
;; https://github.com/tpapp/julia-repl/pull/84
(package! julia-repl
  :recipe (:host github :repo "tpapp/julia-repl" :branch "tp/terminal-backends"))

;; (package! eglot-jl)
