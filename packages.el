;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el
(package! ace-window)
(package! aggressive-indent)
;; (package! company-posframe)
(package! deadgrep)
(package! dired-narrow)
(package! dmenu)
(package! google-this)
(package! helpful)
(package! jest)
(package! key-chord)
;; (package! mathpix :recipe (:host github :repo "jethrokuan/mathpix"))
(package! org-cliplink)
(package! org-download)
(package! org-drill)
(package! org-noter)
(package! org-recoll :recipe (:host github :repo "alraban/org-recoll"))
(package! org-ref)
(package! org-roam :recipe (:host github :repo "jethrokuan/org-roam"))
(package! org-roam-bibtex)
(package! org-roam-server)
(package! phi-search)
(package! ob-rust)
(package! real-auto-save)
(package! smartscan)
(package! toc-org)
(package! undo-tree)
(package! wrap-region)
(package! almost-mono-themes)


;; Julia
(package! julia-mode :pin "1c122f1dff")
(package! julia-snail)

;; Use the branch that supports the vterm backend
;; https://github.com/tpapp/julia-repl/pull/84
(package! julia-repl
  :recipe (:host github :repo "tpapp/julia-repl" :branch "tp/terminal-backends"))

(package! julia-formatter
  :recipe (:host github :repo "ki-chi/julia-formatter"))

;; (package! eglot-jl)
