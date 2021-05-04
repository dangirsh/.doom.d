;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el
(package! ace-window)
(package! aggressive-indent)
(package! burly :recipe (:host github :repo "alphapapa/burly.el"))

;; (package! company-posframe)
;; (package! dap-mode)
(package! deadgrep)
(package! dired-narrow)
(package! dmenu)
(package! elegant-agenda-mode :recipe (:host github :repo "justinbarclay/elegant-agenda-mode"))
(package! evxcr-mode :recipe (:host github :repo "serialdev/evcxr-mode"))
(package! google-this)
(package! google-translate)
(package! helpful)
(package! jest)
(package! key-chord)
;; (package! mathpix :recipe (:host github :repo "jethrokuan/mathpix"))
(package! nano-emacs
  :recipe (:host github :repo "rougier/nano-emacs"))
(package! org-cliplink)
(package! org-download)
(package! org-drill)
(package! org-gcal)
(package! org-noter)
(package! org-recoll :recipe (:host github :repo "alraban/org-recoll"))
(package! org-ref)
(package! org-roam :recipe (:host github :repo "jethrokuan/org-roam"))
(package! org-roam-bibtex)
(package! org-roam-server)
(package! org-super-agenda)
(package! org-transclusion :recipe (:host github :repo "nobiot/org-transclusion"))

(package! phi-search)
(package! ob-rust)
(package! real-auto-save)
(package! smartscan)
(package! string-inflection)
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
  :recipe (:host github :repo "tpapp/julia-repl"))

(package! julia-formatter
  :recipe (:host github :repo "ki-chi/julia-formatter"))

;; (package! eglot-jl)
