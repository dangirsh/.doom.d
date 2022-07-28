;; -*- no-byte-compile: t; -*-
  ;;; $DOOMDIR/packages.el
(package! ace-window)
(package! aggressive-indent)
;; (package! benchmark-init)
;; (package! burly :recipe (:host github :repo "alphapapa/burly.el"))
;; (package! company-org-block)
;; (package! company-posframe)
(package! copilot
  :recipe (:host github :repo "zerolfx/copilot.el" :files ("*.el" "dist")))
;; (package! dap-mode)
(package! deadgrep)
(package! dired-narrow)
(package! dmenu)
(package! elegant-agenda-mode :recipe (:host github :repo "justinbarclay/elegant-agenda-mode"))
;; (package! evxcr-mode :recipe (:host github :repo "serialdev/evcxr-mode"))
(package! google-this)
;; (package! google-translate)
(package! helpful)
;; (package! jest)
(package! key-chord)
;; (package! mathpix :recipe (:host github :repo "dangirsh/mathpix"))
;; (package! nano-emacs
;;   :recipe (:host github :repo "rougier/nano-emacs"))
(package! ob-graphql)
(package! openai-api :recipe (:host github :repo "dangirsh/openai-api.el"))
(package! org-cliplink)
(package! org-download)
;; (package! org-drill)
;; (package! org-gcal)
(package! org-noter)
;; (package! org-recoll :recipe (:host github :repo "alraban/org-recoll"))
(package! org-ref)
(package! org-roam :recipe (:host github :repo "jethrokuan/org-roam"))
;; (package! org-roam-bibtex)
(package! websocket)
(package! org-roam-ui :recipe (:host github :repo "org-roam/org-roam-ui" :files ("*.el" "out")))
(package! org-super-agenda)
;; (package! org-transclusion :recipe (:host github :repo "nobiot/org-transclusion"))

(package! phi-search)
;; (package! ob-rust)
(package! real-auto-save)
(package! rust-mode)
;; (package! s3ed)
(package! smartscan)
(package! string-inflection)
(package! toc-org)
(package! undo-tree)
(package! wrap-region)
;; (package! almost-mono-themes)

(package! selectrum)
(package! orderless)
(package! selectrum-prescient)
(package! ctrlf)
(package! mini-frame)
(package! consult)
(package! consult-flycheck)
(package! consult-projectile :recipe (:host gitlab :repo "OlMon/consult-projectile"))
(package! marginalia)
;; (package! embark)
;; (package! embark-consult)

(package! logview)

(package! fancy-dabbrev)


;; Julia
;; (package! julia-mode)
;; (package! julia-snail)

;; (package! julia-repl
;;   :recipe (:host github :repo "tpapp/julia-repl"))

;; (package! julia-formatter
;;   :recipe (:host github :repo "ki-chi/julia-formatter"))

;; (package! eglot-jl)

;; (unpin! dirvish)
