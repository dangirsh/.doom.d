;;; init.el -*- lexical-binding: t; -*-

(doom! :input
       ;; :desktop
       ;; exwm

       :personal
       neurosys

       :completion
       (company)
       ;; (ivy +prescient +childframe)

       :ui
       ;; deft
       doom
       hl-todo
       modeline
       nav-flash
       zen

       :editor
       ;; Nice, but messes with org-journal
       lispy
       multiple-cursors
       word-wrap
       format
       snippets

       :emacs
       dired
       electric
       vc

       :term
       vterm

       :checkers
       syntax

       :tools
       direnv
       (eval +overlay)
       docker
       lookup
       (magit +forge)
       lsp
       pass
       pdf

       :lang
       common-lisp
       data
       emacs-lisp
       go
       ;; javascript
       (haskell +dante)
       ;; (julia +lsp)
       ;; julia
       (latex +latexmk +cdlatex)
       markdown
       nix
       (org +hugo
	    +jupyter
            +roam2)
       python
       (rust +lsp)
       ;; rust
       sh
       yaml

       :app
       ;; calendar

       :config
       literate
       (default +bindings)
       ;; (default +bindings +smartparens)
       )
