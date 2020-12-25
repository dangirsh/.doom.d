;;; init.el -*- lexical-binding: t; -*-

(doom! :input
       ;; :desktop
       ;; exwm

       :personal
       neurosys

       :completion
       (company)
       (ivy +prescient +childframe)

       :ui
       deft
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
       pass
       pdf

       :lang
       common-lisp
       data
       emacs-lisp
       javascript
       (haskell +dante)
       ;; (julia +lsp)
       ;; julia
       (latex +latexmk +cdlatex)
       markdown
       nix
       (org
	+journal
	+hugo
	+jupyter
        +roam
	)
       python
       rust
       sh


       :app
       calendar

       :config
       literate
       (default +bindings)
       ;; (default +bindings +smartparens)
       )
