;;; init.el -*- lexical-binding: t; -*-

(doom! :input

       :personal
       neurosys

       :completion
       (company +childframe)

       :ui
       doom
       hl-todo
       modeline
       nav-flash
       zen

       :editor
       lispy
       multiple-cursors
       word-wrap
       format
       snippets

       :emacs
       ;; dired
       electric
       vc

       :term
       vterm

       :checkers
       syntax

       :tools
       (eval +overlay)
       direnv
       docker
       lookup
       (magit +forge)
       lsp
       pass
       pdf
       tree-sitter

       :lang
       ;; cc
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
       sh
       yaml

       :config
       literate
       (default +bindings))
