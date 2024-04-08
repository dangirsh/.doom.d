;;; init.el -*- lexical-binding: t; -*-

  (doom! :input

	 :personal
	 neurosys

	 :completion
	 (company +childframe)

	 :ui
	 doom
	 modeline
	 zen

	 :editor
	 lispy
	 multiple-cursors
	 (format +onsave)

	 :emacs
	 dired
	 electric

	 :term
	 vterm

	 :checkers
	 syntax

	 :tools
;	 (eval +overlay)
         direnv
         docker
         lookup
         magit
         (lsp +eglot)
         pass
         pdf
         tree-sitter

	 :lang
	 cc
	 ; common-lisp
	 data
	 emacs-lisp
	 go
	 ;; javascript
	 (haskell +dante)
	 ;; (julia +lsp)
	 ; (latex +latexmk +cdlatex)
	 markdown
	 nix
	 (org +roam2)
	 python
	 rest
       (rust +lsp)
	 sh
	 yaml

	 :config
	 literate
	 (default +bindings))
