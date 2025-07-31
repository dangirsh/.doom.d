;;; init.el -*- lexical-binding: t; -*-

(doom! :input

	 :personal
	 neurosys

	 :completion
	 (vertico +icons)

	 :ui
	 doom
	 modeline
	 zen

	 :editor
	 lispy
	 multiple-cursors
	 (format +onsave)

	 :emacs
	 ;dired
	 electric

	 :term
	 vterm

	 :checkers
	 syntax

	 :tools
       direnv
       docker
       magit
       (lsp +eglot)
       pass
       pdf
       tree-sitter

	 :lang
	 cc
	 data
	 emacs-lisp
	 ;; (go +lsp)
	 ;; javascript
	 ;; (haskell +dante)
	 ;; (julia +lsp)
	 (latex +latexmk +cdlatex)
	 markdown
	 nix
	 (org +roam2)
	 python
	 ;; rest
       (rust +lsp)
	 sh
	 yaml

	 :config
	 literate
	 (default +bindings))
