;;; init.el -*- lexical-binding: t; -*-

(doom! :input
	     ;; :desktop
	     ;; exwm

       :personal
       neurosys

	     :completion
	     (company +childframe)
	     (ivy +prescient +childframe)

	     :ui
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

	     :emacs
	     dired
	     electric
	     vc

	     :term
       vterm

	     :checkers
	     syntax

	     :tools
	     ;; direnv
	     (eval +overlay)
	     lookup
	     (magit +forge)
	     pass
	     pdf

	     :lang
	     common-lisp
	     data
	     emacs-lisp
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
	      )
	     sh

	     :app
	     calendar

	     :config
	     literate
	     (default +bindings +smartparens))
