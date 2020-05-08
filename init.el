;;; init.el -*- lexical-binding: t; -*-

(doom! :input

	     :desktop
	     exwm

	     :completion
	     (company +childframe)
	     (ivy +prescient)

	     :ui
	     doom
	     doom-dashboard
	     hl-todo
	     modeline
	     nav-flash
	     ophints
	     vc-gutter
	     zen

	     :editor
       ;; Nice, but messes with org-journal
	     ;; file-templates
	     fold
	     lispy
	     multiple-cursors
	     snippets
	     word-wrap

	     :emacs
	     dired
	     electric
	     vc

	     :term
	     eshell
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
       ;; lsp

	     :lang
	     common-lisp
	     data
	     emacs-lisp
	     (haskell +dante)
	     ;; (julia +lsp)
	     ;; julia
	     markdown
	     nix
	     (org
	      +journal
	      +hugo
	      +jupyter
	      )
	     sh
       ;; python

	     :app
	     calendar

	     :config
	     literate
	     (default +bindings +smartparens))
