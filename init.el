;;; init.el -*- lexical-binding: t; -*-

(doom! :input

       :desktop
       exwm                                        ; Emacs X Window Manager

       :completion
       (company +childframe)                       ; the ultimate code completion backend
       (ivy +prescient)                            ; a search engine for love and life

       :ui
       doom                                        ; what makes DOOM look the way it does
       doom-dashboard                              ; a nifty splash screen for Emacs
       hl-todo                                     ; highlight TODO/FIXME/NOTE/DEPRECATED/HACK/REVIEW
       modeline                                    ; snazzy, Atom-inspired modeline, plus API
       nav-flash                                   ; blink the current line after jumping
       ophints                                     ; highlight the region an operation acts on
       vc-gutter                                   ; vcs diff in the fringe
       zen                                         ; distraction-free coding or writing

       :editor
       file-templates                              ; auto-snippets for empty files
       fold                                        ; (nigh) universal code folding
       lispy                                       ; vim for lisp, for people who don't like vim
       multiple-cursors                            ; editing in many places at once
       snippets                                    ; my elves. They type so I don't have to
       word-wrap                                   ; soft wrapping with language-aware indent

       :emacs
       dired                                       ; making dired pretty [functional]
       electric                                    ; smarter, keyword-based electric-indent
       vc                                          ; version-control and Emacs, sitting in a tree

       :term
       eshell                                      ; a consistent, cross-platform shell (WIP)

       :checkers
       syntax                                      ; tasing you for every semicolon you forget

       :tools
       direnv
       (eval +overlay)                             ; run code, run (also, repls)
       lookup                                      ; navigate your code and its documentation
       (magit +forge)                                       ; a git porcelain for Emacs
       pass                                        ; password manager for nerds
       pdf                                         ; pdf enhancements

       :lang
       common-lisp                               ; if you've seen one lisp, you've seen them all
       data                                        ; config/data formats
       emacs-lisp                                  ; drown in parentheses
       (haskell +dante)                            ; a language that's lazier than I am
       julia                                       ; a better, faster MATLAB
       markdown                                    ; writing docs for people to ignore
       nix                                       ; I hereby declare "nix geht mehr!"
       (org                                        ; organize your plain life in plain text
	+journal
	+hugo                                      ; use Emacs for hugo blogging
	+jupyter                                   ; ipython/jupyter support for babel
	)
       sh                                          ; she sells {ba,z,fi}sh shells on the C xor

       :app
       calendar

       :config
       literate
       (default +bindings +smartparens))
