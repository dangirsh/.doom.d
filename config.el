;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(setq user-full-name "Dan Girshovich"
      user-mail-address (rot13 "qna.tvefu@tznvy.pbz"))

(setq my/home-dir "/home/dan/")

(setq my/sync-base-dir (concat my/home-dir "Sync/"))
(setq my/work-base-dir (concat my/home-dir "Work/"))
(setq my/media-base-dir (concat my/home-dir "Media/"))

(setq org-directory my/sync-base-dir
      org-roam-directory (concat my/sync-base-dir "org-roam/"))

(load-file (concat doom-private-dir "funcs.el"))

(setq doom-font (font-spec :family "Hack" :size 16)
      doom-variable-pitch-font (font-spec :family "Libre Baskerville")
      doom-serif-font (font-spec :family "Libre Baskerville"))

(when (file-exists-p "~/.doom.d/banners")
  (setq +doom-dashboard-banner-padding '(0 . 2)
        +doom-dashboard-banner-file "deepfield-window.png"
        +doom-dashboard-banner-dir "~/.doom.d/banners"))

(setq display-line-numbers-type nil)

;; Thin grey line separating windows
(set-face-background 'vertical-border "grey")
(set-face-foreground 'vertical-border (face-background 'vertical-border))

(use-package! doom-themes
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t      ; if nil, bold is universally disabled
        doom-themes-enable-italic t)   ; if nil, italics is universally disabled
  (load-theme 'doom-solarized-light t)
  ;; (load-theme 'doom-one-light t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)

  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))


;; Waiting on https://github.com/hlissner/emacs-doom-themes/issues/252
;; Currently, some things like italics and some links in org fail to render correctly.
;; (use-package! poet-theme
;;   :config
;;   (load-theme 'poet))

(use-package! key-chord
  :config
  (key-chord-mode 1)
  (setq key-chord-one-keys-delay 0.02
        key-chord-two-keys-delay 0.03))

(defun simulate-seq (seq)
  (setq unread-command-events (listify-key-sequence seq)))

(defun send-doom-leader ()
  (interactive)
  (simulate-seq "\C-c"))

(setq doom-localleader-alt-key "M-c")

(defun send-doom-local-leader ()
  (interactive)
  (simulate-seq "\M-c"))

  (after! key-chord

    (key-chord-define-global "fj" 'send-doom-leader)
    (key-chord-define-global "gh" 'send-doom-local-leader)

    (setq dk-keymap (make-sparse-keymap))
    (setq sl-keymap (make-sparse-keymap))

    (key-chord-define-global "dk" dk-keymap)
    (key-chord-define-global "sl" sl-keymap)

    (defun add-to-keymap (keymap bindings)
      (dolist (binding bindings)
	      (define-key keymap (kbd (car binding)) (cdr binding))))

    (defun add-to-dk-keymap (bindings)
      (add-to-keymap dk-keymap bindings))

    (defun add-to-sl-keymap (bindings)
      (add-to-keymap sl-keymap bindings))

    (add-to-dk-keymap
     '(("c" . my/open-literate-private-config-file)
       ("v" . neurosys/open-config-file)
       ("r" . my/edit-resume)
       ("k" . doom/kill-this-buffer-in-all-windows)
       ("n" . narrow-or-widen-dwim)
       ("d" . dired-jump)
       ("b" . my/set-brightness)
       ("<SPC>" . rgrep)
       ("o" . ibuffer)
       ("p" . my/publish-dangirsh.org)
       ("s" . save-buffer)
       ("t" . +vterm/here)
       ("w" . google-this-noconfirm)
       ("x" . sp-splice-sexp)
       ("/" . counsel-recoll)
       ("." . pop-global-mark)))

(key-chord-define-global ",." 'end-of-buffer)
(key-chord-define-global "xz" 'beginning-of-buffer)  ; ergodox
(key-chord-define-global "xc" 'beginning-of-buffer)

(key-chord-define-global "qw" 'delete-window)
(key-chord-define-global "qp" 'delete-other-windows)

(key-chord-define-global "fk" 'other-window)
(key-chord-define-global "jd" 'rev-other-window)

(key-chord-define-global "JJ" 'previous-buffer)
(key-chord-define-global "KK" 'next-buffer)


(key-chord-define-global "hh" 'helpful-at-point)
(key-chord-define-global "hk" 'helpful-key)
(key-chord-define-global "hv" 'helpful-variable)

;; no bueno: e.g. "pathfinder", "highfidelity"
;; (key-chord-define-global "hf" 'helpful-function)

(key-chord-define-global "vn" 'split-window-vertically-and-switch)
(key-chord-define-global "vm" 'split-window-vertically-and-switch)  ; ergodox
(key-chord-define-global "hj" 'split-window-horizontally-and-switch)

(key-chord-define-global "jm" 'my/duplicate-line-or-region)
(key-chord-define-global "fv" 'comment-line)

(key-chord-define-global "kl" 'er/expand-region)

(key-chord-define-global "xx" 'execute-extended-command)
(key-chord-define-global "xf" 'find-file)

(key-chord-define-global "l;" 'repeat)

)

(defun fix-keyboard ()
  (interactive)
  (shell-command "setxkbmap -option 'ctrl:nocaps'")
  (shell-command "xset r rate 160 60"))

(fix-keyboard)

(defun toggle-touchpad ()
  (interactive)
  (shell-command "/home/dan/my-config/scripts/toggle_trackpad.sh"))

(add-to-dk-keymap
   '(("m" . toggle-touchpad)))

(defun my/set-brightness (brightness)
  (interactive "nBrightness level: ")
  (save-window-excursion
    (find-file "/sudo:root@localhost:/sys/devices/pci0000:00/0000:00:02.0/drm/card0/card0-eDP-1/intel_backlight/brightness")
    (kill-region
     (point-min)
     (point-max))
    (insert
     (format "%s" brightness))
    (save-buffer)
    (kill-buffer)))

(use-package! org
  :mode ("\\.org\\'" . org-mode)
  :init
  (add-hook 'org-src-mode-hook #'(lambda () (flycheck-mode 0)))
  (add-hook 'org-mode-hook #'(lambda () (flycheck-mode 0)))
  (map! :map org-mode-map
        "M-n" #'outline-next-visible-heading
        "M-p" #'outline-previous-visible-heading
        "C-c ;" nil)
  (setq org-src-window-setup 'current-window
        org-return-follows-link t
        org-confirm-elisp-link-function nil
        org-confirm-shell-link-function nil
        org-use-speed-commands t
        org-catch-invisible-edits 'show
        ;; Use with consel-org-goto (gh .)
        org-goto-interface 'outline-path-completion
        org-preview-latex-image-directory "/tmp/ltximg/"))


(after! org
  ;; FIXME: Don't know why this isn't loaded automatically...
  (require 'ob-async)
  ;; (add-hook 'ob-async-pre-execute-src-block-hook
  ;;           '(lambda ()
  ;;              (setq inferior-julia-program-name "/usr/local/bin/julia")
  ;;              ;; (setq inferior-julia-program-name "/home/dan/cms-stack/home/julia")
  ;;              ))

  (add-to-list 'org-capture-templates `("l" "Listen" entry (file ,(concat org-directory "listen.org"))
                                        "* TODO %?\n%i"))

  (add-to-list 'org-latex-packages-alist "\\usepackage{braket}")

  ;; http://kitchingroup.cheme.cmu.edu/blog/2015/01/04/Redirecting-stderr-in-org-mode-shell-blocks/
  (setq org-babel-default-header-args:sh
        '((:prologue . "exec 2>&1") (:epilogue . ":")))

  (setq org-babel-default-header-args:jupyter-julia '((:kernel . "julia-1.5")
                                                      (:display . "text/plain")
                                                      (:async . "yes")))

  (setq org-confirm-babel-evaluate nil
        org-use-property-inheritance t
        org-export-with-sub-superscripts nil
        org-startup-indented t
        org-pretty-entities nil
        org-use-speed-commands t
        org-return-follows-link t
        org-outline-path-complete-in-steps nil
        org-ellipsis ""
        org-html-htmlize-output-type 'css
        org-fontify-whole-heading-line t
        org-fontify-done-headline t
        org-fontify-quote-and-verse-blocks t
        org-image-actual-width nil
        org-src-fontify-natively t
        org-src-tab-acts-natively t
        org-src-preserve-indentation t
        org-edit-src-content-indentation 0
        org-adapt-indentation nil
        org-hide-emphasis-markers t
        org-special-ctrl-a/e t
        org-special-ctrl-k t
        org-export-with-broken-links t
        org-yank-adjusted-subtrees t
        org-src-window-setup 'reorganize-frame
        org-src-ask-before-returning-to-edit-buffer nil
        org-insert-heading-respect-content nil)

  (add-hook 'org-babel-after-execute-hook 'org-display-inline-images 'append)
  (add-hook 'org-babel-after-execute-hook 'org-toggle-latex-fragment 'append)

  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
  (add-to-list 'org-structure-template-alist '("sh" . "src sh"))
  (add-to-list 'org-structure-template-alist '("jl" . "src jupyter-julia"))
  (add-to-list 'org-structure-template-alist '("py" . "src jupyter-python"))

  (setq org-refile-use-outline-path 'file
        org-outline-path-complete-in-steps nil
        org-refile-allow-creating-parent-nodes 'confirm)

  (setq org-format-latex-options
        (quote (:foreground default
                :background default
                :scale 2.0
                :matchers ("begin" "$1" "$" "$$" "\\(" "\\["))))

  (setq org-todo-keywords
        '((sequence "TODO(t)" "In-Progress(p)" "|" "DONE(d)")
          (sequence "WAITING(w)" "BLOCKED(b)" "HOLD(h)" "|" "CANCELLED(c)")))

  ;; Colorize org babel output. Without this color codes are left in the output.
  (defun my/display-ansi-colors ()
    (interactive)
    (let ((inhibit-read-only t))
      (ansi-color-apply-on-region (point-min) (point-max))))

  (add-hook 'org-babel-after-execute-hook #'my/display-ansi-colors)

  (advice-add 'org-meta-return :override #'my/org-meta-return))

(use-package! toc-org
  :hook (org-mode . toc-org-mode))

(use-package! org-noter
  :after org
  :config
  ;; helpful in EXWM, where there are no frames
  ;; (customize-set-variable 'org-noter-always-create-frame nil)
  (setq org-noter-notes-window-location 'other-frame
        org-noter-notes-search-path '("~/Sync")
        org-noter-auto-save-last-location t
        org-noter-default-notes-file-names '("~/Sync/pdf_notes.org"))

  ;; This works for assigning PDF paths, but then breaks when trying to find the tpath later.
  ;; (defadvice! better-org-noter--get-or-read-document-property (orig-fn &rest args)
  ;;   :around 'org-noter--get-or-read-document-property
  ;;   (let ((default-directory (if (boundp 'my/noter-default-directory)
  ;;                                my/noter-default-directory
  ;;                              default-directory) ))
  ;;     (apply orig-fn args)))
  )

(use-package! org-recoll
  :after org)

;; Note that this pulls in Helm :/
;; https://github.com/jkitchin/org-ref/issues/202
(use-package! org-ref
  :after (org bibtex)
  :init
  (setq org-ref-default-bibliography '("~/Sync/references.bib"))
  :config
  (setq org-latex-pdf-process
        '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
          "bibtex %b"
          "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
          "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f")
        org-ref-bibliography-notes "~/Sync/pdf_notes.org"
        org-ref-pdf-directory "~/Sync/pdf/"
        org-ref-notes-function #'org-ref-notes-function-one-file)

  (defun get-pdf-filename (key)
    (let ((results (bibtex-completion-find-pdf key)))
      (if (equal 0 (length results))
          (org-ref-get-pdf-filename key)
        (car results))))

  (add-hook 'org-ref-create-notes-hook
            (lambda ()
              (org-entry-put
               nil
               "NOTER_DOCUMENT"
               (get-pdf-filename (org-entry-get
                                  (point) "Custom_ID")))) )

  (defun my/org-ref-noter-at-point ()
    (interactive)
    (let* ((results (org-ref-get-bibtex-key-and-file))
           (key (car results))
           (pdf-file (funcall org-ref-get-pdf-filename-function key))
           (orig-bibtex-dialect bibtex-dialect))
      (if (file-exists-p pdf-file)
          (save-window-excursion
            ;; using the local flag for bibtex-set-dialect doesn't work
            ;; likely because org-ref-open-notes-at-point loses the buffer context
            (bibtex-set-dialect 'BibTeX)
            (org-ref-open-notes-at-point)
            (bibtex-set-dialect orig-bibtex-dialect)
            (find-file-other-window pdf-file)
            (org-noter))
        (message "no pdf found for %s" key))))

  (map! :leader
        :map org-mode-map
        :desc "org-noter from ref"
        "n p" 'my/org-ref-noter-at-point))

(use-package! org-journal
  :after org
  :config
  (customize-set-variable 'org-journal-dir (concat org-roam-directory "journal"))
  (customize-set-variable 'org-journal-file-format "private-%Y-%m-%d.org")
  (customize-set-variable 'org-journal-date-prefix "#+TITLE: ")
  (customize-set-variable 'org-journal-time-prefix "* ")
  (customize-set-variable 'org-journal-time-format "")
  (customize-set-variable 'org-journal-carryover-items "TODO=\"TODO\"")
  (customize-set-variable 'org-journal-date-format "%Y-%m-%d")
  (map! :leader
        (:prefix-map ("n" . "notes")
          (:prefix ("j" . "journal")
            :desc "Today" "t" #'org-journal-today)))
  (defun org-journal-today ()
    (interactive)
    (org-journal-new-entry t)))

(use-package! org-roam
  :commands (org-roam-insert org-roam-find-file org-roam-switch-to-buffer org-roam)
  :hook
  (org-mode . org-roam-mode)
  :custom-face
  (org-roam-link ((t (:inherit org-link))))
  :init
  (require 'org-roam-protocol)
  (map! :leader
        :prefix "n"
        :desc "org-roam" "l" #'org-roam
        :desc "org-roam-insert" "i" #'org-roam-insert
        :desc "org-roam-switch-to-buffer" "b" #'org-roam-switch-to-buffer
        :desc "org-roam-find-file" "f" #'org-roam-find-file
        :desc "org-roam-show-graph" "g" #'org-roam-show-graph
        :desc "org-roam-capture" "c" #'org-roam-capture)
  (key-chord-define-global "[[" #'org-roam-insert)
  (setq org-roam-db-location "/home/dan/Sync/org-roam/org-roam.db"
        org-roam-graph-exclude-matcher "private"))

(setq deft-directory org-roam-directory)
(setq deft-recursive t)

(use-package! org-roam-server
  :config
  (setq org-roam-server-host "127.0.0.1"
        org-roam-server-port 8080
        org-roam-server-authenticate nil
        org-roam-server-label-truncate t
        org-roam-server-label-truncate-length 60
        org-roam-server-label-wrap-length 20))

(use-package! company-org-roam
  :when (featurep! :completion company)
  :after org-roam
  :config
  (set-company-backend! 'org-roam-mode 'company-org-roam))

(use-package! org-roam-bibtex
  :after org-roam
  :hook (org-roam-mode . org-roam-bibtex-mode)
  :bind (:map org-mode-map
         (("C-c n a" . orb-note-actions)))
  :config
  (setq bibtex-completion-library-path "~/Sync/pdf/")
  (setq orb-preformat-keywords
        '(("citekey" . "=key=") "title" "url" "file" "author-or-editor" "keywords"))

  (setq orb-templates
        '(("r" "ref" plain (function org-roam-capture--get-point)
           ""
           :file-name "${citekey}"
           :head "#+TITLE: ${citekey}: ${title}\n#+ROAM_KEY: ${ref}

- tags ::

* ${title}
:PROPERTIES:
:Custom_ID: ${citekey}
:URL: ${url}
:AUTHOR: ${author-or-editor}
:NOTER_DOCUMENT: %(orb-process-file-field \"${citekey}\")
:NOTER_PAGE:
:END:"))))

(setq org-roam-capture-templates
      '(("d" "default" plain (function org-roam--capture-get-point)
         "%?"
         :file-name "%<%Y%m%d%H%M%S>-${slug}"
         :head "#+TITLE: ${title}\n"
         :unnarrowed t
         :immediate-finish t)))

(after! org-roam
  (setq my/org-roam-files (directory-files org-roam-directory  t ".*.org"))
  (setq my/org-roam-todo-file (concat org-roam-directory "todo.org"))
  (setq org-refile-targets `((,(append (my/open-org-files-list) (directory-files org-directory  t ".*.org")) :maxlevel . 7)))
  (setq org-agenda-files `(,my/org-roam-todo-file))

  (defun my/org-roam-get-title (path)
    (save-window-excursion
      ;; A simple find-file didn't work when the original was narrowed
      (with-temp-buffer
        (insert-file-contents path)
        (org-mode)
        (car (org-roam--extract-titles-title)))))

  (add-to-list 'org-capture-templates '("t" "org-roam todo" entry (file my/org-roam-todo-file)
                                        "* TODO %?  #[[%F][%(my/org-roam-get-title \"%F\")]]\n%i\n%a")))

(use-package! org-download
  :config
  ;; take an image that is already on the clipboard
  (customize-set-variable 'org-download-screenshot-method "xclip -selection clipboard -t image/png -o > %s"))

(use-package! org-cliplink)

(use-package! org-drill
  :after org
  :config
  (add-to-list 'org-capture-templates
               `("d" "drill Item" entry
                 (file ,(concat org-directory "drill.org"))
                 "* %^{Heading} :drill:\n\n%^{Question}\n\n** Answer\n\n%^{Answer}")))

(use-package! lispy
  :config
  (advice-add 'delete-selection-pre-hook :around 'lispy--delsel-advice)
  ;; FIXME: magit-blame still fails to all "ret" when lispy is on
  ;; the compat code isn't even getting hit!
  (setq lispy-compat '(edebug magit-blame-mode))

  ;; this hook leaves lispy mode off, but that's not as bad as breaking blame!
  (add-hook 'magit-blame-mode-hook #'(lambda () (lispy-mode 0)))
  :hook
  ((emacs-lisp-mode common-lisp-mode lisp-mode) . lispy-mode)
  :bind (:map lispy-mode-map
          ("'" . nil)             ; leave tick behaviour alone
          ("M-n" . nil)
          ("C-M-m" . nil)))

(use-package! smartparens
  :init
  (map! :map smartparens-mode-map
        "C-M-f" #'sp-forward-sexp
        "C-M-b" #'sp-backward-sexp
        "C-M-u" #'sp-backward-up-sexp
        "C-M-d" #'sp-down-sexp
        "C-M-p" #'sp-backward-down-sexp
        "C-M-n" #'sp-up-sexp
        "C-M-s" #'sp-splice-sexp
        "C-)" #'sp-forward-slurp-sexp
        "C-}" #'sp-forward-barf-sexp
        "C-(" #'sp-backward-slurp-sexp
        "C-M-)" #'sp-backward-slurp-sexp
        "C-M-)" #'sp-backward-barf-sexp))

(use-package! wrap-region
  :hook
  (org-mode . wrap-region-mode)
  (latex-mode . wrap-region-mode)
  :config
  (wrap-region-add-wrappers
   '(("*" "*" nil (org-mode))
     ("~" "~" nil (org-mode))
     ("/" "/" nil (org-mode))
     ("=" "=" nil (org-mode))
     ("_" "_" nil (org-mode))
     ("$" "$" nil (org-mode latex-mode)))))

(use-package! aggressive-indent
  :hook
  (emacs-lisp-mode . aggressive-indent-mode)
  (common-lisp-mode . aggressive-indent-mode))

(use-package! multiple-cursors
              :init
              (setq mc/always-run-for-all t)
              :config
              (add-to-list 'mc/unsupported-minor-modes 'lispy-mode)
              :bind (("C-S-c" . mc/edit-lines)
                     ("C-M-g" . mc/mark-all-like-this-dwim)
                     ("C->" . mc/mark-next-like-this)
                     ("C-<" . mc/mark-previous-like-this)
                     ("C-)" . mc/skip-to-next-like-this)
                     ("C-M->" . mc/skip-to-next-like-this)
                     ("C-(" . mc/skip-to-previous-like-this)
                     ("C-M-<" . mc/skip-to-previous-like-this)))

(use-package! iedit
  :init
  (map! "C-;" 'company-complete)
  (map! "M-i" 'iedit-mode))

(use-package undo-tree
  :init
  (setq undo-tree-visualizer-timestamps t
        undo-tree-visualizer-diff t)
  :config
  ;; stolen from layers/+spacemacs/spacemacs-editing/package.el
  (progn
    ;; restore diff window after quit.  TODO fix upstream
    (defun my/undo-tree-restore-default ()
      (setq undo-tree-visualizer-diff t))
    (advice-add 'undo-tree-visualizer-quit :after #'my/undo-tree-restore-default))
  (global-undo-tree-mode 1))

(defvar inferior-julia-program-name "julia")

(use-package! julia
  :interpreter "julia"
  :hook (julia-mode . julia-repl-mode))

;; (defun my/julia-repl-hook ()
;;   (setq julia-repl-terminal-backend (make-julia-repl--buffer-vterm)))

(use-package! julia-repl
  :config
                                        ; See: https://github.com/tpapp/julia-repl/pull/84
  ;; (require 'vterm)
  ;; (setq julia-repl-terminal-backend (make-julia-repl--buffer-vterm))
  )

;; https://github.com/gcv/julia-snail
(use-package julia-snail
  :hook (julia-mode . julia-snail-mode))

;; (use-package eglot-jl
;;   :hook (julia-mode . eglot)
;;   :config
;;   (eglot-jl-init))

(setq haskell-mode-stylish-haskell-path "brittany")

(use-package! ob-rust)

(use-package! jupyter
  :init
  (setq jupyter-eval-use-overlays t)

  (map!
   :map org-mode-map
   :localleader
   (:desc "Org Hydra"       "j" #'jupyter-org-hydra/body))

  (defun my/insert-julia-src-block ()
    (interactive)
    (jupyter-org-insert-src-block t current-prefix-arg))

  ;; Better than `M-c C-, j` or `M-c j =`
  (key-chord-define-global "j;" #'my/insert-julia-src-block)
  (map!
   :map julia-mode-map
   :localleader
   (:prefix ("j" . "jupyter")
     :desc "Run REPL"         "o" #'jupyter-run-repl
     :desc "Eval function"    "f" #'jupyter-eval-defun
     :desc "Eval buffer"      "b" #'jupyter-eval-buffer
     :desc "Eval region"      "r" #'jupyter-eval-region
     :desc "Restart REPL"     "R" #'jupyter-repl-restart-kernel
     :desc "Interrupt REPL"   "i" #'jupyter-repl-interrup-kernel
     :desc "Scratch buffer"   "s" #'jupyter-repl-scratch-buffer
     :desc "Remove overlays"  "O" #'jupyter-eval-remove-overlays
     :desc "Eval string"      "w" #'jupyter-eval-string
     :desc "Inspect at point" "d" #'jupyter-inspect-at-point)))

(after! ivy
  ;; Causes open buffers and recentf to be combined in ivy-switch-buffer
  (setq ivy-use-virtual-buffers t
        counsel-find-file-at-point t
        ivy-wrap nil
        ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-frame-top-center))
        ivy-posframe-height-alist '((t . 20))
        ivy-posframe-parameters '((internal-border-width . 1))
        ivy-posframe-width 100)
  (add-hook 'eshell-mode-hook
            (lambda ()
              (eshell-cmpl-initialize)
              (define-key eshell-mode-map (kbd "M-r") 'counsel-esh-history)))
  (add-to-dk-keymap
   '(("g" . +ivy/project-search)
     ("h" . +ivy/projectile-find-file)
     ("i" . counsel-semantic-or-imenu)
     ("j" . ivy-switch-buffer))))

(after! dired
  (setq dired-listing-switches "-aBhl  --group-directories-first"
        dired-dwim-target t
        dired-recursive-copies (quote always)
        dired-recursive-deletes (quote top)
        ;; Directly edit permisison bits!
        wdired-allow-to-change-permissions t
        dired-omit-mode nil))

(use-package! dired-narrow
              :commands (dired-narrow-fuzzy)
              :init
              (map! :map dired-mode-map
                    :desc "narrow" "/" #'dired-narrow-fuzzy))

;; Directly edit permisison bits!
(setq wdired-allow-to-change-permissions t)

(use-package! deadgrep
              :if (executable-find "rg")
              :init
              (map! "M-s" #'deadgrep))

(use-package! smartscan
  :init (global-smartscan-mode 1)
  :bind (("M-N" . smartscan-symbol-go-forward)
         ("M-P" . smartscan-symbol-go-backward)
         :map smartscan-map
         ("M-p" . nil)
         ("M-n" . nil)))

(setq vc-ignore-dir-regexp
                (format "\\(%s\\)\\|\\(%s\\)"
                        vc-ignore-dir-regexp
                        tramp-file-name-regexp))

(use-package! magit
  :config
  (set-default 'magit-stage-all-confirm nil)
  (set-default 'magit-unstage-all-confirm nil)

  (remove-hook 'magit-mode-hook 'turn-on-magit-gitflow)

  ;; Restores "normal" behavior in branch view (when hitting RET)
  (setq magit-visit-ref-behavior '(create-branch checkout-any focus-on-ref))

  (setq git-commit-finish-query-functions nil)
  (setq magit-visit-ref-create 1)
  (setq magit-revision-show-gravatars nil))

(after! (magit key-chord)
  (add-to-sl-keymap
   '(("k" . magit-dispatch-popup)
     ("s" . magit-status)
     ("o" . magit-log)
     ("u" . magit-submodule-update)
     ("l" . magit-show-refs-head))))

(after! pdf-tools
  ;;swiper doesn't trigger the pdf-isearch
  (map! :map pdf-isearch-minor-mode-map
        "C-s" 'isearch-forward-regexp))

(use-package! dmenu)

(use-package! ace-window
  :config
  (map! "C-M-SPC" #'ace-window)
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))

(use-package! real-auto-save
  :hook
  (prog-mode . real-auto-save-mode)
  (org-mode . real-auto-save-mode))

(setq swiper-use-visual-line nil)
(setq swiper-use-visual-line-p (lambda (a) nil))

(map!
 "M-p" (lambda () (interactive) (scroll-down 4))
 "M-n" (lambda () (interactive) (scroll-up 4))

 "C-h h" 'helpful-at-point
 "C-h f" 'helpful-function
 "C-h v" 'helpful-variable
 "C-h k" 'helpful-key

 "M-SPC" 'avy-goto-word-or-subword-1

 "C-s" 'swiper-isearch
 ;; "C-M-s" 'swiper-isearch

 "C-S-d" 'my/duplicate-line-or-region
 "C-c <left>" 'winner-undo
 "C-c <right>" 'winner-redo

 "C-+" 'text-scale-increase
 "C--" 'text-scale-decrease

 ;; FIXME: This currently relies on Helm as an undeclared dep!
 "M-y" 'helm-show-kill-ring

 "<f5>" 'my/night-mode
 "<f6>" 'my/day-mode

 "C-z"   'undo-fu-only-undo
 "C-S-z" 'undo-fu-only-redo

 "C-/"   'undo-fu-only-undo
 "C-?" 'undo-fu-only-redo)

(global-set-key [remap goto-line] 'goto-line-with-feedback)
(global-set-key [remap goto-line] 'goto-line-with-feedback)

(flycheck-mode 0)

(setq direnv-always-show-summary nil)

(add-to-list 'auto-mode-alist '("\\.eps\\'" . doc-view-minor-mode))

;; all backup and autosave files in the tmp dir
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; Coordinate between kill ring and system clipboard
(setq save-interprogram-paste-before-kill t)

(setq eshell-history-file-name (concat doom-private-dir "eshell-history"))

;; This is dangerous, but reduces the annoying step of confirming local variable settings each time
;; a file with a "Local Variables" clause (like many Org files) is opened.
(setq enable-local-variables :all)

;; This is usually just annoying
(setq compilation-ask-about-save nil)

;; No confirm on exit
(setq confirm-kill-emacs nil)


;; Help out Projectile for remote files via TRAMP
;; https://sideshowcoder.com/2017/10/24/projectile-and-tramp/
(defadvice projectile-on (around exlude-tramp activate)
  "This should disable projectile when visiting a remote file"
  (unless  (--any? (and it (file-remote-p it))
                   (list
                    (buffer-file-name)
                    list-buffers-directory
                    default-directory
                    dired-directory))
    ad-do-it))

(setq projectile-mode-line "Projectile")

(setq password-store-password-length 20)

;; Truncate compiilation buffers, otherwise Emacs gets slow
;; https://stackoverflow.com/questions/11239201/can-i-limit-the-length-of-the-compilation-buffer-in-emacs
(add-hook 'compilation-filter-hook 'comint-truncate-buffer)
(setq comint-buffer-maximum-size 2000)

(setq recentf-max-saved-items 10000)
