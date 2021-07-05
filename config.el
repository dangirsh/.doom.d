;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(setq user-full-name "Dan Girshovich"
      user-mail-address (rot13 "qna.tvefu@tznvy.pbz"))

(setq my/home-dir "/home/dan/")

(setq my/sync-base-dir (concat my/home-dir "Sync/"))
(setq my/work-base-dir (concat my/home-dir "Work/"))
(setq my/media-base-dir (concat my/home-dir "Media/"))

(setq org-directory my/sync-base-dir
      ;; org-roam-directory "/home/dan/Work/Worldcoin/org-roam"
      org-roam-directory (concat my/sync-base-dir "org-roam/")
      )

(load-file (concat doom-private-dir "funcs.el"))

(setq doom-font (font-spec :family "Hack" :size 20)
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
  (load-theme 'doom-acario-light t)
  ;; (load-theme 'leuven t)
  ;; (load-theme 'doom-dark+ t)
  ;; (load-theme 'doom-solarized-light t)
  ;; (load-theme 'doom-one t)
  ;; (load-theme 'doom-nord-light t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)

  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))


;; Waiting on https://github.com/hlissner/emacs-doom-themes/issues/252
;; Currently, some things like italics and some links in org fail to render correctly.
;; (use-package! poet-theme
;;   :config
;;   (load-theme 'poet))

;; (use-package! almost-mono-themes
;;   :config
;;   ;; (load-theme 'almost-mono-black t)
;;   (load-theme 'almost-mono-white t))

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
   '(("." . pop-global-mark)
     ("/" . org-recoll-search)
     ("<SPC>" . rgrep)
     ("a" . my/org-agenda)
     ("b" . my/set-brightness)
     ("c" . my/open-literate-private-config-file)
     ("d" . dired-jump)
     ("k" . doom/kill-this-buffer-in-all-windows)
     ("m" . my/mathpix-screenshot-to-clipboard)
     ("n" . narrow-or-widen-dwim)
     ("o" . ibuffer)
     ("p" . my/publish-dangirsh.org)
     ("r" . my/edit-resume)
     ("s" . save-buffer)
     ("t" . +vterm/here)
     ("T" . google-translate-at-point)
     ("v" . neurosys/open-config-file)
     ("w" . google-this-noconfirm)
     ("x" . sp-splice-sexp)))

  (key-chord-define-global ",." 'end-of-buffer)
  ;; FIXME: accidentally triggered too often
  (key-chord-define-global "zx" 'beginning-of-buffer)

  (key-chord-define-global "qw" 'delete-window)
  (key-chord-define-global "qp" 'delete-other-windows)
  (key-chord-define-global ",," 'doom/open-scratch-buffer)

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
  (key-chord-define-global "vm" 'split-window-vertically-and-switch) ; ergodox
  (key-chord-define-global "hj" 'split-window-horizontally-and-switch)

  (key-chord-define-global "jm" 'my/duplicate-line-or-region)
  (key-chord-define-global "fv" 'comment-line)

  (key-chord-define-global "kl" 'er/expand-region)

  (key-chord-define-global "xx" 'execute-extended-command)
  (key-chord-define-global "xf" 'find-file)

  (key-chord-define-global "jp" 'my/insert-jupyter-python-block))

(defun fix-keyboard ()
  (interactive)
  (shell-command "setxkbmap -option 'ctrl:nocaps'")
  (shell-command "xset r rate 160 60"))

(defun toggle-touchpad ()
  (interactive)
  (shell-command "/home/dan/my-config/scripts/toggle_trackpad.sh"))

(setq my/brightness-min 1)
(setq my/brightness-max 100)
(setq my/brightness-step 5)

(defun my/get-brightness ()
  (* my/brightness-step (round (string-to-number
                                (shell-command-to-string "xbacklight -get"))
                               my/brightness-step)))

(defun my/set-brightness (level)
  (interactive "n:")
  (let ((safe-level
         (cond ((< level my/brightness-min) my/brightness-min)
               ((> level my/brightness-max) my/brightness-max)
               (t level))))
    (save-window-excursion
      (shell-command
       (format "xbacklight -set %s &" safe-level) nil nil))))

(defun my/brightness-step-change (delta)
  (my/set-brightness (+ delta (my/get-brightness))))

(defun my/brightness-increase ()
  (interactive)
  (my/brightness-step-change my/brightness-step))

(defun my/brightness-decrease ()
  (interactive)
  (my/brightness-step-change (- my/brightness-step)))

(map! "<XF86MonBrightnessDown>" 'my/brightness-decrease)
(map! "<XF86MonBrightnessUp>" 'my/brightness-increase)

(setq my/volume-min 1)
(setq my/volume-max 100)
(setq my/volume-step 5)

(defun my/get-volume ()
  (* my/volume-step (round (string-to-number
                                (shell-command-to-string "awk -F\"[][]\" '/dB/ { print $2 }' <(amixer sget Master)"))
                               my/volume-step)))

(defun my/set-volume (level)
  (interactive "n:")
  (let ((clipped-level
         (cond ((< level my/volume-min) my/volume-min)
               ((> level my/volume-max) my/volume-max)
               (t level))))
    (save-window-excursion
      (shell-command
       (format "amixer set Master %s%% &" clipped-level) nil nil))))

(defun my/volume-step-change (delta)
  (my/set-volume (+ delta (my/get-volume))))

(defun my/volume-increase ()
  (interactive)
  (my/volume-step-change my/volume-step))

(defun my/volume-decrease ()
  (interactive)
  (my/volume-step-change (- my/volume-step)))

(map! "<XF86AudioRaiseVolume>" 'my/volume-increase)
(map! "<XF86AudioLowerVolume>" 'my/volume-decrease)

(defun my/connect-to-bose-700s ()
  (interactive)
  (shell-command "bluetoothctl -- connect 4C:87:5D:27:B8:63"))

(defun my/disconnect-to-bose-700s ()
  (interactive)
  (shell-command "bluetoothctl -- disconnect 4C:87:5D:27:B8:63"))

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
        org-preview-latex-image-directory "/tmp/ltximg/")
  (setq org-file-apps '((auto-mode . emacs)
                        (directory . emacs)
                        ("\\.mm\\'" . default)
                        ("\\.x?html?\\'" . default)
                        ("\\.pdf\\'" . (lambda (file link) (org-pdftools-open link))))))


(after! org
  ;; FIXME: Don't know why this isn't loaded automatically...
  (require 'ob-async)

  ;; Clear Doom's default templates
  (setq org-capture-templates '())

  (add-to-list 'org-capture-templates `("l" "Listen" entry (file ,(concat org-directory "listen.org"))
                                        "* TODO %?\n%i"))

  (add-to-list 'org-latex-packages-alist "\\usepackage{braket}")

  ;; http://kitchingroup.cheme.cmu.edu/blog/2015/01/04/Redirecting-stderr-in-org-mode-shell-blocks/
  ;; NOTE: This will affect (break) tangled output. Use directly on top of code blocks when needed instead.
  ;; TODO: Figure out how to keep this without adding it to tangled output.
  ;; (setq org-babel-default-header-args:sh
  ;;       '((:prologue . "exec 2>&1") (:epilogue . ":")))

  (setq org-babel-default-header-args:jupyter-julia '((:kernel . "julia-1.6")
                                                      (:display . "text/plain")
                                                      (:async . "yes")))

  (setq org-confirm-babel-evaluate nil
        org-use-property-inheritance t
        org-export-with-sub-superscripts nil
        org-export-use-babel nil
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
        '((sequence "TODO(t)" "WIP(p)" "WAITING(w)" "DELEGATED(o)" "SOMEDAY(s)" "QUESTION(q)" "|" "DONE(d)" "CANCELLED(c)")))

  ;; Colorize org babel output. Without this color codes are left in the output.
  (defun my/display-ansi-colors ()
    (interactive)
    (let ((inhibit-read-only t))
      (ansi-color-apply-on-region (point-min) (point-max))))

  (add-hook 'org-babel-after-execute-hook #'my/display-ansi-colors)

  (advice-add 'org-meta-return :override #'my/org-meta-return)
  (setq org-tags-match-list-sublevels 'indented)

  (setq org-image-actual-width nil)

  (setq org-agenda-files '())
  (setq org-agenda-prefix-format '((agenda . " %i %-12:c%?-12t% s")
                                   (todo . " %i %b")
                                   (tags . " %i %-12:c %b")
                                   (search . " %i %-12:c %b")))
  (setq org-agenda-category-icon-alist
        `(("Personal" ,(list (all-the-icons-material "home" :height 1.2)) nil nil :ascent center)
          ("Incoming" ,(list (all-the-icons-material "move_to_inbox" :height 1.2)) nil nil :ascent center)))
  )

(use-package! toc-org
  :hook (org-mode . toc-org-mode))

(use-package! org-noter
  :after org
  :config
  ;; helpful in EXWM, where there are no frames
  (customize-set-variable 'org-noter-always-create-frame t)
  (customize-set-variable 'org-noter-notes-window-behavior '(start))
  (customize-set-variable 'org-noter-notes-window-location 'horizontal-split)
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

(after! org-roam
  (add-hook 'org-journal-mode 'org-roam-mode)
  ;; Globally accessible commands
  (map! :leader
        :prefix "n"
        :desc "org-roam-find-file" "f" #'org-roam-find-file)
  (set-company-backend! 'org-roam-mode 'company-capf)
  (setq org-roam-db-location "/home/dan/Sync/org-roam/org-roam.db"
        +org-roam-open-buffer-on-find-file nil
        org-id-link-to-org-use-id t
        org-roam-graph-exclude-matcher '("private" "todo" "daily")))

(defun my/org-roam-search ()
  "Search org-roam directory using consult-ripgrep. With live-preview."
  (interactive)
  (let ((consult-ripgrep-command "rg --null --ignore-case --type org --line-buffered --color=always --max-columns=500 --no-heading --line-number . -e ARG OPTS"))
    (consult-ripgrep org-roam-directory)))

(map! "<f8>" 'my/org-roam-search)

(use-package! org-roam-server
  :config
  (setq org-roam-server-host "127.0.0.1"
        org-roam-server-port 8081
        org-roam-server-authenticate nil
        org-roam-server-export-inline-images t
        org-roam-server-serve-files nil
        org-roam-server-served-file-extensions '("pdf" "mp4" "ogv")
        org-roam-server-network-poll t
        org-roam-server-network-arrows nil
        org-roam-server-network-label-truncate t
        org-roam-server-network-label-truncate-length 60
        org-roam-server-network-label-wrap-length 20))

(after! org-roam
  (setq my/org-roam-todo-file (concat org-roam-directory "orgzly/todo.org"))
  (setq org-refile-targets `((,(append (my/open-org-files-list) (directory-files org-directory  t ".*.org")) :maxlevel . 7)))
  (add-to-list 'org-agenda-files my/org-roam-todo-file)
  (add-to-list 'org-capture-templates '("t" "Todo" entry (file my/org-roam-todo-file)
                                        "* TODO %?"))
  (add-to-list 'org-capture-templates '("T" "Todo with Context" entry (file my/org-roam-todo-file)
                                        "* TODO %?  #[[%F][%(my/org-roam-get-title \"%F\")]]\n%i\n%a"))
  )

(use-package! org-transclusion
  :hook (org-roam-mode . org-transclusion-mode))

(use-package! org-download
  :config
  ;; take an image that is already on the clipboard
  (customize-set-variable 'org-download-screenshot-method "xclip -selection clipboard -t image/png -o > %s"))

(use-package! org-cliplink)

(use-package! org-drill
  :after org
  :config
  (add-to-list 'org-capture-templates
               `("d" "Drill" entry
                 (file ,(concat org-directory "drill.org"))
                 "* %^{Heading} :drill:\n\n%^{Question}\n\n** Answer\n\n%^{Answer}")))

(setq org-agenda-start-day "+0d"      ; start today
      org-agenda-show-current-time-in-grid t
      org-agenda-timegrid-use-ampm t
      org-agenda-use-time-grid nil    ; Toggle it with 'G' in agenda view
      org-agenda-span 3)

(add-to-list 'org-agenda-files "~/Sync/org-roam/orgzly/boox-incoming.org")
(add-to-list 'org-agenda-files "~/Sync/org-roam/orgzly/pixel-incoming.org")

(defun my/org-agenda ()
  (interactive)
  (org-agenda nil "n"))

(use-package! org-super-agenda
  :after org-agenda
  :config
  (setq org-super-agenda-groups
        '((:discard (:todo "SOMEDAY"))
          (:discard (:todo "QUESTION"))
          (:name "WIP"
           :todo "WIP")
          ;; https://github.com/alphapapa/org-super-agenda/issues/192
          (:name "High Priority"
           :priority "A")
          (:name "Med Priority"
           :priority "B")
          (:name "Low Priority"
           :priority "C")
          (:name "Today"
           ;; :time-grid t
           :scheduled today
           :deadline today)
          (:auto-todo t)))
  (org-super-agenda-mode))

(defun my/open-questions ()
  (interactive)
  (let ((org-super-agenda-groups
         '((:discard (:not (:todo "QUESTION")))
           (:auto-todo t))))

    (org-agenda nil "t")))

(after! tramp
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path))

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

;; (use-package! smartparens
;;   :init
;;   (map! :map smartparens-mode-map
;;         "C-M-f" #'sp-forward-sexp
;;         "C-M-b" #'sp-backward-sexp
;;         "C-M-u" #'sp-backward-up-sexp
;;         "C-M-d" #'sp-down-sexp
;;         "C-M-p" #'sp-backward-down-sexp
;;         "C-M-n" #'sp-up-sexp
;;         "C-M-s" #'sp-splice-sexp
;;         ;; conflicts with mc
;;         ;; "C-)" #'sp-forward-slurp-sexp
;;         "C-}" #'sp-forward-barf-sexp
;;         ;; conflicts with mc
;;         ;; "C-(" #'sp-backward-slurp-sexp
;;         "C-M-)" #'sp-backward-slurp-sexp
;;         "C-M-)" #'sp-backward-barf-sexp))

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

(defun setup-mathpix ()
  (load-file (concat doom-private-dir "mathpix.el"))
  (require 'mathpix)
  (customize-set-variable 'mathpix-app-id "dan_girsh_gmail_com_5d68dc")
  (customize-set-variable 'mathpix-app-key "600336b7b2b932549ce4")
  (customize-set-variable 'mathpix-screenshot-method "scrot -s %s"))

(setup-mathpix)

(defun my/mathpix-screenshot-to-clipboard ()
  (interactive)
  (with-temp-buffer
    (mathpix-screenshot)
    (kill-new
     (format "$$\n%s\n$$" (buffer-string)))))

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

(use-package! undo-tree
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

(setq haskell-mode-stylish-haskell-path "brittany")

;; https://robert.kra.hn/posts/2021-02-07_rust-with-emacs/
(after! rustic
  (map! :map rustic-mode-map
        "M-j" #'lsp-ui-imenu
        "M-?" #'lsp-find-references
        "C-c C-c l" #'flycheck-list-errors
        "C-c C-c a" #'lsp-execute-code-action
        "C-c C-c r" #'lsp-rename
        "C-c C-c q" #'lsp-workspace-restart
        "C-c C-c Q" #'lsp-workspace-shutdown
        "C-c C-c s" #'lsp-rust-analyzer-status)

  (setq lsp-enable-symbol-highlighting nil)
  ;; (setq rustic-format-on-save t)
  ;; (setq rustic-format-trigger 'on-save)
  (setq lsp-rust-analyzer-server-display-inlay-hints t)
  (add-hook 'rustic-mode-hook 'my/rustic-mode-hook))

(defun my/rustic-mode-hook ()
  ;; so that run C-c C-c C-r works without having to confirm
  (setq-local buffer-save-without-query t))

(use-package! ob-rust)

(defun start-file-process-shell-command@around (start-file-process-shell-command name buffer &rest args)
  "Start a program in a subprocess.  Return the process object for it. Similar to `start-process-shell-command', but calls `start-file-process'."
  ;; On remote hosts, the local `shell-file-name' might be useless.
  (let ((command (mapconcat 'identity args " ")))
    (funcall start-file-process-shell-command name buffer command)))

(advice-add 'start-file-process-shell-command :around #'start-file-process-shell-command@around)

(with-eval-after-load "lsp-rust"
  (lsp-register-client
   (make-lsp-client
    :new-connection (lsp-tramp-connection "rust-analyzer")
    :remote? t
    :major-modes '(rust-mode rustic-mode)
    :initialization-options 'lsp-rust-analyzer--make-init-options
    :notification-handlers (ht<-alist lsp-rust-notification-handlers)
    :action-handlers (ht ("rust-analyzer.runSingle" #'lsp-rust--analyzer-run-single))
    :library-folders-fn (lambda (_workspace) lsp-rust-library-directories)
    :after-open-fn (lambda ()
                     (when lsp-rust-analyzer-server-display-inlay-hints
                       (lsp-rust-analyzer-inlay-hints-mode)))
    :ignore-messages nil
    :server-id 'rust-analyzer-remote)))

(defun my/register-remote-rust-analyzer ()
  (interactive)
  (lsp-register-client
   (make-lsp-client
    :new-connection (lsp-tramp-connection "rust-analyzer")
    ;; (lsp-tramp-connection
    ;;  (lambda ()
    ;;    `(,(or (executable-find
    ;;            (cl-first lsp-rust-analyzer-server-command))
    ;;           (lsp-package-path 'rust-analyzer)
    ;;           "rust-analyzer")
    ;;      ,@(cl-rest lsp-rust-analyzer-server-args))))
    :remote? t
    :major-modes '(rust-mode rustic-mode)
    :initialization-options 'lsp-rust-analyzer--make-init-options
    :notification-handlers (ht<-alist lsp-rust-notification-handlers)
    :action-handlers (ht ("rust-analyzer.runSingle" #'lsp-rust--analyzer-run-single))
    :library-folders-fn (lambda (_workspace) lsp-rust-library-directories)
    :after-open-fn (lambda ()
                     (when lsp-rust-analyzer-server-display-inlay-hints
                       (lsp-rust-analyzer-inlay-hints-mode)))
    :ignore-messages nil
    :server-id 'rust-analyzer-remote)))

(use-package! jupyter
  :init
  (setq jupyter-eval-use-overlays t)

  (map!
   :map org-mode-map
   :localleader
   (:desc "Jupyter Org Hydra"       "j" #'jupyter-org-hydra/body))

  (defun my/insert-julia-src-block ()
    (interactive)
    (jupyter-org-insert-src-block t current-prefix-arg))

  ;; I locally modified jupyter-completion-at-point to check for this,
  ;; since completions regularly crash the julia kernel for me :/
  (setq my/jupyter-enable-completions nil)

  ;; Better than `M-c C-, j` or `M-c j =`
  (key-chord-define-global "jq" #'my/insert-julia-src-block)
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

(use-package! selectrum
  :config
  (selectrum-mode +1)
  (setq selectrum-highlight-candidates-function #'orderless-highlight-matches))

(use-package! orderless
  :custom (completion-styles '(orderless)))

(use-package! selectrum-prescient
  :after (selectrum)
  :config
  (setq selectrum-prescient-enable-filtering nil)
  (selectrum-prescient-mode +1)
  (prescient-persist-mode +1))

(use-package! ctrlf
  :init
  (ctrlf-mode +1))

(use-package! mini-frame
  :init
  (mini-frame-mode +1)
  (setq resize-mini-frames t)
  (setq mini-frame-show-parameters
        '((top . 25)
          (height . 1)
          (width . 0.7)
          (left . 0.5)))
  ;; (dolist (cmd '(consult-line
  ;;                consult-ripgrep))
  ;;   (add-to-list 'mini-frame-ignore-commands cmd))
  )

(use-package! consult
  :init
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
  (add-to-dk-keymap
   '(("<SPC>" . consult-ripgrep)
     ("g" . consult-git-grep)
     ("i" . consult-imenu)
     ("l" . consult-locate)
     ("j" . consult-buffer)))
  (map! :localleader
        :map org-mode-map
        ;; override default binding for org-goto
        "." 'consult-outline)
  :config
  (setq consult-narrow-key "<")
  (setq consult-async-split-style 'space)
  (autoload 'projectile-project-root "projectile")
  (setq consult-project-root-function #'projectile-project-root)
  :bind (;; C-c bindings (mode-specific-map)
         ("C-c h" . consult-history)
         ("C-c m" . consult-mode-command)
         ("C-c b" . consult-bookmark)
         ("C-c k" . consult-kmacro)
         ;; C-x bindings (ctl-x-map)
         ("C-x M-:" . consult-complex-command) ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)            ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame) ;; orig. switch-to-buffer-other-frame
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store) ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ;; Other custom bindings

         ;; only differs from built-in yank-pop by a preview mechanism
         ;; the previews break pasting into vterm, so disabling for now
         ;; ("M-y" . consult-yank-pop)     ;; orig. yank-pop

         ("<help> a" . consult-apropos) ;; orig. apropos-command
         ;; M-g bindings (goto-map)
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)     ;; Alternative: consult-flycheck
         ("M-g g" . consult-goto-line)   ;; orig. goto-line
         ("M-g M-g" . consult-goto-line) ;; orig. goto-line
         ("M-g o" . consult-outline)     ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-project-imenu)
         ;; M-s bindings (search-map)
         ("M-s f" . consult-find)
         ("M-s L" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s m" . consult-multi-occur)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch)
         :map isearch-mode-map
         ("M-s e" . consult-isearch) ;; orig. isearch-edit-string
         ("M-s l" . consult-line))   ;; needed by consult-line to detect isearch
  )


(use-package! consult-flycheck
  :bind (:map flycheck-command-map
         ("!" . consult-flycheck)))


(use-package! consult-projectile
  :config
  (add-to-dk-keymap
   '(("h" . consult-projectile))))


(consult-customize consult-buffer consult-ripgrep
                   consult-git-grep consult-grep consult-bookmark
                   consult-recent-file consult-xref consult--source-file
                   consult--source-project-file consult--source-bookmark
                   consult-theme
                   :preview-key
                   (list (kbd "M-.")
                         :debounce 0.5 (kbd "<up>") (kbd "<down>")
                         :debounce 1 'any))

(consult-customize
 consult--source-file consult--source-project-file consult--source-bookmark
 :preview-key (kbd "M-."))

(use-package! marginalia
  :init (marginalia-mode)
  :bind
  (("M-A" . marginalia-cycle)
   :map minibuffer-local-map
   ("M-A" . marginalia-cycle)))

(use-package! embark
  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ("M-<RET>" . embark-dwim)    ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'
  :init
  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)
  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none))))
  (setq embark-prompter 'embark-completing-read-prompter)
  )

;; Consult users will also want the embark-consult package.
(use-package! embark-consult
  :after (embark consult)
  :demand t ; only necessary if you have the hook below
  ;; if you want to have consult previews as you move around an
  ;; auto-updating embark collect buffer
  :hook
  (embark-collect-mode . embark-consult-preview-minor-mode))

(after! dired
  (setq dired-listing-switches "-aBhl  --group-directories-first"
        dired-dwim-target t
        dired-recursive-copies (quote always)
        dired-recursive-deletes (quote top)
        ;; Directly edit permisison bits!
        wdired-allow-to-change-permissions t))

(use-package! dired-narrow
              :commands (dired-narrow-fuzzy)
              :init
              (map! :map dired-mode-map
                    :desc "narrow" "/" #'dired-narrow-fuzzy))

;; Directly edit permisison bits!
(setq wdired-allow-to-change-permissions t)

(use-package! deadgrep)

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
  (map! :map pdf-isearch-minor-mode-map
        "C-s" 'isearch-forward-regexp))

(use-package! dmenu)

(use-package! ace-window
  :config
  (map! "C-M-SPC" #'ace-window)
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))

;; prevents horizontal splits when split-window-sensibly is used
(setq split-width-threshold nil)

(use-package! real-auto-save
  :hook
  (prog-mode . real-auto-save-mode)
  (org-mode . real-auto-save-mode))

(use-package! google-translate
  :custom
  (google-translate-backend-method 'curl)
  :config
  (defun google-translate--search-tkk () "Search TKK." (list 430675 2721866130))
  (setq google-translate-output-destination 'kill-ring))

(use-package! string-inflection)

(use-package! dotenv)

(defun my/load-env-file (env-file)
  (interactive "f")
  (dotenv-update-env (dotenv-load% env-file)))

(use-package! logview)

(use-package! wgrep)

(load-file "/home/dan/Work/Worldcoin/emacs/worldcoin-setup.el")
(require 'worldcoin-setup)

(map!
 "M-p" (lambda () (interactive) (scroll-down 4))
 "M-n" (lambda () (interactive) (scroll-up 4))

 "C-h h" 'helpful-at-point
 "C-h f" 'helpful-function
 "C-h v" 'helpful-variable
 "C-h k" 'helpful-key

 "M-SPC" 'avy-goto-word-or-subword-1

 "C-S-d" 'my/duplicate-line-or-region
 "C-c <left>" 'winner-undo
 "C-c <right>" 'winner-redo

 "C-+" 'text-scale-increase
 "C--" 'text-scale-decrease

 "C-<f5>" 'my/night-mode
 "C-<f6>" 'my/day-mode

 "C-z"   'undo-fu-only-undo
 "C-S-z" 'undo-fu-only-redo

 "C-/"   'undo-fu-only-undo
 "C-?" 'undo-fu-only-redo)

;; (global-set-key [remap goto-line] 'goto-line-with-feedback)
;; (global-set-key [remap goto-line] 'goto-line-with-feedback)

(setq warning-minimum-level :emergency)

(setq isearch-allow-scroll t)

(setq async-shell-command-buffer 'new-buffer)

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

;; Alternative to calling save-buffers-kill-emacs, since
;; a) Muscle memory sends me to "kill-emacs" via fj-q-q
;; b) save-buffers-kill-emacs sometimes fails
;; This way, we try to save things, but quit in any case.
(defun my/save-ignore-errors ()
  (ignore-errors
    (save-some-buffers)))

(add-hook 'kill-emacs-hook 'my/save-ignore-errors)


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
 mad-do-it))

(setq projectile-mode-line "Projectile")

(setq password-store-password-length 20)

;; Truncate compiilation buffers, otherwise Emacs gets slow
;; https://stackoverflow.com/questions/11239201/can-i-limit-the-length-of-the-compilation-buffer-in-emacs
(add-hook 'compilation-filter-hook 'comint-truncate-buffer)
(setq comint-buffer-maximum-size 2000)

(setq recentf-max-saved-items 10000)

(after! vterm
  (setq vterm-buffer-name-string "vterm: %s")
  (setq vterm-copy-exclude-prompt t))
