;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(setq user-full-name "Dan Girshovich"
      user-mail-address (rot13 "qna.tvefu@tznvy.pbz"))

(setq my/home-dir "/home/dan/")

(setq my/sync-base-dir (concat my/home-dir "Sync/"))
(setq my/work-base-dir (concat my/home-dir "Work/"))
(setq my/media-base-dir (concat my/home-dir "Media/"))

(setq org-directory my/sync-base-dir
      org-roam-directory "/home/dan/Sync/org-roam2/"
      org-roam-db-location (concat org-roam-directory "org-roam.db")
      my/org-roam-todo-file (concat org-roam-directory "orgzly/todo.org"))

(save-window-excursion
  (find-file my/org-roam-todo-file)
  (save-buffer))

(setq my/brightness-min 1)
(setq my/brightness-max 100)
(setq my/brightness-step 5)

(defun my/set-brightness (level)
  (interactive "nBrightness level: ")
  (let ((safe-level
         (cond ((< level my/brightness-min) my/brightness-min)
               ((> level my/brightness-max) my/brightness-max)
               (t level))))
    (save-window-excursion
      (shell-command
       (format "sudo light -S %s" safe-level) nil nil))))

(load-file (concat doom-private-dir "funcs.el"))

(setq
 doom-font (font-spec :family "Iosevka" :size 22)
 doom-variable-pitch-font (font-spec :family "Libre Baskerville")
 doom-serif-font (font-spec :family "Libre Baskerville"))

(setq display-line-numbers-type nil)

;; Thin grey line separating windows
(set-face-background 'vertical-border "grey")
(set-face-foreground 'vertical-border (face-background 'vertical-border))

(use-package! doom-themes
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)
  (load-theme 'doom-one t)
  (doom-themes-visual-bell-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

(use-package! key-chord
  :config
  (key-chord-mode 1)
  (setq key-chord-one-key-delay 0.20 ; same key (e.g. xx)
        key-chord-two-keys-delay 0.04)
  (customize-set-variable 'key-chord-safety-interval-forward 0.0)
  (customize-set-variable 'key-chord-safety-interval-backward 0.0))

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
  ;; My external keyboard (Voyager) supports chords in the firmware
  ;; For some cases, I find it less error prone to use these instead of
  ;; keychord.el. In these cases, the keyboard sends a function key (e.g. f13)

  (key-chord-define-global "pl" 'send-doom-leader)
  ;; (global-set-key (kbd "<XF86Launch7>") 'send-doom-leader)
  (key-chord-define-global "bj" 'send-doom-local-leader)

  (setq dk-keymap (make-sparse-keymap))

  (key-chord-define-global "fu" dk-keymap)
  (global-set-key (kbd "<XF86Launch6>") dk-keymap)

  (defun add-to-keymap (keymap bindings)
    (dolist (binding bindings)
      (define-key keymap (kbd (car binding)) (cdr binding))))

  (defun add-to-dk-keymap (bindings)
    (add-to-keymap dk-keymap bindings))

  (add-to-dk-keymap
   '(("." . jump-to-register)
     ("<SPC>" . rgrep)
     ("a" . my/org-agenda)
     ("b" . my/set-brightness)
     ("c" . my/open-literate-private-config-file)
     ("d" . dired-jump)
     ("k" . doom/kill-this-buffer-in-all-windows)
     ("m" . magit-status)
     ("n" . narrow-or-widen-dwim)
     ("s" . save-buffer)
     ("t" . +vterm/here)
     ("v" . neurosys/open-config-file)
     ("w" . google-this-noconfirm)))

  (key-chord-define-global ",." 'end-of-buffer)
  (key-chord-define-global "xc" 'beginning-of-buffer)
  (key-chord-define-global "zx" 'beginning-of-buffer)

  (key-chord-define-global "qw" 'delete-window)
  (key-chord-define-global "q;" 'delete-other-windows)
  (key-chord-define-global ",," 'doom/open-scratch-buffer)

  (key-chord-define-global "pu" 'other-window)
  (key-chord-define-global "fl" 'rev-other-window)

  (global-set-key (kbd "<XF86Launch5>") 'other-window)
  (global-set-key (kbd "<XF86Tools>") 'rev-other-window)


  ;; (key-chord-define-global "dh" 'split-window-vertically-and-switch)
  (global-set-key (kbd "<XF86Launch7>") 'split-window-vertically-and-switch)
  (key-chord-define-global "mn" 'split-window-horizontally-and-switch)

  (key-chord-define-global "nh" 'my/duplicate-line-or-region)
  (key-chord-define-global "td" 'comment-line)

  (key-chord-define-global "uy" 'er/expand-region)

  (key-chord-define-global "xx" 'execute-extended-command)
  ;; (key-chord-define-global "xt" 'ffap)
  )

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
        org-catch-invisible-edits 'show
        ;; Use with consel-org-goto (gh .)
        org-goto-interface 'outline-path-completion)
  (setq org-file-apps '((auto-mode . emacs)
                        (directory . emacs)
                        ("\\.mm\\'" . default)
                        ("\\.x?html?\\'" . default)
                        ("\\.pdf\\'" . (lambda (file link) (org-pdftools-open link))))))

(after! org
  ;; FIXME: Don't know why this isn't loaded automatically...
  (require 'ob-async)

  (setq org-capture-templates '())

  (setq org-confirm-babel-evaluate nil
        org-use-property-inheritance t
        org-export-use-babel nil
        org-pretty-entities nil
        org-use-speed-commands t
        org-return-follows-link t
        org-outline-path-complete-in-steps nil
        org-ellipsis ""
        org-fontify-whole-heading-line t
        org-fontify-done-headline t
        org-fontify-quote-and-verse-blocks t
        org-image-actual-width nil
        org-src-fontify-natively t
        org-src-tab-acts-natively t
        org-startup-indented t
        org-src-preserve-indentation t
        org-edit-src-content-indentation 0
        org-adapt-indentation nil
        org-hide-emphasis-markers t
        org-special-ctrl-a/e t
        org-special-ctrl-k t
        org-yank-adjusted-subtrees t
        org-src-window-setup 'reorganize-frame
        org-src-ask-before-returning-to-edit-buffer nil
        org-insert-heading-respect-content nil)

  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
  (add-to-list 'org-structure-template-alist '("sh" . "src sh"))
  (add-to-list 'org-structure-template-alist '("r" . "src rust"))
  (add-to-list 'org-structure-template-alist '("py" . "src jupyter-python"))

  (setq org-refile-use-outline-path 'file
        org-outline-path-complete-in-steps nil
        org-refile-allow-creating-parent-nodes 'confirm)

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

  (setq org-todo-keywords
        '((sequence
           "TODO(t)"
           "WAIT(w)"
           "HOLD(h)"
           "IDEA(i)"
           "DELEGATED(e)"
           "|"
           "DONE(d)"
           "KILL(k)")
          )
        org-todo-keyword-faces
        '(("WAIT" . +org-todo-onhold)
          ("HOLD" . +org-todo-onhold)
          ("DELEGATED" . +org-todo-onhold)
          ("KILL" . +org-todo-cancel)))

  ;; Update parent TODO state when all children TODOs are done
  ;; NOTE: Only works if the parent has a "[/]" or "[%]" in the heading!!
  ;; https://orgmode.org/manual/Breaking-Down-Tasks.html#Breaking-Down-Tasks
  (defun org-summary-todo (n-done n-not-done)
    "Switch entry to DONE when all subentries are done, to TODO otherwise."
    (let (org-log-done org-log-states)  ; turn off logging
      (org-todo (if (= n-not-done 0) "DONE" "TODO"))))

  (add-hook 'org-after-todo-statistics-hook 'org-summary-todo)

  ;; (add-to-list 'org-agenda-files "~/Sync/org-roam/orgzly/boox-incoming.org")
  (add-to-list 'org-agenda-files "~/Sync/org-roam2/orgzly/pixel-incoming.org")
  (add-to-list 'org-agenda-files "~/Sync/org-roam2/orgzly/incoming.org")

  (add-to-list 'org-latex-default-packages-alist "\\PassOptionsToPackage{hyphens}{url}")
  (require 'ox-latex))


;; Setup syntax highlighting for code block pdf exports
;; (after! ox-latex
;;   (setq org-latex-pdf-process
;;         '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f")
;;         org-latex-listings 'minted
;;         org-latex-packages-alist '(("" "minted"))))

(use-package! toc-org
  :hook (org-mode . toc-org-mode))

(defun my/org-roam-capture-new-node-hook ()
  (org-entry-put (point) "header-args" ":noweb yes"))

(after! org-roam
  (setq +org-roam-open-buffer-on-find-file nil
        org-id-link-to-org-use-id t
        org-roam-mode-section-functions (list #'org-roam-backlinks-section
                                              #'org-roam-reflinks-section
                                              #'org-roam-unlinked-references-section))
  (add-hook 'org-roam-capture-new-node-hook 'my/org-roam-capture-new-node-hook))

(after! org-roam-dailies
  (setq org-roam-dailies-directory "daily/")

  (setq org-roam-dailies-capture-templates
        '(("d" "default" entry
           "* %?"
           :if-new (file+head "%<%Y-%m-%d>.org"
                              "#+TITLE: %<%Y-%m-%d>\n#+FILETAGS: daily")))))

(add-to-dk-keymap
 '(("J" . org-roam-dailies-goto-today)))

;; leader-n-r-d-t also works, but this muscle-memory from the org-journal days is easier to type
(map! :leader
      (:prefix-map ("n" . "notes")
       (:prefix ("j" . "journal")
        :desc "Today" "j" #'my/today)))

(after! org
  (add-to-list 'org-agenda-files my/org-roam-todo-file)
  (add-to-list 'org-capture-templates '("t" "Todo" entry (file my/org-roam-todo-file)
                                        "* TODO %?"))
  (add-to-list 'org-capture-templates '("T" "Todo with Context" entry (file my/org-roam-todo-file)
                                        "* TODO %?  #[[%F][%(my/org-get-title \"%F\")]]\n%i\n%a"))
  )

(setq org-agenda-start-day "+0d"        ; start today
      org-agenda-show-current-time-in-grid nil
      org-agenda-timegrid-use-ampm t
      org-agenda-use-time-grid nil      ; Toggle it with 'G' in agenda view
      org-agenda-span 3
      org-agenda-skip-timestamp-if-done t
      org-agenda-skip-deadline-if-done t
      org-agenda-overriding-header "⚡ Agenda"
      org-agenda-prefix-format '((agenda . " %i %-12:c%?-12t% s")
                                 (todo . " %i %b")
                                 (tags . " %i %-12:c %b")
                                 (search . " %i %-12:c %b"))
      org-agenda-category-icon-alist
      `(("Personal" ,(list (nerd-icons-mdicon "nf-md-home" :height 1.2)) nil nil :ascent center)
        ("Incoming" ,(list (nerd-icons-mdicon "nf-md-inbox_arrow_down" :height 1.2)) nil nil :ascent center))
      org-agenda-todo-keyword-format "%-1s"
      org-agenda-scheduled-leaders '("" "")
      org-agenda-deadline-leaders '("Deadline:  " "In %3d d.: " "%2d d. ago: ")

      org-priority-highest 1
      org-priority-lowest 5
      org-priority-default 3)

(customize-set-variable 'org-priority-faces '((49 . error)
                                              (50 . warning)
                                              (51 . success)
                                              (52 . success)
                                              (53 . success)))

(defun my/org-agenda ()
  (interactive)
  (org-agenda nil "n"))

(use-package! org-super-agenda
  :after org-agenda
  :config
  (setq org-super-agenda-groups
        '((:discard (:todo "HOLD" :todo "IDEA"))
          (:name "WIP"
           :todo "[-]")
          (:name "High Priority"
           :priority "1")
          (:name "Med Priority"
           :priority "2")
          (:name "Low Priority"
           :priority "3")
          (:name "Lower Priority"
           :priority "4")
          (:name "Lowest Priority"
           :priority "5")
          (:name "Today"
           ;; :time-grid t
           :scheduled today
           :deadline today)
          (:auto-todo t)))
  (org-super-agenda-mode))

(use-package! org-cliplink)

(after! tramp
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path)
  (setq tramp-use-scp-direct-remote-copying t)
  (customize-set-variable 'tramp-default-method "scp"))

(setq password-cache-expiry nil)

(use-package! teleport
  :init  (teleport-tramp-add-method)
  :bind (:map teleport-list-nodes-mode-map
              ("v" . vterm)
              ("t" . term)
              ("d" . dired)))

(with-eval-after-load 'vterm
  (add-to-list 'vterm-tramp-shells `(,teleport-tramp-method "/bin/bash")))

(with-eval-after-load 'dired-rsync
  (defun teleport--is-file-on-teleport (filename)
    (when (tramp-tramp-file-p filename)
      (with-parsed-tramp-file-name filename v
        (string= v-method teleport-tramp-method))))

  (defun teleport-rsync-advice (orig-func sfiles dest)
    (if (or (teleport--is-file-on-teleport (car sfiles)) (teleport--is-file-on-teleport dest))
        (let ((dired-rsync-options (format "%s %s" dired-rsync-options "-e \"tsh ssh\"")))
          (funcall orig-func sfiles dest))
      (funcall orig-func sfiles dest)))
  (advice-add 'dired-rsync--remote-to-from-local-cmd :around #'teleport-rsync-advice))

(use-package! org-ai
  :hook
  (org-mode . org-ai-mode)
  :init
  (org-ai-global-mode)                  ; installs global keybindings on C-c M-a
  :config
  (setq org-ai-service 'openai)
  (setq org-ai-default-max-tokens nil)
  (setq org-ai-default-chat-model "gpt-4o"))

;; hack around password-store init. neither "after" or "requires" worked...
(defun my/set-org-ai-token ()
  (setq org-ai-openai-api-token (encode-coding-string (format "%s" (password-store-get "openai/apikey")) 'utf-8)))

(run-with-idle-timer 1 nil #'my/set-org-ai-token)

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

(use-package! undo-tree
  :init
  (setq undo-tree-auto-save-history nil
        undo-tree-visualizer-timestamps t
        undo-tree-visualizer-diff t)
  :config
  ;; stolen from layers/+spacemacs/spacemacs-editing/package.el
  (progn
    ;; restore diff window after quit.  TODO fix upstream
    (defun my/undo-tree-restore-default ()
      (setq undo-tree-visualizer-diff t))
    (advice-add 'undo-tree-visualizer-quit :after #'my/undo-tree-restore-default))
  (global-undo-tree-mode 1))

(use-package! topsy
  :defer t
  :init
  (add-hook! prog-mode
    (unless (memq major-mode '(+doom-dashboard-mode org-mode dirvish-mode))
      (topsy-mode +1))))

;; Colemak
(customize-set-variable 'avy-keys '(?a ?r ?s ?t ?n ?e ?i ?o))

(setq rustic-lsp-client 'eglot)

(add-hook 'eglot-managed-mode-hook
          (lambda ()
            (flymake-mode -1)
            (eglot-inlay-hints-mode -1)))

(map! :map vertico-map
      "C-SPC" #'+vertico/embark-preview)

(global-set-key (kbd "M-i") 'iedit-mode)

(use-package! consult
  :bind
  ;; swiper muscle-memory
  ("M-s l" . consult-line))

(add-to-dk-keymap
 '(("<SPC>" . deadgrep)
   ;; Project content search. ripgrep automatically understands .gitignore
   ("g" . consult-ripgrep)
   ;; Project file search.
   ("j" . consult-projectile)
   ("i" . consult-imenu)
   ("l" . consult-buffer)
   ("o" . consult-outline)))

(global-set-key [remap yank-pop] 'consult-yank-pop)

(use-package! ctrlf
  :init
  (ctrlf-mode +1))

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

(setq dired-omit-extensions nil)

(after! dired
  (remove-hook 'dired-mode-hook 'dired-omit-mode)
  (setq dired-listing-switches "-aBhlv --group-directories-first"
        dired-dwim-target t
        dired-recursive-copies (quote always)
        dired-recursive-deletes (quote top)
        ;; Directly edit permisison bits!
        wdired-allow-to-change-permissions t))

(use-package! dired-rsync
  :bind (:map dired-mode-map
              ("C-c C-r" . dired-rsync)))

(use-package! dired-x)

;; Directly edit permission bits!
(setq wdired-allow-to-change-permissions t)

;; prevents horizontal splits when split-window-sensibly is used
(setq split-width-threshold nil)

(delete 'register-alist savehist-additional-variables)

(set-register ?h '(file . "~/Sync/home/config.org"))
(set-register ?r '(file . "~/Sync/resume/resume.tex"))

(unless (getenv "EMACS_NON_WORK_MODE")
  (load-file "/home/dan/Work/w/emacs/work-config.el")
  (require 'work-config))

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
 "C-?" 'undo-fu-only-redo
 "C-x C-z" nil)
  ;; remove binding for suspend-frame
;; (global-set-key [remap goto-line] 'goto-line-with-feedback)
;; (global-set-key [remap goto-line] 'goto-line-with-feedback)

(defun pause-greenclip-daemon ()
  (shell-command "ps axf | grep 'greenclip daemon' | grep -v grep | awk '{print $1}' | xargs kill -20"))

(defun resume-greenclip-daemon ()
  (shell-command "greenclip print ' ' && ps axf | grep 'greenclip daemon' | grep -v grep | awk '{print $1}' | xargs kill -18"))

(defadvice password-store-copy (around pause-and-resume-greenclip activate)
  "Pause the greenclip daemon before saving the password to the kill ring, then resume the daemon after saving."
  (pause-greenclip-daemon)
  ad-do-it
  (run-with-idle-timer 10 1 #'resume-greenclip-daemon)
  )

(doom/open-scratch-buffer nil nil t)

(set-company-backend! 'text-mode nil)

(defun my/file-local-p (f)
  (not (file-remote-p f)))

(after! recentf
  (add-to-list 'recentf-keep 'my/file-local-p))
;; (setq warning-minimum-level :emergency)

;; (when doom-debug-p
;;   (require 'benchmark-init)
;;   (add-hook 'doom-first-input-hook #'benchmark-init/deactivate))

(setq async-shell-command-buffer 'new-buffer)

                                        ; (add-to-list 'auto-mode-alist '("\\.eps\\'" . doc-view-minor-mode))

;; all backup and autosave files in the tmp dir
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; Coordinate between kill ring and system clipboard
(setq save-interprogram-paste-before-kill t)

;; (setq eshell-history-file-name (concat doom-private-dir "eshell-history"))

;; This is dangerous, but reduces the annoying step of confirming local variable settings each time
;; a file with a "Local Variables" clause (like many Org files) is opened.
(setq-default enable-local-variables :all)

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
  (setq vterm-max-scrollback 100000
        vterm-copy-exclude-prompt t))
(customize-set-variable 'vterm-buffer-name-string nil)

;; Emacs 28: Hide commands in M-x which do not work in the current mode.
;; Vertico commands are hidden in normal buffers.
(setq read-extended-command-predicate
      #'command-completion-default-include-p)

(defun crm-indicator (args)
  (cons (concat "[CRM] " (car args)) (cdr args)))


(advice-add #'completing-read-multiple :filter-args #'crm-indicator)

(defun my-compilation-mode-hook ()
  (visual-line-mode 1))

(add-hook 'compilation-mode-hook 'my-compilation-mode-hook)

(use-package! exec-path-from-shell
  :init
  (exec-path-from-shell-copy-env "SSH_AGENT_PID")
  (exec-path-from-shell-copy-env "SSH_AUTH_SOCK"))

(use-package! real-auto-save
  :hook
  (prog-mode . real-auto-save-mode)
  (org-mode . real-auto-save-mode))

(use-package! fancy-dabbrev
  :hook
  (prog-mode . fancy-dabbrev-mode)
  (org-mode . fancy-dabbrev-mode)
  :config
  ;; (setq fancy-dabbrev-preview-delay 0.1)
  (setq fancy-dabbrev-preview-context 'before-non-word)
  ;; Let dabbrev searches ignore case and expansions preserve case:
  (setq dabbrev-case-distinction nil)
  (setq dabbrev-case-fold-search t)
  (setq dabbrev-case-replace nil)
  (add-hook 'minibuffer-setup-hook (lambda () (fancy-dabbrev-mode 0)))
  (add-hook 'minibuffer-exit-hook (lambda () (fancy-dabbrev-mode 1))))
