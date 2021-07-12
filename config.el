(use-package! consult
  :init
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
  (map! :localleader
  :map org-mode-map
        ;; override default binding for org-goto
        "." 'consult-outline)
  :config
  (setq consult-narrow-key "<")
  (setq consult-async-split-style 'space)
  (autoload 'projectile-project-root "projectile")
  (setq consult-project-root-function #'projectile-project-root)
  :bind
  (;; C-c bindings (mode-specific-map)
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
   ;; binding this way breaks pasting into vterm, must use [remap yank-pop]
   ;; ("M-y" . consult-yank-pop)     ;; orig. yank-pop

   ("<help> a" . consult-apropos) ;; orig. apropos-command
   ;; M-g bindings (goto-map)
   ("M-g e" . consult-compile-error)
   ("M-g f" . consult-flymake)           ;; Alternative: consult-flycheck
   ("M-g g" . consult-goto-line)         ;; orig. goto-line
   ("M-g M-g" . consult-goto-line)       ;; orig. goto-line
   ("M-g o" . consult-outline)           ;; Alternative: consult-org-heading
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
   ("M-s e" . consult-isearch)     ;; orig. isearch-edit-string
   ("M-s l" . consult-line))       ;; needed by consult-line to detect isearch
  )

(use-package! consult-flycheck
  :bind (:map flycheck-command-map
         ("!" . consult-flycheck)))

(use-package! consult-projectile)

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

(add-to-dk-keymap
 '(("<SPC>" . consult-ripgrep)
   ("g" . consult-git-grep)
   ("h" . consult-projectile)
   ("i" . consult-imenu)
   ("l" . consult-locate)
   ("j" . consult-buffer)))

(global-set-key [remap yank-pop] 'consult-yank-pop)
