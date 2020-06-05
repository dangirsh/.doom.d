;;; ~/.doom.d/funcs.el -*- lexical-binding: t; -*-

(defun my/open-literate-private-config-file ()
  "Open the private config.org file."
  (interactive)
  (find-file (expand-file-name "config.org" doom-private-dir)))

(defun my/rot13-and-kill-region ()
  (interactive)
  (kill-new (rot13
             (buffer-substring (region-beginning) (region-end)))))

(defun my/org-export-subtree-as-markdown-and-copy ()
  (interactive)
  (save-window-excursion
    (let ((export-buffer (org-md-export-as-markdown nil t nil)))
      (with-current-buffer export-buffer
        (clipboard-kill-ring-save (point-min) (point-max)))
      (kill-buffer export-buffer))))

(defun goto-line-with-feedback ()
  "Show line numbers temporarily, while prompting for the line number input"
  (interactive)
  (unwind-protect
      (progn
        (linum-mode 1)
        (call-interactively 'goto-line))
    (linum-mode -1)))

(defun split-window-horizontally-and-switch ()
  (interactive)
  (split-window-horizontally)
  (other-window 1))

(defun split-window-vertically-and-switch ()
  (interactive)
  (split-window-vertically)
  (other-window 1))

(defun my-increment-number-decimal
    (&optional
     arg)
  "Increment the number forward from point by 'arg'."
  (interactive "p*")
  (save-excursion
    (save-match-data
      (let (inc-by field-width answer)
        (setq inc-by
              (if arg
                  arg
                1))
        (skip-chars-backward "0123456789")
        (when (re-search-forward "[0-9]+" nil t)
          (setq field-width (- (match-end 0)
                               (match-beginning 0)))
          (setq answer (+ (string-to-number (match-string 0) 10) inc-by))
          (when (< answer 0)
            (setq answer (+ (expt 10 field-width) answer)))
          (replace-match (format (concat "%0" (int-to-string field-width) "d") answer)))))))

(defun rev-other-window ()
  (interactive)
  (other-window -1))

(defun eshell-here ()
  "Opens up a new shell in the directory associated with the
     current buffer's file. The eshell is renamed to match that
     directory to make multiple eshell windows easier."
  (interactive)
  (let* ((parent (if (buffer-file-name)
                     (file-name-directory (buffer-file-name))
                   default-directory))
         (name   (car (last (split-string parent "/" t)))))
    (eshell "new")
    (rename-buffer (concat "*eshell: " name "*"))
    (insert (concat "ls"))
    (eshell-send-input)))

;; https://www.emacswiki.org/emacs/CopyingWholeLines
(defun my/duplicate-line-or-region (&optional n)
  "Duplicate current line, or region if active.
With argument N, make N copies.
With negative N, comment out original line and use the absolute value."
  (interactive "*p")
  (let ((use-region (use-region-p)))
    (save-excursion
      (let ((text (if use-region        ; Get region if active, otherwise line
                      (buffer-substring (region-beginning) (region-end))
                    (prog1 (thing-at-point 'line)
                      (end-of-line)
                      (if (< 0 (forward-line 1)) ; Go to beginning of next line, or make a new one
                          (newline))))))
        (dotimes (i (abs (or n 1)))     ; Insert N times, or once if not specified
          (insert text))))
    (if use-region nil                  ; Only if we're working with a line (not a region)
      (let ((pos (- (point) (line-beginning-position)))) ; Save column
        (if (> 0 n)                             ; Comment out original with negative arg
            (comment-region (line-beginning-position) (line-end-position)))
        (forward-line 1)
        (forward-char pos)))))

(defun my/org-ref-noter-link-from-arxiv (arxiv-number)
  "Retrieve a pdf for ARXIV-NUMBER and save it to the default PDF dir.
Then, add a bibtex entry for the new file in the default bib
file. Then, create a new org-ref note heading for it (see
org-ref-create-notes-hook in packages.el to see it also creates
a property for org-noter). Finally, insert a descriptive link to
the note heading at point, using the paper title as the link
text.
"
  (interactive "sarxiv number: ")
  (let ((bibtex-dialect 'BibTeX))
    (org-ref-save-all-bibtex-buffers)
    (save-window-excursion
      (arxiv-get-pdf-add-bibtex-entry arxiv-number
                                      (car org-ref-default-bibliography)
                                      org-ref-pdf-directory)
      (org-ref-save-all-bibtex-buffers))
    (let* ((parsed-entry (save-excursion
                           (with-temp-buffer
                             (insert-file-contents (car org-ref-default-bibliography))
                             (bibtex-set-dialect (parsebib-find-bibtex-dialect) t)
                             (search-forward (format "{%s}" arxiv-number))
                             (bibtex-narrow-to-entry)
                             (bibtex-beginning-of-entry)
                             (bibtex-parse-entry)))))
      (org-insert-heading)
      (let* ((raw-ref-title (cdr (assoc "title" parsed-entry)))
             (ref-title (s-replace-regexp (rx (sequence "\n" (+ space))) " "
                                          (car (cdr (s-match (rx "{" (group (+ anything)) "}") raw-ref-title)))))
             (ref-key (cdr (assoc "=key=" parsed-entry))))
        (insert ref-title)
        (insert "\n\n")
        (insert (format "cite:%s" ref-key))))))

(defun my/set-redshift (level)
  (interactive "nRedshift level: ")
  (shell-command (format "redshift -O %s" level)))

(defun my/night-mode ()
  (interactive)
  (load-theme 'doom-one t)
  (doom/reload-theme)
  (my/set-brightness 10)
  (my/set-redshift 1500))

(defun my/day-mode ()
  (interactive)
  (load-theme 'doom-solarized-light t)
  (doom/reload-theme)
  (my/set-brightness 1000)
  (my/set-redshift 6000))


(defun narrow-or-widen-dwim (p)
  "If the buffer is narrowed, it widens. Otherwise, it narrows intelligently.
Intelligently means: region, subtree, or defun, whichever applies
first.

With prefix P, don't widen, just narrow even if buffer is already
narrowed."
  (interactive "P")
  (declare (interactive-only))
  (cond ((and (buffer-narrowed-p) (not p)) (widen))
        ((region-active-p)
         (narrow-to-region (region-beginning) (region-end)))
        ((derived-mode-p 'org-mode) (org-narrow-to-subtree))
        (t (narrow-to-defun))))

;; https://stackoverflow.com/questions/28727190/org-babel-tangle-only-one-code-block
(defun my/org-babel-tangle-block()
  (interactive)
  (let ((current-prefix-arg '(4)))
    (call-interactively 'org-babel-tangle)))

(defun my/open-org-files-list ()
  (delq nil
        (mapcar (lambda (buffer)
                  (buffer-file-name buffer))
                (org-buffer-list 'files t))))

(defun my/org-latex-toggle-recent ()
  (when (looking-back (rx "$ "))
    (save-excursion
      (backward-char 1)
      (org-toggle-latex-fragment))))

(add-hook 'org-mode-hook
          (lambda ()
            (org-cdlatex-mode)
            (add-hook 'post-self-insert-hook #'my/org-latex-toggle-recent 'append 'local)))

(defun my/save-shebanged-file-as-executable ()
  (and (save-excursion
         (save-restriction
           (widen)
           (goto-char (point-min))
           (save-match-data
             (looking-at "^#!"))))
       (not (file-executable-p buffer-file-name))
       (shell-command (concat "chmod +x " buffer-file-name))
       (message
        (concat "Saved as script: " buffer-file-name))))

(add-hook 'after-save-hook #'my/save-shebanged-file-as-executable)

;; https://llazarek.com/2018/10/images-in-org-mode.html
(defun my/org-link-file-path-at-point ()
  "Get the path of the file referred to by the link at point."
  (let* ((org-element (org-element-context))
         (is-subscript-p (equal (org-element-type org-element) 'subscript))
         (is-link-p (equal (org-element-type org-element) 'link))
         (is-file-p (equal (org-element-property :type org-element) "file")))
    (when is-subscript-p
      (user-error "Org thinks you're in a subscript. Move the point and try again."))
    (unless (and is-link-p is-file-p)
      (user-error "Not on file link"))
    (expand-file-name (org-element-property :path org-element))))


(defun my/org-resize-image-at-point (&optional arg)
  "Resize the image linked at point."
  (interactive)
  (let ((img (my/org-link-file-path-at-point))
        (percent (read-number "Resize to what percentage of current size? ")))
    (start-process "mogrify" nil "/usr/bin/mogrify"
                   "-resize"
                   (format "%s%%" percent)
                   img)))


(defun my/run-in-fresh-compilation (cmd &optional dir)

  (defun local-compile-buffer-namer (ignored)
    (generate-new-buffer-name cmd))

  (let* ((compilation-buffer-name-function #'local-compile-buffer-namer)
         (compilation-ask-about-save nil)
         (full-cmd (if dir (concat "cd " dir " && " cmd) cmd)))
    (compile full-cmd)))

(defun my/publish-dangirsh.org ()
  (interactive)
  (let ((neurosys-org-file "/home/dan/repos/dangirsh.org/site/projects/neurosys.org")
        (doom-org-file "/home/dan/repos/dangirsh.org/site/projects/doom-config.org"))
    ;; Hack: copy in the files - had issues hardlinking it.
    (copy-file (concat neurosys/base-dir "README.org") neurosys-org-file t)
    (copy-file (concat doom-private-dir "config.org") doom-org-file t)
    (my/run-in-fresh-compilation "./publi.sh" "/home/dan/repos/dangirsh.org/")))

(defun my/edit-resume ()
  (interactive)
  (find-file "~/Sync/resume/resume.tex"))

(defun my/org-split-block ()
    "Sensibly split the current Org block at point."
    (interactive)
    (if (my/org-in-any-block-p)
        (save-match-data
          (save-restriction
            (widen)
            (let ((case-fold-search t)
                  (at-bol (bolp))
                  block-start
                  block-end)
              (save-excursion
                (re-search-backward "^\\(?1:[[:blank:]]*#\\+begin_.+?\\)\\(?: .*\\)*$" nil nil 1)
                (setq block-start (match-string-no-properties 0))
                (setq block-end (replace-regexp-in-string
                                 "begin_" "end_" ;Replaces "begin_" with "end_", "BEGIN_" with "END_"
                                 (match-string-no-properties 1))))
              ;; Go to the end of current line, if not at the BOL
              (unless at-bol
                (end-of-line 1))
              (insert (concat (if at-bol "" "\n")
                              block-end
                              "\n\n"
                              block-start
                              (if at-bol "\n" "")))
              ;; Go to the line before the inserted "#+begin_ .." line
              (beginning-of-line (if at-bol -1 0)))))
      (message "Point is not in an Org block")))

  (defun my/org-in-any-block-p ()
    "Return non-nil if the point is in any Org block.
The Org block can be *any*: src, example, verse, etc., even any
Org Special block.
This function is heavily adapted from `org-between-regexps-p'."
    (save-match-data
      (let ((pos (point))
            (case-fold-search t)
            (block-begin-re "^[[:blank:]]*#\\+begin_\\(?1:.+?\\)\\(?: .*\\)*$")
            (limit-up (save-excursion (outline-previous-heading)))
            (limit-down (save-excursion (outline-next-heading)))
            beg end)
        (save-excursion
          ;; Point is on a block when on BLOCK-BEGIN-RE or if
          ;; BLOCK-BEGIN-RE can be found before it...
          (and (or (org-in-regexp block-begin-re)
                   (re-search-backward block-begin-re limit-up :noerror))
               (setq beg (match-beginning 0))
               ;; ... and BLOCK-END-RE after it...
               (let ((block-end-re (concat "^[[:blank:]]*#\\+end_"
                                           (match-string-no-properties 1)
                                           "\\( .*\\)*$")))
                 (goto-char (match-end 0))
                 (re-search-forward block-end-re limit-down :noerror))
               (> (setq end (match-end 0)) pos)
               ;; ... without another BLOCK-BEGIN-RE in-between.
               (goto-char (match-beginning 0))
               (not (re-search-backward block-begin-re (1+ beg) :noerror))
               ;; Return value.
               (cons beg end))))))

  (defun my/org-meta-return (&optional arg)
    "Insert a new heading or wrap a region in a table.
Calls `org-insert-heading', `org-insert-item',
`org-table-wrap-region', or `my/org-split-block' depending on
context.  When called with an argument, unconditionally call
`org-insert-heading'."
    (interactive "P")
    (org-check-before-invisible-edit 'insert)
    (or (run-hook-with-args-until-success 'org-metareturn-hook)
        (call-interactively (cond (arg #'org-insert-heading)
                                  ((org-at-table-p) #'org-table-wrap-region)
                                  ((org-in-item-p) #'org-insert-item)
                                  ((my/org-in-any-block-p) #'my/org-split-block)
                                  (t #'org-insert-heading)))))
