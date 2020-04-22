;;; desktop/exwm/config.el -*- lexical-binding: t; -*-
(use-package! exwm
  :init
  (setq
   mouse-autoselect-window t
   focus-follows-mouse t)
  :config
  (setq exwm-workspace-number 9))

(defun my/exwm-rename-buffer-to-title () (exwm-workspace-rename-buffer (format "%s - %s" exwm-class-name exwm-title)))
(setq exwm-workspace-show-all-buffers t
      exwm-layout-show-all-buffers t
      exwm-manage-force-tiling t)


(setq exwm-input-prefix-keys '(?\s- ))

(display-battery-mode 1)
(display-time-mode 1)


(setq exwm-manage-configurations
      '(((string= exwm-class-name "Google-chrome")
         workspace 0)
        ((string= exwm-class-name "Firefox")
         workspace 1)
        ((string= exwm-instance-name "terminator")
         workspace 8)
        ((string= exwm-instance-name "keybase")
         workspace 9)))


(defun my/launch (command)
  (interactive (list (read-shell-command "$ ")))
  (start-process-shell-command command nil command))

(defun my/launch-terminal ()
  (interactive)
  (my/launch "terminator"))

(defun my/launch-browser ()
  (interactive)
  (my/launch "firefox"))

(defun my/launch-emacs ()
  (interactive)
  (my/launch "emacs"))

(defun my/lock-screen ()
  (interactive)
  (my/launch "xtrlock -b"))

(defun my/volume-up ()
  (interactive)
  (my/launch "amixer sset Master unmute")
  (my/launch "amixer sset Master 5%+"))

(defun my/volume-down ()
  (interactive)
  (my/launch "amixer sset Master 5%-"))


;; TODO Prompt for location after taking the shot
(defun my/screenshot ()
  (interactive)
  (shell-command
   (concat "bash -c 'FILENAME=/home/dan/screenshots/$(date +'%Y-%m-%d-%H:%M:%S').png && maim -s $FILENAME'")))


(defun my/screen-to-clipboard ()
  (interactive)
  (shell-command
   (concat "bash -c 'FILENAME=/home/dan/screenshots/$(date +'%Y-%m-%d-%H:%M:%S').png && maim -s $FILENAME"
           " && xclip $FILENAME -selection clipboard "
           "-t image/png &> /dev/null && rm $FILENAME'"))
  (message "Added to clipboard."))

(setq exwm-workspace-minibuffer-position 'nil)

(setq ivy-posframe-parameters '((parent-frame nil)))


(exwm-input-set-key (kbd "s-:") #'eval-expression)

;; https://emacs.stackexchange.com/questions/33326/how-do-i-cut-and-paste-effectively-between-applications-while-using-exwm
(defun my/exwm-input-line-mode ()
  "Set exwm window to line-mode and show mode line"
  (call-interactively #'exwm-input-grab-keyboard))

(defun my/exwm-input-char-mode ()
  "Set exwm window to char-mode and hide mode line"
  (call-interactively #'exwm-input-release-keyboard))

(defun my/exwm-input-toggle-mode ()
  "Toggle between line- and char-mode"
  (interactive)
  (with-current-buffer (window-buffer)
    (when (eq major-mode 'exwm-mode)
      (if (equal (second (second mode-line-process)) "line")
          (my/exwm-input-char-mode)
        (my/exwm-input-line-mode)))))

(defun my/toggle-exwm-input-line-mode-passthrough ()
  (interactive)
  (if exwm-input-line-mode-passthrough
      (progn
        (setq exwm-input-line-mode-passthrough nil)
        (message "App receives all the keys now (with some simulation)"))
    (progn
      (setq exwm-input-line-mode-passthrough t)
      (message "emacs receives all the keys now")))
  (force-mode-line-update))

(exwm-input-set-key (kbd "s-;") 'my/toggle-exwm-input-line-mode-passthrough)



;; Switch to last workspace
(defvar my/exwm-workspace-previous-index 0 "The previous active workspace index.")

(defun my/exwm-workspace--current-to-previous-index (_x &optional _y)
  (setq my/exwm-workspace-previous-index exwm-workspace-current-index))

(advice-add 'exwm-workspace-switch :before #'my/exwm-workspace--current-to-previous-index)

(defun my/exwm-workspace-switch-to-previous ()
  (interactive)
  "Switch to the previous active workspace."
  (let ((index my/exwm-workspace-previous-index))
    (exwm-workspace-switch index)))

(defun my/switch-to-last-buffer ()
  "Switch to last open buffer in current window."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))

;; Re-use muscle memory from 6 years of an xmonad setup
(exwm-input-set-key (kbd "s-p") #'dmenu)
(exwm-input-set-key (kbd "s-P") #'counsel-linux-app)
(exwm-input-set-key (kbd "s-s") #'password-store-copy)
(exwm-input-set-key (kbd "s-<return>") #'my/launch-terminal)
(exwm-input-set-key (kbd "s-.") #'my/switch-to-last-buffer)
(exwm-input-set-key (kbd "s-,") #'my/exwm-workspace-switch-to-previous)
(exwm-input-set-key (kbd "s-i") #'my/launch-browser)
(exwm-input-set-key (kbd "s-b") 'switch-to-buffer)
(exwm-input-set-key (kbd "s-M-O") #'my/lock-screen)
(exwm-input-set-key (kbd "s-<up>") #'my/volume-up)
(exwm-input-set-key (kbd "s-<down>") #'my/volume-down)
(exwm-input-set-key (kbd "s-<print>") #'my/screen-to-clipboard)

(exwm-input-set-key (kbd "s-R") #'doom/reload)
(exwm-input-set-key (kbd "s-Q") #'kill-emacs)

(exwm-input-set-key (kbd "s-m") #'bury-buffer)
(exwm-input-set-key (kbd "s-M") #'unbury-buffer)

(exwm-input-set-key (kbd "s-j") #'other-window)
(exwm-input-set-key (kbd "s-k") #'rev-other-window)

(exwm-input-set-key (kbd "s-J") #'previous-buffer)
(exwm-input-set-key (kbd "s-K") #'next-buffer)

(exwm-input-set-key (kbd "s-h") 'shrink-window)
(exwm-input-set-key (kbd "s-l") 'enlarge-window)
(exwm-input-set-key (kbd "s-H") 'shrink-window-horizontally)
(exwm-input-set-key (kbd "s-L") 'enlarge-window-horizontally)

(exwm-input-set-key (kbd "s-/") 'winner-undo)
(exwm-input-set-key (kbd "s-?") 'winner-redo)

(exwm-input-set-key (kbd "s-'") 'exwm-edit--compose)

(exwm-input-set-key (kbd "s-w") 'delete-window)
(exwm-input-set-key (kbd "s-q") 'kill-this-buffer)

(exwm-input-set-key (kbd "s-c") 'my/goto-literate-private-config-file)
(exwm-input-set-key (kbd "s-C") 'cfw:open-org-calendar)

(exwm-input-set-key (kbd "s-x") 'counsel-M-x)

(exwm-input-set-key (kbd "s-t") 'vterm)

(mapcar (lambda (i)
          (exwm-input-set-key (kbd (format "s-%d" i))
                              `(lambda ()
                                 (interactive)
                                 (exwm-workspace-switch-create ,i))))
        (number-sequence 0 9))

;; Configure firefox to open every tab as a new window instead
;; http://p.hagelb.org/exwm-ff-tabs.html
(add-hook 'exwm-manage-finish-hook
          (lambda ()
            ;; these have their own Emacs simulation installed (e.g. Surfingkeys)
            (if (or (string= exwm-class-name "Firefox")
                    (string= exwm-class-name "Google-chrome")
                    (string= exwm-class-name "Atom"))
                (progn
                  (exwm-input-set-local-simulation-keys
                   `(([?\s-w] . [?\C-w])
                     ([?\M-w] . [?\C-c])
                     ([?\C-y] . [?\C-v])))
                  (exwm-layout-hide-mode-line))
              (exwm-layout-show-mode-line))))

;; (add-hook 'exwm-update-title-hook
;;           (defun my/exwm-title-hook ()
;;             (when (string-match "Firefox" exwm-class-name)
;;               (exwm-workspace-rename-buffer exwm-title))))

(add-hook 'exwm-update-title-hook 'my/exwm-rename-buffer-to-title)

(setq browse-url-firefox-arguments '("-new-window"))

(setq exwm-input-simulation-keys
      '(
        ;; movement
        ([?\C-b] . [left])
        ([?\M-b] . [C-left])
        ([?\C-f] . [right])
        ([?\M-f] . [C-right])
        ([?\C-p] . [up])
        ([?\C-n] . [down])
        ([?\C-e] . [end])
        ([?\M-v] . [prior])
        ([?\C-v] . [next])
        ([?\C-d] . [delete])
        ;; undo
        ([?\C-/] . [?\C-z])

        ;; Interferes with Slack
        ;; ([?\C-k] . [S-end delete])

        ;; cut/copy/paste.
        ([?\C-w] . [?\C-x])
        ([?\M-w] . [?\C-c])
        ([?\C-y] . [?\C-v])
        ;; search
        ([?\C-s] . [?\C-f])))

(define-ibuffer-column exwm-class (:name "Class")
  (if (bound-and-true-p exwm-class-name)
      exwm-class-name
    ""))
(define-ibuffer-column exwm-instance (:name "Instance")
  (if (bound-and-true-p exwm-instance-name)
      exwm-instance-name
    ""))
(define-ibuffer-column exwm-urgent (:name "U")
  (if (bound-and-true-p exwm--hints-urgency)
      "U"
    " "))

(defun my/exwm-ibuffer (&optional other-window)
  (interactive "P")
  (let ((name (buffer-name)))
    (ibuffer other-window
             "*exwm-ibuffer*"
             '((mode . exwm-mode))
             nil nil nil
             '((mark exwm-urgent
                     " "
                     (name 64 64 :left :elide)
                     " "
                     (exwm-class 20 -1 :left)
                     " "
                     (exwm-instance 10 -1 :left))))
    (ignore-errors (ibuffer-jump-to-buffer name))))

(exwm-input-set-key (kbd "s-o") #'my/exwm-ibuffer)

(use-package! exwm-edit
  :init
  ;; Otherwise it steals C-c ' from org
  (setq exwm-edit-bind-default-keys nil))

(defun my/exwm-start-in-char-mode ()
  (when (or (string-prefix-p "terminator" exwm-instance-name)
            (string-prefix-p "emacs" exwm-instance-name)
            (string-prefix-p "next" exwm-instance-name))
    (exwm-input-release-keyboard (exwm--buffer->id (window-buffer)))))
(add-hook 'exwm-manage-finish-hook 'my/exwm-start-in-char-mode)

(require 'exwm-randr)
;; FIXME
(setq exwm-randr-workspace-monitor-plist '(0 "eDP-1"
                                             1 "HDMI-1"
                                             1 "HDMI-1"
                                             2 "HDMI-1"
                                             3 "HDMI-1"
                                             4 "HDMI-1"
                                             5 "HDMI-1"
                                             6 "HDMI-1"
                                             7 "HDMI-1"
                                             8 "HDMI-1"
                                             9 "HDMI-1"))


(require 'exwm-randr)
(exwm-randr-enable)

(setq my/default-screenlayout-file "/home/dan/.screenlayout/default.sh")
(setq my/monitor-screenlayout-file "/home/dan/.screenlayout/main.sh")

(defun my/default-screen-layout ()
  (interactive)
  (call-process "bash" nil 0 nil "-c" my/default-screenlayout-file))

(defun my/monitor-screen-layout ()
  (interactive)
  (call-process "bash" nil 0 nil "-c" my/monitor-screenlayout-file))

(exwm-enable)

;; (use-package! exwm-mff
;;   :config
;;   (exwm-mff-mode 1))
