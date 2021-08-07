;;; mathpix.el --- Mathpix API from Emacs

;;; Commentary:
;; This package provides an interface similar to the Mathpix snipping tool.
;;
;; mathpix-screenshot prompts for a screenshot, which is sent for processing
;; via Mathpix's API.
;;
;;  Heavily adapted from org-download.

(require 'json)

(defcustom mathpix-app-id nil
  "App ID for Mathpix.")

(defcustom mathpix-app-key nil
  "App key for Mathpix.")

;; From org-download
(defcustom mathpix-screenshot-method "gnome-screenshot -a -f %s"
  "The tool to capture screenshots."
  :type '(choice
          (const :tag "gnome-screenshot" "gnome-screenshot -a -f %s")
          (const :tag "scrot" "scrot -s %s")
          (const :tag "gm" "gm import %s")
          (const :tag "imagemagick/import" "import %s")
          (const :tag "imagemagick/import + xclip to save to clipboard"
                 "export filename=\"%s\"; import png:\"$filename\" ;xclip -selection clipboard -target image/png -filter < \"$filename\" &>/dev/null")
          (const :tag "xfce4-screenshooter" "xfce4-screenshooter -r -o cat > %s")
          ;; screenshot method in ms-windows, /capture=4 stands for interactive.
          (const :tag "IrfanView" "i_view64 /capture=4 /convert=\"%s\"")
          ;; screenshot script in osx, -i stands for interactive,
          ;; press space key to toggle between selection and
          ;; window/application mode.
          (const :tag "screencapture" "screencapture -i %s")
          ;; take an image that is already on the clipboard, for Linux
          (const :tag "xclip"
                 "xclip -selection clipboard -t image/png -o > %s")
          ;; take an image that is already on the clipboard, for Windows
          (const :tag "imagemagick/convert" "convert clipboard: %s")
          (function :tag "Custom function")))

(defcustom mathpix-screenshot-file
  (expand-file-name "mathpix.png" temporary-file-directory)
  "The file to capture mathpix screenshots"
  :type 'string)

;; screenshot programs have exit-code of 0 even when screenshotting is cancelled.
;; To save API calls, we use the existence of the file as a check if the user
;; wants to continue. Hence, we must delete the file after each function call.
(defun mathpix-screenshot ()
  "Capture screenshot and send result to Mathpix API."
  (interactive)
  (let ((default-directory "~"))
    (make-directory (file-name-directory mathpix-screenshot-file) t)
    (if (functionp mathpix-screenshot-method)
        (funcall mathpix-screenshot-file mathpix-screenshot-file)
      (shell-command-to-string
       (format mathpix-screenshot-method mathpix-screenshot-file)))
    (when (file-exists-p mathpix-screenshot-file)
      (mathpix-insert-result mathpix-screenshot-file)
      (delete-file mathpix-screenshot-file))))


(defun mathpix-get-b64-image (file)
  "Returns the base-64 image string from file."
  (with-temp-buffer
    (insert-file-contents file)
    (base64-encode-string (buffer-string) t)))


(defun mathpix-insert-result (file)
  "Sends the image to Mathpix API."
  (require 'request)
  (request
    "https://api.mathpix.com/v3/latex"
    :type "POST"
    :headers `(("app_id" . ,mathpix-app-id)
               ("app_key" . ,mathpix-app-key)
               ("Content-type" . "application/json"))
    :data (json-encode-alist
           `(("src" . ,(format "data:image/%s;base64,%s"
                               (file-name-extension file)
                               (mathpix-get-b64-image file)))
             ("formats" . ,(list "latex_styled"))
             ("format_options" .
              ,`(("latex_styled" .
                  ,`(("transforms" .
                      (cons "rm_spaces" '()))))))))
    :parser 'json-read
    :sync t
    :complete (cl-function
               (lambda (&key response &allow-other-keys)
                 (insert
                  (alist-get 'latex_styled (request-response-data response)))))))

(provide 'mathpix)

;;; mathpix.el ends here
