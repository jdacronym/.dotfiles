;; useful goodies and ad-hoc Emacs Lisp practice

(provide 'cookiejar)

(defun setq-with-prompt(var prompt)
  "Set quoted variable name var, defaulted to it's current value, with given prompt"
  (let ((val (read-string prompt (symbol-value var))))
    (set var val)))

;; insert a string at point and, indent it to the default indentation,
;; then return point to where it started. Useful for inserting
;; multiline code expressions.
(defun insert-and-indent (string)
  (let ((start (point))
	(buffer-window (buffer-window-if-visible (buffer-name))))
    (insert string)
    (indent-region start (point))
    (set-window-point buffer-window start)))

;; auto-sharpquote. Found on endlessparentheses.com
(defun sharpify ()
  "insert #' unless in a string or comment."
  (interactive)
  (call-interactively #'self-insert-command)
  (let ((ppss (syntax-ppss)))
    (unless (or (elt ppss 3)
                (elt ppss 4)
                (eq (char-after) ?'))
      (insert "'"))))

(define-key emacs-lisp-mode-map "#" #'sharpify)

;; manipulating windows
(defun close-buffer-window (buffer-name)
  "Find a window showing a buffer and delete it."
  (let ((win (get-buffer-window buffer-name)))
      (cond ((null win) nil)
            ('t (delete-window win)))))

;; network stuff
(defvar curl-url "")
(defvar curl-post-body "")

(defun curl-reset-defaults ()
  (interactive)
  (setq curl-url "")
  (setq curl-post-body ""))

(defun curl-post-opts (body &optional content-type)
  (concat "-vvv -XPOST "
	  (if (null content-type)
	      (concat "--data-urlencode '" body "'")
	      (concat "--data '" body "' " "-H 'content-type: " content-type "'"))))

(defun do-curl (url opts &optional switch buffer)
  (let ((buffer-name (if (null buffer) (concat "*curl*<" url ">") buffer)))
    (setq curl-url url)
    (async-shell-command (concat "curl " opts " '" url "'") buffer-name)
    (if switch (switch-to-buffer-other-window buffer-name))))

(defun curl (url opts)
  "GET a given URL. quotes THE URL, so you can use query strings without fear."
  (interactive
   (let ((url (read-string "url: " curl-url))
	 (opts (read-string "opts: " "-vvv")))
     (list url opts)))
  (setq curl-url url)
  (do-curl url opts))

(defun curl-post (url body)
  "POST a BODY given URL. quotes THE URL, so you can use query strings without fear."
  (interactive
   (let ((url (read-string "url: " curl-url))
	 (body (read-string "body: " curl-post-body)))
     (list url body)))
  (setq curl-post-body body)
  (do-curl url (curl-post-opts body)))

(defun curl-post-json (url body)
  "POST a BODY given URL. quotes THE URL, so you can use query strings without fear."
  (interactive
   (let ((url (read-string "url: " curl-url))
	 (body (read-string "body: " curl-post-body)))
     (list url body)))
  (setq curl-post-body body)
  (do-curl url (curl-post-opts body "application/json")))

;;(do-curl slack-external-message-url (curl-post-opts "{\"text\":\"<http://i138.photobucket.com/albums/q255/gavdiggity/Scanners.gif|Mind. Blown.>\", \"icon_emoji\": \":boom:\", \"username\":\"WAT\"}" "application/json"))

(defun window-name (win)
  "buffer-name for windows"
  (buffer-name (window-buffer win)))

(defun buffer-window-if-visible (name)
  "Returns first window containing buffer NAME or nil if no live window contains it."
  (cl-reduce #'(lambda (acc win)
                 (or acc (if (equal (window-name win) name) win nil)))
             (cons nil (window-list))))

(defun select-buffer-window-or-switch (name)
  (let ((win (buffer-window-if-visible name)))
    (cond ((null win) (switch-to-buffer name))
          ('t (select-window win "no record")))))

(defun buffer-is-visible-p (name)
  (if (not (null (buffer-window-if-visible name))) 't))

(defun showable-shell-window (name prefix command)
  (let ((cmd (concat prefix command)))
    (lambda (&optional show)
      (interactive "P")
      (if (null show)
	  (progn
	    (async-shell-command cmd name)
	    ;;(close-buffer-window name)
	    )
	(select-buffer-window-or-switch name)))))

(defun ping (arg)
  "Ping Google Primary DNS. A reliable connectivity test.

C-u controls number of pings."
  (interactive "P")
  (let ((count (if (null arg) "2" (number-to-string (car arg))))
        (name "*ping*"))
    (message (concat "Starting a sequence of " count " pings"))
    (async-shell-command (concat "ping -c" count " 8.8.8.8") name)
    (let* ((win (get-buffer-window name))
           (height (window-height win)))
      (unless (buffer-is-visible-p name)
        (switch-to-buffer-other-window name))
      (window-resize win (- 5 (window-height win))))))

;; setting Emacs colors
(defun amber-mode ()
  "Set Emacs to a warm amber and black color scheme."
  (interactive)
  (let ((amber "#F0A000"))
    (color-on-color-mode amber "black" 't)))

(defun seafoam-mode ()
  "Set Emacs to a seafoam green color on black."
  (interactive)
  (let ((seafoam "#AAFFAA"))
    (color-on-color-mode seafoam "black" 't)))

(defun high-contrast-mode ()
  "A high-contrast mode for outdoor use"
  (interactive)
  (color-on-color-mode "black" "white"))

(defun wy-mode ()
  "Set foreground colors to Weyland-Yutani cyan."
  (interactive)
  (let ((wy "#F0FFF0"))
    (color-on-color-mode wy "black" 't)))

(defun color-on-color-mode (color1 color2 &optional dark)
  (set-foreground-color color1)
  (set-background-color color2)
  (let ((inactive-fg (if dark "grey80" "black"))
        (inactive-bg (if dark color2 "grey80")))
    (message inactive-fg)
    (custom-set-faces
     ;; TODO improve this, maybe with macros, or something other than
     ;; custom-set-faces
     (list 'mode-line
           (list (list 't (list :foreground color2
                                :background color1
                                :box (list :line-width -1
                                           :color color1
                                           :style 'released-button)))))
     (list 'mode-line-inactive
           (list (list 't (list :inherit 'mode-line
                                :foreground inactive-fg
                                :background inactive-bg
                                :box (list :line-width -1 :color color1)
                                :weight 'light)))))))
