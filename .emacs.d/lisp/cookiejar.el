;; useful goodies and ad-hoc Emacs Lisp practice

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
(defun curl ()
  "GET a given URL. quotes THE URL, so you can use query strings without fear."
  (interactive)
  (let* ((url (read-string "url: "))
         (buffer-name (concat "*curl*<" url ">")))
    (async-shell-command (concat "curl -vvv '" url "'") buffer-name)
    (switch-to-buffer-other-window buffer-name)))

(defun curl-post ()
  "POST a BODY given URL. quotes THE URL, so you can use query strings without fear."
  (interactive)
  (let* ((url (read-string "url: "))
         (body (read-string "body: "))
         (buffer-name (concat "*curl*<" url ">")))
    (async-shell-command (concat "curl -XPOST -vvv '" url "'"
                                 " --data '" body "'") buffer-name)
    (switch-to-buffer-other-window buffer-name)))

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

(defun ping (arg)
  "Ping Google Primary DNS. A reliable connectivity test.

C-u controls number of pings."
  (interactive "P")
  (let ((count (if (null arg) "2" (number-to-string (car arg))))
        (name "*ping*"))
    (message (concat "Starting a sequence of " count " pings"))
    ;(start-process "ping" "*ping*" "ping" (concat "-c " count) "8.8.8.8")
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
