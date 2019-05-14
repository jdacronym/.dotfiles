;; useful goodies and ad-hoc Emacs Lisp practice

(require 'cl-lib)

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
(defvar curl-url "http://www.example.com/")
(defvar curl-post-body "")
(defvar curl-cookie-header "")

(defun curl-reset-defaults ()
  (interactive)
  (setq curl-url "http://www.example.com/")
  (setq curl-post-body ""))

(defun curl-post-opts (body &optional content-type)
  (concat "-vvv -XPOST "
	  (if (null content-type)
	      (concat "--data-urlencode '" body "'")
	    (concat "--data '" body "' " "-H 'content-type: " content-type "'"))))

(defun curl-buffer-name (url)
  "Used to set the buffer name in do-curl if a buffer name is not provided"
  (concat "*curl*<" url ">"))

(defun do-curl (url opts &optional buffer switch callback)
  (let ((buffer-name (if (null buffer) (curl-buffer-name url) buffer)))
    (setq curl-url url)
    (async-shell-command (concat "curl " opts " '" url "'") buffer-name)
    (if callback
	(with-current-buffer buffer-name
	  (apply callback '())))
    (if switch (switch-to-buffer-other-window buffer-name))))

(defun curl (url opts)
  "GET a given URL. quotes the URL, so you can use query strings without fear."
  (interactive
   (let ((url (read-string "url: " curl-url))
	 (opts (read-string "opts: " "-vvv")))
     (list url opts)))
  (setq curl-url url)
  (do-curl url opts))

(defun curl-post (url body)
  "POST a BODY given URL. quotes the URL, so you can use query strings without fear."
  (interactive
   (let ((url (read-string "url: " curl-url))
	 (body (read-string "body: " curl-post-body)))
     (list url body)))
  (setq curl-post-body body)
  (do-curl url (curl-post-opts body)))

(defun curl-post-buffer (url body)
  (interactive
   (let ((url (read-string "url: " curl-url))
	 (buffer (read-string "buffer: " (buffer-name))))
     (with-current-buffer buffer
       (list url (buffer-string)))))
  (setq curl-url url)
  (do-curl url (curl-post-opts body)))

(defun curl-cookie-header-for (cookie-string)
  "takes a cookie string (which may be nil) and returns a Cookie header"
  (cond ((null cookie-string) "")
	((string-empty-p cookie-string) "")
	('t (concat "Cookie: " cookie-string))))

(defun curl-post-buffer-file (url fname cookie)
  (interactive
   (let* ((url (read-string "url: " curl-url))
	  (fname (read-string "filename: " (buffer-file-name)))
	  (cookie (read-string "Cookie: " curl-cookie-header))
	  (referer (read-string "Referer: " url))
	  (user-agent (read-string "User-Agent: " curl-user-agent))
	  (encoding (read-string "Accept-Encoding: " curl-accept-encoding)))
     (list url fname cookie referrer user-agent encoding)))
  (setq curl-url url)
  (setq curl-cookie-header cookie)
  (do-curl url (concat "-vvv --data @" fname
		       " --referer " referer
		       " -H 'Accept-Encoding: " encoding "'"
		       " -H '" (curl-cookie-header-for cookie) "'"
		       " -H 'User-Agent: " user-agent "'"
		       )))

(defvar curl-user-agent "Mozilla/5.0 (Macintosh; Intel Mac OS X 10.12; rv:54.0) Gecko/20100101 Firefox/54.0"
  "The User-Agent used with cURL")
(defvar curl-accept-encoding "gzip, deflate"
  "The encodings you'll accept")

(defun curl-process (url &rest args)
  "Similar to #'curl, but uses #'call-process rather than start a terminal"
  (let ((bufname (curl-buffer-name url))
        (errfile ".tmp-curl-error"))
    (if (file-exists-p errfile) (delete-file errfile))
    (if (buffer-live-p (get-buffer bufname)) (kill-buffer bufname))
    (apply #'call-process "curl" nil (list bufname errfile) 't url "-vvv" args)
    (find-file errfile)
    (let ((str (buffer-string)))
      (message "stderr output was %s" str)
      (set-buffer bufname)
      (cond ((string-match-p "application\/json" str)
             (javascript-mode)
             (json-pretty-print (point-min) (point-max)))
            ((string-match-p "application\/xml" str)
             (xml-mode)
             (noxml-fold-mode)
             (prettify-xml-response))
            ('t nil)))
    (kill-buffer errfile)
    (display-buffer bufname)))

(defun prettify-xml-response ()
  ;; I go back and forth here in hope that it's more efficient
  ;; collapse empty lines
  (replace-regexp "\n[ ]*\n" "\n" nil (point-min) (point-max))
  ;; add newlines
  (replace-regexp ">" ">\n" nil (point-min) (point-max))
  (replace-regexp "</" "\n</" nil (point-min) (point-max))
  ;; collapse any remaining blank lines
  (replace-regexp "\n[ ]*\n" "\n" nil (point-min) (point-max))
  ;; indent
  (indent-region (point-min) (point-max)))

(defun curl-post-json (url body)
  "POST a BODY given URL. quotes THE URL, so you can use query strings without fear."
  (interactive
   (let ((url (read-string "url: " curl-url))
	 (body (read-string "body: " curl-post-body)))
     (list url body)))
  (setq curl-post-body body)
  (do-curl url (curl-post-opts body "application/json")))

;;(do-curl slack-external-message-url (curl-post-opts "{\"text\":\"<http://i138.photobucket.com/albj;ums/q255/gavdiggity/Scanners.gif|Mind. Blown.>\", \"icon_emoji\": \":boom:\", \"username\":\"WAT\"}" "application/json"))

(provide 'curl)
(defun window-name (win)
  "#'buffer-name, but for windows"
  (buffer-name (window-buffer win)))

(defun buffer-window-if-visible (name)
  "Returns first window containing buffer NAME or nil if no live window contains it."
  (cl-reduce #'(lambda (acc win) (or acc (if (equal (window-name win) name) win nil)))
             (cl-reduce #'(lambda (all-windows frame)
                            (cl-reduce #'(lambda (acc window)
                                           (if (not (null window))
                                               (cons window acc)
                                             acc))
                                       (window-list frame)
                                       :initial-value all-windows))
                        (frame-list)
                        :initial-value nil)
             :initial-value nil))
;; (buffer-window-if-visible "*shell*")
;; (buffer-is-visible-p "*shell*")

(defun select-buffer-window-or-switch (name)
  "If a buffer with name NAME is visible, select the window displaying that buffer. Othewise, switch the current window to that buffer."
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

(defun wait-for-process-end (buffer-or-name)
  (let ((buf (get-buffer buffer-or-name)))
    (with-current-buffer buf
      (sleep-for 500)
      (while mode-line-process
	(sleep-for 1000)))))

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
;; set ping as a command
(global-set-key (kbd "C-x p") #'ping)

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

(defun shell-command-maybe-region (command)
  (interactive
   (let ((command-string (read-string "$ " (if (region-active-p)
                                               (let ((begin (region-beginning))
                                                     (end (region-end)))
                                                 (buffer-substring begin end))
                                             ""))))
     (list command-string)))
  (shell-command command))

(defvar docker-login-command "`aws ecr get-login 2>
&1 | sed -n 's/.e none//p`"
  "a shell command that will log the current shell into docker")

(defun docker-login ()
    (interactive)
    (shell-command "`aws ecr get-login 2>&1 | sed -n 's/.e none//p'`"))

(defvar docker-last-container nil "the name of the last docker container that was run")

(defun docker-read-container-name ()
  (interactive)
  (let* ((region (let ((repo (read-string "Region: " "east")))
                   (cond ((string-equal repo "east") 'east)
                         ((string-equal repo "west") 'west)
                         ('t                         'east))))
         (repository (if (eq region 'east)
                         "584878871707.dkr.ecr.us-east-1.amazonaws.com/ads"
                       "584878871707.dkr.ecr.us-west-2.amazonaws.com/ads"))
         (service (read-string "Service: " "services-common"))
         (version (read-string "Version: " "0.0.0-SNAPSHOT")))
    (let ((container (format "%s:%s-%s" repository service version)))
      (setq docker-last-container container)
      container)))

;; (call-interactively #'docker-read-container-name)

(defun docker-get-container-name (&optional container)
  (interactive)
  (cond ((and (null container) (null docker-last-container)) (call-interactively #'docker-read-container-name))
        ('t (read-string "Container: " docker-last-container))))

;;(setq docker-last-container nil)
;;(call-interactively #'docker-get-container-name)

(defun docker-pull-container (&optional container)
  (interactive (list (docker-get-container-name)))
  (setq docker-last-container container)
  (shell-command (format "`aws ecr get-login 2>&1 | sed -n 's/.e none//p'` && docker pull %s" container)))


;; (shell-command "`source ~/.bashrc; aws ecr get-login 2>&1 | sed -n 's/.e none//p'`")
;; (shell-command "`aws configure`")
;;(call-interactively #'docker-pull-container)

(defun run-docker-container (&optional container environment region)
  (interactive
   (let ((container (read-string "Container: " docker-last-container))
         (environment (read-string "Environment: " "stg1"))
         (region (read-string "Region: " "us-east-1")))
     (list container environment region)))
  (setq docker-last-container container)
  (let ((procname (format "*docker<%s>*" container)))
    (start-process procname procname
                   "docker" "run" "-it" "--entrypoint=bash"
                   "-v" (format "%s/root" (getenv "HOME"))
                   container)
    (send-string
     procname
     (format "export ADS_ENVIRONMENT=%s && export ADS_AWS=true && export ADS_AWS_REGION=%s && export ADS_LOGGING_CONSOLE=1\n"
             environment
             region))
    (send-string procname "java -jar integration-service.jar &\n")
    (pop-to-buffer procname)))


(defvar current-local-shell-command "gradlew test")

(defun bind-local-shell-command (key-string shell-command)
  "Locally binds a keychord to a command sent to the *shell*
  process. Requires *shell* to be running."
  (interactive
   (let ((key-string (read-string "key chord: " "C-x t"))
         (shell-command (read-string "$ " current-local-shell-command)))
     (list key-string (format "%s\n" shell-command))))
  (message "Binding shell command '%s' to keychord '%s'" key-string shell-command)
  (setq current-local-shell-command shell-command)
  (let ((fn (lambda () (interactive) (send-string "*shell*" current-local-shell-command))))
    (local-set-key (kbd key-string) fn)))

(provide 'cookiejar)
