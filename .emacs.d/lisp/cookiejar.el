;; useful goodies and ad-hoc Emacs Lisp practice

(require 'cl-lib)
(require 'w3m)
(require 'noxml-fold)

(defun setq-with-prompt (var prompt)
  "Set quoted variable name var, defaulted to it's current value, with given prompt"
  (let ((val (read-string prompt (symbol-value var))))
    (set var val)))

(eval-after-load 'w3m
  (lambda ()
    (defun w3m-set-width (&optional columns justify)
      "Uses #'set-window-margins to confine the column width of an w3m buffer"
      (interactive)
      (unless (and (not (null columns))
                   (numberp columns)
                   (integerp columns)
                   (> (window-width) 0))
        (setq columns 66))
      (when (eq major-mode 'w3m-mode)
        (let (frame-width diff l-margin r-margin)
          (setq frame-width (frame-width))
          (setq diff (max 0 (- frame-width columns)))
          (cond ((eq justify 'right)
                 (message "right justify")
                 (setq l-margin diff)
                 (setq r-margin 0))
                ('t
                 (setq l-margin 0)
                 (setq r-margin diff)))
          (set-window-margins (selected-window) l-margin r-margin)
          (w3m-redisplay-this-page))))))

;; insert a string at point and indent it to the default indentation,
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
(defvar curl-user-agent "Mozilla/5.0 (Macintosh; Intel Mac OS X 10.12; rv:54.0) Gecko/20100101 Firefox/54.0"
  "The User-Agent used with cURL")
(defvar curl-accept-encoding "gzip, deflate"
  "The encodings you'll accept")

(defun curl-buffer-name (url)
  "Used to set the buffer name in do-curl if a buffer name is not provided"
  (concat "*curl*<" url ">"))

(defun de-cr-current-buffer ()
  "Remove carriage returns from the current buffer"
  (save-excursion
    (set-window-point nil 0)
    (while (re-search-forward "" nil t)
      (replace-match "" nil nil))))

(defun do-curl (url opts &optional buffer switch callback)
  (let ((buffer-name (if (null buffer) (curl-buffer-name url) buffer)))
    (setq curl-url url)
    (async-shell-command (concat "curl " opts " '" url "'") buffer-name)
    (if callback
	(with-current-buffer buffer-name
	  (apply callback '())))
    (if switch (switch-to-buffer-other-window buffer-name))))

(defun curl-cookie-header-for (cookie-string)
  "takes a cookie string (which may be nil) and returns a Cookie header"
  (cond ((null cookie-string) "")
	((string-empty-p cookie-string) "")
	('t (concat "Cookie: " cookie-string))))

(defun curl-post-buffer-file (url fname cookie &optional referer user-agent encoding)
  (interactive
   (let* ((url (read-string "url: " curl-url))
	  (fname (read-string "filename: " (buffer-file-name)))
	  (cookie (read-string "Cookie: " curl-cookie-header))
	  (referer (read-string "Referer: " url))
	  (user-agent (read-string "User-Agent: " curl-user-agent))
	  (encoding (read-string "Accept-Encoding: " curl-accept-encoding)))
     (list url fname cookie referer user-agent encoding)))
  (setq curl-url url)
  (setq curl-cookie-header cookie)
  (do-curl url (concat "-vvv --data @" fname
		       " --referer " referer
		       " -H 'Accept-Encoding: " encoding "'"
		       " -H '" (curl-cookie-header-for cookie) "'"
		       " -H 'User-Agent: " user-agent "'"
		       )))

(defun curl-process (url &rest args)
  "Similar to #'curl, but uses #'call-process rather than start a terminal"
  (let ((bufname (curl-buffer-name url))
        (errfile ".tmp-curl-error"))
    (if (file-exists-p errfile) (delete-file errfile))
    (if (buffer-live-p (get-buffer bufname)) (kill-buffer bufname))
    (apply #'call-process "curl" nil (list bufname errfile) 't url "-vvv" "--silent" args)
    (find-file errfile)
    (de-cr-current-buffer)
    (save-buffer)
    (let ((str (buffer-string)))
      (set-buffer bufname)
      (cond ((string-match-p "< [Cc]ontent-[Tt]ype: application\/json" str)
             (javascript-mode)
             (json-pretty-print-buffer))
            ((or (string-match-p "< [Cc]ontent-[Tt]ype: application\/xml" str)
                 (string-match-p "< [Cc]ontent-[Tt]ype: text\/xml" str)
                 (string-match-p "< [Cc]ontent-[Tt]ype: text\/html" str))
             (xml-mode)
             (noxml-fold-mode)
             (sgml-pretty-print))
            ('t nil))
      (cond ((and (string-match-p "< [Cc]ontent-[Ll]ength:" str)
                  (not (string-match-p "< [Cc]ontent-[Ll]ength: [^0]" str)))
             (display-buffer bufname)
             (find-file errfile)
             (message "Response has no content"))
            ((string-match-p "< HTTP/[0-9][0-9.]* 204" str)
             (find-file errfile)
             (message "204 No Content"))
            ('t
             (display-buffer bufname)
             (kill-buffer errfile)
             (message "got response body"))))))

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

(defun select-buffer-window-or-switch (name)
  "If a buffer with name NAME is visible, select the window displaying that buffer. Othewise, switch the current window to that buffer."
  (let ((win (buffer-window-if-visible name)))
    (cond ((null win) (switch-to-buffer name))
          ('t (select-window win "no record")))))

(defun buffer-is-visible-p (name)
  (if (not (null (buffer-window-if-visible name))) 't))

(defun ping (arg)
  "Ping Google Primary DNS. A connectivity test.

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
      (progn
        (window-resize win (- 5 (window-height win)))))))

;; set ping as a command
(global-set-key (kbd "C-x p") #'ping)

(defun shell-command-maybe-region (command)
  "Interactive command to run (if active) the highlighted text as a shell command. Inspired by Plan 9 from Bell Labs' click-to-execute default shell behavior."
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

(defun docker-pull-container (&optional container)
  (interactive)
  (shell-command (format "`aws ecr get-login 2>&1 | sed -n 's/.e none//p'` && docker pull %s" container)))

(defun run-docker-container (&optional container environment region)
  (interactive
   (let ((container (read-string "Container: " docker-last-container))
         (environment (read-string "Environment: " "staging"))
         (region (read-string "Region: " "us-west-2")))
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

(defun chrome-open (url)
  (call-process "open" nil nil nil "-a" "Google Chrome" url))

(defun firefox-open (url)
  (call-process "open" nil nil nil "-a" "Firefox" url))

(eval-after-load 'dap-java
  (lambda ()
    (defun dap-java-run-test-project ()
      (interactive)
      (-let* ((to-run (lsp-java--get-root))
              (test-class-name (cl-first (s-split "#" (dap-java-test-class))))
              (class-path (->> (with-lsp-workspace (lsp-find-workspace 'jdtls)
                                 (lsp-send-execute-command "vscode.java.resolveClasspath"
                                                           (vector test-class-name nil)))
                               cl-second
                               (s-join dap-java--classpath-separator)))

              ;; (package-includes (->> (with-lsp-workspace
              ;;                            (lsp-find-workspace 'jdtls)
              ;;                          (lsp-send-execute-command "vscode.java.test.search.items"
              ;;                                                    (vector (lsp--json-serialize `(:uri ,(lsp--path-to-uri to-run) :level 1)))))
              ;;                        (mapcar (lambda (h) (gethash "displayName" h)))
              ;;                        (mapcar (lambda (p) (format "--select-package=%s" p)))
              ;;                        (s-join " ")))
              (prog-list (if dap-java-use-testng
                             nil ;; testNg not supported
                           (cl-list* dap-java-java-command "-jar" dap-java-test-runner
                                     "-cp" (format dap-java--var-format "JUNIT_CLASS_PATH")
                                     ;;"-d" to-run ;; run root directory (doesn't work not recursive)
                                     ;;"--scan-modules" ;; scan all modules?
                                     "--scan-classpath"
                                     "--include-engine=junit-jupiter"
                                     "--exclude-engine=junit-vintage"
                                     dap-java-test-additional-args
                                     )))
              (command (list :program-to-start (s-join " " prog-list)
                             :environment-variables `(("JUNIT_CLASS_PATH" . ,class-path))
                             :name to-run
                             :cwd (lsp-java--get-root))))
        (setq dap-java--latest-test (-> command (plist-put :skip-debug-session t)))
        (dap-start-debugging dap-java--latest-test)
        ))))

(defun xml-snip ()
  "Grab the region, which hopefully contains XML, copy it into a new buffer and format it for easier reading."
  (interactive)
  (when (region-active-p)
    (let ((substr (buffer-substring (region-beginning) (region-end))))
      (pop-to-buffer "*formatted-xml*")
      (xml-mode)
      (rename-uniquely)
      (insert substr)
      (sgml-pretty-print 0 (buffer-end))
      (setq-local truncate-lines 't))))

(provide 'cookiejar)
