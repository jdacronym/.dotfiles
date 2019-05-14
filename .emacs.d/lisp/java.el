(require 'cookiejar)
(require 'cl-lib)

;; convenience functions I wrote while working on Java code. Hopefully these
;; will be useful at work. They're not pretty and need a LOT of refactoring.

(defun java-project-root-for-file (&optional fname)
  (cond ((or (string-match "\/src\/test\/java" fname)
	     (string-match "\/src\/main\/java" fname))
	 (replace-regexp-in-string "src\/....\/java\/.*" "" fname))
	('t (message "no match"))))

(cl-assert (string-equal
	    (java-project-root-for-file "/Users/foo/src/bar/baz/src/test/java/com/hulu/baz/testng/BazTest.java")
	    "/Users/foo/src/bar/baz/"))

(defvar java-method-access "public")
(defvar java-method-modifier "static")
(defvar java-method-return-type "void")
(defvar java-method-name "foo")
(defvar java-current-buffer nil)
(defconst *java-method-create-menu-name* "*Java Method Creation Menu*")
(defconst *java-code-block-delimiters* "{\n\n}")

(defun java-draw-create-method-menu()
  ;; TODO pull these out and resuse them to insert classes/interfaces
  (let* ((my-format (lambda (s) ; options separated by pipes " | "
		      (if (consp s)
			  (format "\"%s\" (%s)" (car s) (cdr s))
			(format "\"%s\"" s))))
	 (option-list (lambda (l) ; option list in brackets "[]"
			(format "[%s]" (mapconcat my-format l " | "))))
	 ;; TODO make these vars or consts
	 (access-opts '("public" "protected" "private" ("". "package-private")))
	 (modifier-opts '("static" "abstract" "")))
    (let ((access (apply option-list access-opts nil))
	  (modifier (apply option-list modifier-opts nil)))
      ;; TODO use mapconcat here insted
      (insert (concat
	       "\t\tMethod Creation Menu"
	       "\n(a)ccess:\t" access
	       "\n(m)odifier:\t" modifier
	       "\nreturn (t)ype:"
	       "\nmethod (n)ame:"
	       "\n(q)uit:\t\tKill this buffer without adding a method"
	       "\n(c)reate:\tinsert method with this signature at point:\n\n"
	       (java-method-signature))))))

(defun java-create-method-menu ()
  (interactive)
  (let ((menu-name *java-method-create-menu-name*)
	(buf-name (buffer-name)))
    (setq java-current-buffer buf-name)
    (pop-to-buffer menu-name)
    (java-draw-create-method-menu)
    (java-set-create-method-hooks)
    (read-only-mode)))

(defun java-set-create-method-hooks ()
  (let ((st (point)))
    (local-set-key (kbd "c") #'java-insert-method)
    (local-set-key (kbd "q")
		   #'(lambda () (interactive)
		       (kill-buffer *java-method-create-menu-name*)))
    (local-set-key (kbd "a")
		   #'(lambda ()
		       (interactive)
		       (setq-with-prompt 'java-method-access "Access level: ")
		       (java-redraw-method-menu)))
    (local-set-key (kbd "m")
		   #'(lambda () (interactive)
		       (setq-with-prompt 'java-method-modifier "Modifier: ")
		       (java-redraw-method-menu)))
    (local-set-key (kbd "t")
		   #'(lambda ()
		       (interactive)
		       (setq-with-prompt 'java-method-return-type "Return type: ")
		       (java-redraw-method-menu)))
    (local-set-key (kbd "n")
		   #'(lambda () (interactive)
		       (setq-with-prompt 'java-method-name "Method name: ")
		       (java-redraw-method-menu)))))

(defun java-redraw-method-menu ()
  (let ((pt (point)))
    (read-only-mode -1)
    (delete-region 1 pt)
    (java-draw-create-method-menu)
    (read-only-mode)))

(defun java-method-signature ()
  (concat java-method-access " " java-method-modifier " " java-method-return-type " " java-method-name "(/* args */)"))

(defun java-insert-method ()
  (interactive)
  (switch-to-buffer-other-window java-current-buffer)
  (insert-and-indent
   (concat (java-method-signature)  *java-code-block-delimiters*))
  (kill-buffer *java-method-create-menu-name*))

(defvar java-last-dumped-class "Object")
(defun java-insert-class-method-dump (class-name)
  "Inserts a for loop that will enumerate the names of all declared methods in a class"
  (interactive
   (let ((class (read-string "Class: " java-last-dumped-class)))
     (list class)))
  (setq java-last-dumped-class class-name)
  (insert-and-indent (format
                      "for (java.lang.reflect.Method method: %s.class.getDeclaredMethods()) { %s } // TODO delete me later"
                      java-last-dumped-class
                      "System.err.format(\"Method: \%s\\n\", method.getName());")))

(defun java-insert-class-field-dump (class-name)
  "Inserts a for loop that will enumerate the names of all declared fields on a class"
  (interactive
   (let ((class (read-string "Class: " java-last-dumped-class)))
     (list class)))
  (setq java-last-dumped-class class-name)
  (insert-and-indent (format
                      "for (java.lang.reflect.Field field: %s.class.getDeclaredFields()) { %s } // TODO delete me later"
                      java-last-dumped-class
                      "System.err.format(\"Field: \%s\\n\", field.getName());")))

;; TODO abstract the above, make a class/interface inserter

(defun java-insert-junit-imports ()
  (interactive)
  (insert "import static org.junit.Assert.*;
import org.junit.Test;
"))

(defvar java-junit-version "4.12")
(defvar java-hamcrest-version "1.3")

(defun java-junit-jar ()
  (concat "junit-" java-junit-version ".jar"))

(defun java-hamcrest-jar ()
  (concat "hamcrest-core-" java-hamcrest-version ".jar"))

(defun java-junit-classpath ()
  (let* ((junit-jar (java-junit-jar))
	 (hamcrest-jar (java-hamcrest-jar))
	 (home (getenv "HOME"))
	 (directories (list "."
			    junit-jar
			    hamcrest-jar
			    (format "%s/.java/my_jars/%s" home junit-jar)
			    (format "%s/.java/my_jars/%s" home hamcrest-jar))))
    (mapconcat (lambda (s) s) directories ":")))

(cl-assert (string-equal (java-junit-classpath)
			 (let ((home (getenv "HOME"))
			       (jars "/.java/my_jars/"))
			   (concat ".:junit-4.12.jar:hamcrest-core-1.3.jar:"
				   home jars "junit-4.12.jar:"
				   home jars "hamcrest-core-1.3.jar"))))

(defun java-compile-buffer ()
  (interactive)
  (java-compile-file (buffer-name)))

(defun java-classpath-for-file (filename)
  (if (string-match-p "Test.java" filename)
      (concat "-cp " (java-junit-classpath))))

(defun java-compile-file (filename)
  (let* ((classpath (java-classpath-for-file filename))
	 (command (concat "javac -verbose " classpath " " filename)))
    (async-shell-command command (concat "*javac*<" filename ">"))))

(defun java-test-source-toggle ()
  (interactive)
  (let* ((bufname (buffer-name))
         (target-bufname (if (string-match "Test.java" bufname)
                             (java-test-to-source-name bufname)
                           (java-source-to-test-name bufname)))
         (complement (java-locate-complementary-file)))
    (if (not (null complement))
        (find-file complement))))

(defun java-locate-complementary-file (&optional bufname)
  (interactive)
  (let* ((bufname (if (null bufname) (buffer-name) bufname))
         (buf-file (expand-file-name (buffer-file-name (current-buffer))))
         (target-bufname (if (string-match "Test.java" bufname)
                             (java-test-to-source-name bufname)
                           (java-source-to-test-name bufname)))
         (components (split-string buf-file "/" 't))
         (files (cdr (java-compile-subdirectories components nil)))
         (file (locate-file target-bufname files)))
    (cond ((null file)
           (message "could not find %s" target-bufname)
           nil)
          ('t file))))

(defun java-compile-subdirectories (path-elements results)
  (message "path element '%s'" (car path-elements))
  (cond ((null path-elements) results)
        ((or (string-equal (car path-elements) "main")
             (string-equal (car path-elements) "test"))
         (ffap-all-subdirs (car results) 20))
        ('t
         (java-compile-subdirectories (cdr path-elements)
                                      (cons (concat (car results) "/" (car path-elements))
                                            results)))))

(defun java-open-test-file ()
  (interactive)
  (find-file (java-test-buffer (buffer-name))))

(defun java-open-source-file ()
  (interactive)
  (find-file (java-source-buffer (buffer-name))))

(defun java-test-buffer (bufname)
  (java-source-to-test-name bufname))

(defun java-source-buffer (bufname)
  (java-test-to-source-name bufname))

(defun java-run-junit ()
  (interactive)
  (java-compile-and-run-junit (buffer-name)))

(defun java-compile-and-run-junit (filename)
  (let* ((test-name (if (string-match "Test.java" filename)
			filename
		      (java-source-to-test-name filename)))
	 (test-class (replace-regexp-in-string "\.java" "" test-name)))
    (java-compile-file test-name)
    (sleep-for 0 1500)
    (async-shell-command (concat "java " (java-classpath-for-file test-name)
                                 " org.junit.runner.JUnitCore "
                                 test-class)
                         "*JUnit*")))

(defun java-normalize-buffer-name (filename)
  (replace-regexp-in-string "<.*>$"
                            ""
                            filename))

(cl-assert (string-equal (java-normalize-buffer-name "FooTest.java<somedirectory>")
                         "FooTest.java"))
(cl-assert (string-equal (java-normalize-buffer-name "FooTest.java<1>")
                         "FooTest.java"))
(cl-assert (string-equal (java-normalize-buffer-name "FooTest<something>.java")
                         "FooTest<something>.java"))

(defun java-source-to-test-name (filename)
  (java-class-to-test-name (java-source-to-class-name filename)))

(defun java-source-to-class-name (filename)
  (let ((sans-ext
         (replace-regexp-in-string ".java" "" (java-normalize-buffer-name filename))))
    (concat sans-ext ".class")))

(defun java-class-to-test-name (filename)
  (let ((sans-ext (replace-regexp-in-string ".class" "" (java-normalize-buffer-name filename))))
    (concat sans-ext "Test.java")))

(defun java-test-to-source-name (filename)
  (replace-regexp-in-string "Test.java" ".java" (java-normalize-buffer-name filename)))
    

(cl-assert (string-equal (java-source-to-class-name "Example.java")
                         "Example.class"))
(cl-assert (string-equal (java-source-to-class-name "Example.java<somedirectory>")
                         "Example.class"))
(cl-assert (string-equal (java-class-to-test-name "Example.class")
                         "ExampleTest.java"))
(cl-assert (string-equal (java-class-to-test-name "Example.class<somedirectory>")
                         "ExampleTest.java"))
(cl-assert (string-equal (java-test-to-source-name "ExampleTest.java")
                         "Example.java"))
(cl-assert (string-equal (java-test-to-source-name "ExampleTest.java<somedirectory>")
                         "Example.java"))

(defun java-get-package-buffer (&optional buffer-or-name)
  (interactive (list (buffer-name)))
  (set-buffer buffer-or-name)
  (save-excursion
    (goto-char (point-min))
    (let ((begin-package-name (search-forward-regexp "package[ \t\n]+"))
          (end-package-name (search-forward-regexp "[^;]+")))
      (buffer-substring begin-package-name end-package-name))))

(defun java-get-class-buffer (&optional buffer-or-name)
  (interactive (list (buffer-name)))
  (set-buffer buffer-or-name)
  (save-excursion
    (goto-char (point-min))
    (let ((begin-class-name (search-forward-regexp "class[ \t\n]+" nil nil 1))
          (end-class-name (search-forward-regexp "[^ \t\n]+")))
      (buffer-substring begin-class-name end-class-name))))

;; JDB - Java Debugger

(defvar java-jdb-process "*shell*"
  "the name of the buffer the jdb is currently running in")
(defvar java-jdb-buffer-end nil
  "the last know value of point in the jdb process' buffer")

(defun java-jdb-buffer ()
  (if (string-equal java-jdb-process "*shell*")
      (get-buffer "*shell*")
    (process-buffer (get-process java-jdb-process))))

(defun java-jdb-update-end ()
  (set-buffer (java-jdb-buffer))
  (setq java-jdb-buffer-end (point-max)))

(defun java-jdb-start ()
  (interactive)
  (if (eq major-mode 'java-mode)
      (progn
        (if (string-equal java-jdb-process "*shell*")
            (progn
              (message "starting jdb in shell")
              (send-string "*shell*" "jdb -attach 8000\n"))
          (cond ((null (process-live-p java-jdb-process))
                 (start-process java-jdb-process java-jdb-process "jdb" "-attach" "8000"))
                ((eq 'stop  (process-live-p java-jdb-process))
                 (kill-process java-jdb-process)
                 (start-process java-jdb-process java-jdb-process "jdb" "-attach" "8000"))
                ('t
                 (start-process java-jdb-process java-jdb-process "jdb" "-attach" "8000"))))
        (java-jdb-update-end))))

(defun java-jdb-pre-command-hook ()
  (if (s-starts-with-p "java-jdb" this-command)
      (java-jdb-update-end)))

(add-hook 'pre-command-hook #'java-jdb-pre-command-hook)

(defun java-jdb-set-breakpoint-here ()
  (interactive)
  (let ((line (line-number-at-pos))
        (package-name (java-get-package-buffer (buffer-name)))
        (class-name (java-get-class-buffer (buffer-name))))
    (send-string java-jdb-process (format "stop at %s.%s:%s\n" package-name class-name line))))

(defun java-jdb-clear-breakpoint-here ()
  (interactive)
  (let ((line (line-number-at-pos))
        (package-name (java-get-package-buffer (buffer-name)))
        (class-name (java-get-class-buffer (buffer-name))))
    (send-string java-jdb-process (format "clear %s.%s:%s\n" package-name class-name line))))

(defun java-jdb-show-breakpoints ()
  (interactive)
  (send-string java-jdb-process "clear\n"))

(defun java-jdb-show-locals()
  (interactive)
  (send-string java-jdb-process "locals\n"))

(defun java-jdb-continue ()
  (interactive)
  (send-string java-jdb-process "cont\n"))

(defun java-jdb-exit ()
  (interactive)
  (send-string java-jdb-process "exit\n"))

(defun java-jdb-recent ()
  (set-buffer (java-jdb-buffer))
  (if (not (eq (point-max) java-jdb-buffer-end))
      (buffer-substring java-jdb-buffer-end (point-max))
    ""))

(defun java-jdb-catch-up ()
  (interactive)
  ;;(select-window (buffer-window-if-visible (buffer-name (java-jdb-buffer))))
  (set-buffer (java-jdb-buffer))
  (java-jdb-update-end)
  ;; print the current stack
  (let ((begin java-jdb-buffer-end))
    (goto-char begin)
    (send-string java-jdb-process "where\n")
    (sleep-for 0 200)
    (message "active segment starts at %s, end of buffer is at %s"
             begin (point-max))
    (goto-char begin))
  (message "searching for first stacktrace layer")
  (let* ((regexp "[\[0-9\]+]")
         (point-after-match (search-forward-regexp regexp (point-max) 't)))
    (if (not (eobp))
        point-after-match
      (progn
        (message "sleeping")
        (sleep-for 1)
        (message "searching again")
        (search-forward-regexp regexp (point-max) 't))))
  (message "getting current layer of stacktrace!")
  (let ((line (buffer-substring (point) (point-at-eol))))
    (message "Line is: '%s'" line)
    (string-match "(\\(.*\.java:\[0-9,\]+\\))" line)
    (let ((match (match-string 1 line)))
      (message "match is '%s'" match)
      (when match
        (let* ((split (split-string match ":"))
               (buffer (car split))
               (line-number (string-to-number (replace-regexp-in-string "," "" (cadr split)))))
          (message "split is '%s'" split)
          (message "returning to end of buffer")
          (goto-char (point-max))
          (when (and split (get-buffer (car split)))
            (message "switching to buffer '%s'" buffer)
            (pop-to-buffer buffer)
            (message "moving point to line '%s'" line-number)
            (goto-line line-number)))))));; (java-jdb-catch-up)

;; Java tests (Gradle probably)

(defvar java-test-shell-buffer "*shell*"
  "the shell we watch for test output")

(defvar java-test-shell-begin nil
  "saved point from the test buffer before we save")

(defvar-local java-gradle-process nil
  "The process where this buffer (project) is running gradle.
Should only be running in a magit status buffer")

(defvar-local java-gradle-task "ArthurDent:debugTest"
  "The gradle task (and arguments) to run.

ex. `clean service:test -Denv=stage'

The task will be run with the -t and --info flags for verbose continuous build.")

(defun java-gradle-process-start (&optional gradle-command)
  (interactive
   (let ((command (read-string "gradle task: " java-gradle-task)))
     (list command)))
  (let ((buffer-name (format "*Gradle*<%s>" gradle-command)))
    (magit-status) ;; takes us to a buffer at the root of the project
    (setq java-test-shell-buffer buffer-name)
    (setq java-gradle-task gradle-command)
    (java-get-test-shell-current-point-max)
    (pop-to-buffer buffer-name)
    (hi-lock-face-buffer "SUCCESSFUL" 'hi-green-b)
    (hi-lock-face-buffer "FAILED" 'hi-red-b)
    (hi-lock-face-buffer "SKIPPED" 'hi-yellow)
    (setq java-gradle-process
          (start-process gradle-command buffer-name
                         "/bin/bash" "-c" "--" (format "./gradlew -t %s --info" gradle-command)))))

(defun java-gradle-process-kill ()
  (interactive)
  (unless (null java-gradle-process)
    (kill-process java-gradle-process)))

(defun java-gradle-buffer-goto-end ()
  (interactive)
  (when java-gradle-process
    (let ((buffer (process-buffer java-gradle-process)))
      (setq java-test-shell-buffer (buffer-name buffer))
      (select-window (buffer-window-if-visible (buffer-name buffer)))
      (goto-char (point-max))))
  (if (null java-gradle-process) (message "java-gradle-process is null")))
      
(defun java-gradle-buffer-previous-failure()
  (interactive)
  (when java-gradle-process
    (let* ((buffer (process-buffer java-gradle-process))
           (name (buffer-name buffer)))
      (setq java-test-shell-buffer (buffer-name buffer))
      (select-window (buffer-window-if-visible (buffer-name buffer)))
      (let ((occurrences (if (eq (point) (point-max)) ;; end of test buffer
                             3 ;; there's a "FAILED" for the whole suite and the failed task
                           1))  ;; just find the next "FAILED" in the output
            (orig-case-sensitivity case-fold-search))
        (setq-local case-fold-search nil)
        (if (null (search-backward-regexp "FAILED" java-test-shell-begin 't occurrences))
            (message "already at first error in output"))
        (setq-local case-fold-search orig-case-sensitivity))))
  (if (null java-gradle-process) (message "java-gradle-process is null")))

(defun java-get-test-shell-current-point-max ()
  (interactive)
  (if (eq major-mode 'java-mode)
      (let ((curr-buffer (current-buffer)))
        (set-buffer java-test-shell-buffer)
        (setq java-test-shell-begin (point-max))
        (set-buffer curr-buffer))))

(defun java-search-for-completed-tests ()
  (interactive)
  (set-buffer java-test-shell-buffer)
  (let ((str (buffer-substring java-test-shell-begin (point-max)))
        (pmax (point-max)))
    (message "Checking between %i and %i" java-test-shell-begin pmax)
    (cond
     ((eq java-test-shell-begin pmax) 'pending)
     ((string-match "BUILD SUCCESSFUL" str) 'passed)
     ((string-match "BUILD FAILED" str) 'failed)
     ('t nil))))

(defun java-check-shell ()
  "Alert the user in the modeline to the status of the tests"
  (interactive)
  (if (buffer-is-visible-p java-test-shell-buffer)
    (progn
      (message "canceling function timers")
      (cancel-function-timers #'java-idle-test-check)))
  (let ((status (java-search-for-completed-tests)))
    (cond ((eq status 'passed) (message "BUILD SUCCESSFUL (C-u C-x t to see test results)"))
          ((eq status 'failed) (message "BUILD FAILED (C-u C-x t to see test results)"))
          ((eq status 'pending) (message "Tests not yet started"))
          ('t (message "Tests not finished (C-x t: check again; C-u C-x t: view *shell*)")))
    status))

;; note - this used to be run in a recursive function with a TTL, but now runs
;; via an idle timer so it doesn't lock emacs when tests run. The idle timer is
;; only cleared when #'java-check-test-results is run with a non-nil value for
;; the SWITCH-BUFFER prefix argument
(defun java-watch-shell ()
  "Wait for the java shell to show that tests are completed"
  (interactive)
  (if (eq major-mode 'java-mode)
      (run-with-idle-timer 1.0 'repeat #'java-idle-test-check)
    ))

(defun java-idle-test-check (&optional bufname)
  "Timer function to check for tests results when Emacs is idle"
  (let* ((buffer-name (if (null bufname) java-test-shell-buffer bufname)))
    (java-check-shell)))

(defun java-check-test-results (&optional switch-buffer)
  "Check the java-test-shell-buffer to see if the tests have finished.

If SWITCH-BUFFER is non nil, for instance if called with a prefix
argument, cancels function timers running #'java-idle-test-check,
on the assumption that that you've seen the results and don't
need to be reminded."
  (interactive "P")
  (if switch-buffer
      (progn
        (cancel-function-timers #'java-idle-test-check)
        (switch-to-buffer java-test-shell-buffer)))
  (java-check-shell))

;;(setq debug-on-error 't)
;;(setq debug-on-error nil)

(defun default-highlights ()
  ;; added to hooks below
  (interactive) ;; for debugging purposes
  (hi-lock-face-buffer "\\(Note\\|NOTE\\|note\\)\\b" 'hi-yellow)
  ;;(hi-lock-face-buffer "[Nn][Oo][Tt][Ee]" 'hi-yellow)
  (hi-lock-face-buffer "[Tt][Oo][Dd][Oo]" 'hi-yellow)
  (hi-lock-face-buffer "System[.]\\(out\\|err\\)[.]format" 'hi-yellow)
  (hi-lock-face-buffer "System[.]\\(out\\|err\\)[.]print" 'hi-yellow)
  (hi-lock-face-buffer "System[.]\\(out\\|err\\)[.]println" 'hi-yellow))

;; (setq before-save-hook '())
(add-hook 'before-save-hook #'java-get-test-shell-current-point-max)
(add-hook 'after-save-hook #'java-watch-shell)
(add-hook 'after-save-hook #'java-jdb-start)

;; TODO make these a single keychord that toggles between source/test

;; (setq java-mode-hook nil) ;; reset the java-mode-hook, if you're developing

(add-hook 'java-mode-hook #'default-highlights)
(add-hook 'java-mode-hook
          #'(lambda () (local-set-key (kbd "s-;") #'java-test-source-toggle)))
(add-hook 'java-mode-hook
          #'(lambda () (local-set-key (kbd "C-c m") #'java-create-method-menu)))
(add-hook 'java-mode-hook
          #'(lambda () (local-set-key (kbd "C-c RET") #'java-compile-buffer)))
(add-hook 'java-mode-hook
          #'(lambda () (local-set-key (kbd "C-c t") #'java-run-junit)))
(add-hook 'java-mode-hook
          #'(lambda () (local-set-key (kbd "C-x t") #'java-check-test-results)))
