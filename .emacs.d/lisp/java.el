(require 'cookiejar)
(require 'cl-lib)
(require 'ffap)
(require 'message)
(require 'magit)
;;(require 'cl) ;; maybe?

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

(defun java-test-file-p (bufname)
  (or (string-match "Test.java" bufname)
      (string-match "Tests.java" bufname)))

(defun java-test-source-toggle ()
  (interactive)
  (let* ((bufname (buffer-name))
         (target-bufname (if (java-test-file-p bufname)
                             (java-test-to-source-name bufname)
                           (java-source-to-test-name bufname)))
         (complement (java-locate-complementary-file)))
    (if (not (null complement))
        (find-file complement))))

(defun java-locate-complementary-file (&optional bufname)
  (interactive)
  (let* ((bufname (if (null bufname) (buffer-name) bufname))
         (buf-file (expand-file-name (buffer-file-name (current-buffer))))
         (target-bufname (if (java-test-file-p bufname)
                             (java-test-to-source-name bufname)
                           (java-source-to-test-name bufname)))
         (alternate-bufname (if (java-test-file-p target-bufname)
                                (replace-regexp-in-string "Test.java"
                                                          "Tests.java"
                                                          target-bufname))) ;; normalize test file name
         (components (split-string buf-file "/" 't))
         (files (cdr (java-compile-subdirectories components nil)))
         (file (locate-file target-bufname files))
         (alternate-file (if alternate-bufname (locate-file alternate-bufname files))))
    (message "looking for %s or %s" target-bufname alternate-bufname)
    (cond ((and (null file) (null alternate-file))
           (message "could not find %s or %s" target-bufname alternate-bufname)
           nil)
          ((not (null file)) file)
          ('t alternate-file))))

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
  (message "guessing source file name")
  (replace-regexp-in-string "Tests?.java" ".java" (java-normalize-buffer-name filename)))

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
  (interactive
   (list (if (null buffer-or-name) (buffer-name) buffer-or-name)))
  (save-excursion
    (set-buffer buffer-or-name)
    (goto-char (point-min))
    (let ((begin-package-name (search-forward-regexp "package[ \t\n]+"))
          (end-package-name (search-forward-regexp "[^;]+")))
      (buffer-substring begin-package-name end-package-name))))

(defun java-get-class-buffer (&optional buffer-or-name)
  "find the java classname in the current buffer"
  (interactive
   (list (if (null buffer-or-name) (buffer-name) buffer-or-name)))
  (save-excursion
    (set-buffer buffer-or-name)
    (goto-char (point-min))
    (let ((begin-class-name (search-forward-regexp "public[ \t\n][ \t\nabstrct]*\\\(class\\\|interface\\\)[ \t\n]+" nil nil 1))
          (end-class-name (search-forward-regexp "[^ \t\n]+")))
      (buffer-substring begin-class-name end-class-name))))

;; JDB - Java Debugger

;; ...LOL just use GUD

;; Java tests (Gradle probably)

(defvar java-test-shell-buffer "*shell*"
  "the shell we watch for test output")

(defvar java-test-shell-begin nil
  "saved point from the test buffer before we save")

(defvar-local java-gradle-process nil
  "The process where this buffer (project) is running gradle.
Should only be running in a magit status buffer")

(defvar java-gradle-processes nil
  "alist of java-gradle-process values by [path")

(defvar-local java-gradle-task "test"
  "The gradle task (and arguments) to run.

ex. `clean service:test -Denv=stage'

The task will be run with the -t and --info flags for verbose continuous build.")

(defvar java-source-directory (concat (getenv "HOME") "/src")
  "the root directory where java projects are checked out")
(defvar java-source-directory-regexp
  (replace-regexp-in-string "\\." "\\\\." java-source-directory)
  "regexp to recognize the root directory where java projects are carried out")

(defun java-gradle-process (buffer-or-string)
  (let ((path (java-get-trimmed-path buffer-or-string)))
    ))

(defun java-get-trimmed-path (buffer-or-string)
  (message "java-get-trimmed-path (%s)" buffer-or-string)
  (let* ((buffer (get-buffer buffer-or-string))
         (full-path (if buffer
                        (expand-file-name (buffer-file-name buffer))
                      (expand-file-name buffer-or-string)))
         (path (replace-regexp-in-string java-source-directory-regexp "" full-path)))
    (message "trimmed path is: %s" path)
    path))

(let ((path "/MyProject/MySubbroj/src/test/java/my/project/subproject/testng/SomeTest.java"))
  (cl-assert (string-equal (java-get-trimmed-path (concat java-source-directory path))
                           path)))

(defun java-get-word-or-region ()
  (if (region-active-p)
      (buffer-substring (region-beginning) (region-end))
    (word-at-point)))

(defun java-snip-prev-buffer()
  "switch to the previous snip buffer"
  (interactive)
  (java-snip-switch-buffer (lambda (x) (- x 1))))

(defun java-snip-switch-buffer (successor-fn)
  (let* ((bufname (buffer-name))
         (regexp "[0-9][0-9]*")
         (start (string-match regexp bufname))
         (end (string-match ">" bufname)))
    (unless start (message "%s didn't match /%s/" bufname regexp))
    (when start
      (let* ((match (substring bufname start end))
             (next (funcall successor-fn (string-to-number match)))
             (nubuf (concat (substring bufname 0 (- start 1)) "<" (number-to-string next) ">"))
             )
        (if (buffer-live-p (get-buffer nubuf))
            (switch-to-buffer nubuf)
          (message "Buffer %s does not exist" nubuf))
        ))))

(define-minor-mode java-snip-mode
  "A minor mode with keybinds for snipped java code"
  nil
  " java-snip" ;; modeline symbol
  '(keymap
    (32  . scroll-up-command) ;; SPC
    (?m . java-finder-menu)
    (?n . next-line)
    (?p . previous-line)
    (?l . recenter-top-bottom)
    (?f . forward-char)
    (?b  . backward-char)
    (?N  . java-snip-next-buffer)
    (?P  . java-snip-prev-buffer)
    ))

(defun java-snip-current-defun ()
  "Copy the defun at point into a new buffer for reading"
  (interactive)
  (let* ((mode major-mode)
         (defun-bounds (bounds-of-thing-at-point 'defun))
         (starting-line (line-number-at-pos (car defun-bounds)))
         (ending-line (line-number-at-pos (cdr defun-bounds)))
         (filename (buffer-file-name))
         (this-defun (thing-at-point 'defun)))
    (pop-to-buffer (format "*defun(%s)*" (symbol-name mode)) 't 'norecord)
    (rename-uniquely)
    (funcall (symbol-function mode))
    (java-snip-mode)
    (insert (format "%s (L%s-%s)\n" filename starting-line ending-line))
    (comment-region (point-max) (point-min))
    (insert this-defun)
    (setq buffer-read-only 't)
    (setq buffer-scroll-margin 0)))

(defun java-call-finder-from-menu (finder-fn)
  (let ((word (if (region-active-p)
                  (buffer-substring (region-beginning) (region-end))
                (word-at-point)))
        (dir (magit-toplevel)))
    (funcall finder-fn word dir)))

(defun java-set-jdtls-directory ()
  "sets the JDT.LS workspace directories to match the name of the currently selected repository"
  (interactive)
  (let (remote/repo dir app)
    (setq remote/repo (cadr (java-get-remote-and-userrepo)))
    (setq dir (concat (getenv "HOME") "/.emacs.d/workspaces/" remote/repo ))
    (setq app (cadr (string-split remote/repo "/")))
    ;;(mapcar* #'(lambda (f) (lsp-workspace-folders-remove f)) (lsp-workspace-folders))
    (setq lsp-java-workspace-dir (concat dir "/"))
    (setq lsp-java-workspace-cache-dir (concat dir "/.cache/"))
    (setq dap-java-java-command (concat "REGION=us-west-2 SERVICE=" app " java"))
    (customize-apropos "\\\(lsp-java-workspace.*\\\|dap-java-java-command\\\)")
    )
  )

(defun java-jdb-menu-keymap ()
  (list 'keymap "JDB Helpers"
        '(99 "run" . gud-cont) ;; c
        ))

(defun java-gradle-menu-keymap ()
  (list 'keymap "Gradle"
        ;; if there are no gradle procs, start option goes at the top
        (if (null java-gradle-processes)
            '(110 "start new Gradle proc" . java-gradle-process-start)) ;; n
        '(101 "goto end of gradle buffer" . java-gradle-buffer-goto-end) ;; e
        '(112 "previous failure" . java-gradle-buffer-previous-failure) ;; p
        ;; we only show the "kill task" item if there are some gradle tasks
        ;; TODO conditionally show this option only when *this buffer* has matching tasks
        (if java-gradle-processes '(107 "kill task" . java-gradle-process-kill)) ;; k
        ;; if gradle processes exist, put the start command at the bottom
        (if java-gradle-processes '(110 "start new task" . java-gradle-process-start)))) ;; n

(defun java-github-menu-keymap ()
  (list 'keymap "Git"
        '(?g "open link to this line" . java-open-github-url-at-point)
        '(?m "Pull Requests (this user)" . java-open-github-pull-requests-this-user)
        '(?P "Pull Requests (this repo)" . java-open-github-pull-requests-this-repo)
        '(?O "Open PR (this branch)" . java-open-new-pull-request-this-branch)
        (when (or (eq major-mode 'java-mode) (eq major-mode 'magit-status-mode))
          '(?b "open latest build" . hulu-goto-latest-build))
        (when (or (eq major-mode 'java-mode) (eq major-mode 'magit-status-mode))
          '(?S "open spinnaker deployments (this repo)" . hulu-open-spinnaker-this-repo))
        (when (or (eq major-mode 'java-mode) (eq major-mode 'magit-status-mode))
          '(?k "open Kubernetes (this namespace)" . hulu-open-k8s-dashboard-this-namespace))
        ))

(defun java-finder-menu-keymap ()
  "Generate the keymap for the java finder popup menu. Contents may change based
 on whether the current buffer has java-snip-mode enabled."
  (let ((header (if java-snip-mode
                    (format "%s %s" (java-snip-function-name) (java-snip-file-and-line))
                  "Helpers"))
        (symbol (java-get-word-or-region)))
    (list 'keymap
          header
          (cons ?t
                (cons "Jira Dashboard"
                      (lambda ()
                        (interactive)
                        (chrome-open "https://jira.hulu.com/secure/Dashboard.jspa"))))
          (when (eq major-mode 'terraform-mode)
            '(?p "Terraform (p)lan" . terraform-init-plan))
          (unless (and (eq major-mode 'java-mode) java-snip-mode)
            '(?s "Snip function" . java-snip-current-defun))
          (cons ?H (cons "Git(Hub)" (java-github-menu-keymap)))
          (unless (and (eq major-mode 'java-mode) java-snip-mode)
            (cons ?D (cons "JDB" (java-jdb-menu-keymap))))
          (cons ?G (cons "Gradle" (java-gradle-menu-keymap))))))

(defun java-snip-function-name()
  (if java-snip-mode
      (save-excursion
        (goto-char 0)
        (forward-line 1)
        (search-forward-regexp "[A-Za-z0-9]*(")
        (let* ((line (buffer-substring (point-at-bol) (point-at-eol)))
               (name (string-match "[A-Za-z0-9]*(" line))
               (start (string-match "[A-ZA-z_]" line name))
               (end (string-match "(" line start)))
          (message (substring line name end))))
    ""))

(defun java-get-remote-and-userrepo (&optional remote-name)
  (interactive)
  (if (null remote-name) (setq remote-name "origin"))
  (let (remote-url)
    (setq remote-url (car (magit-config-get-from-cached-list (format "remote.%s.url" remote-name))))
    (message "remote is %s" remote-url)
    (if (not (stringp remote-url)) (error "could not get remote URL from config: are you in a git repo?"))
    (message "inside a git repo, checking remote URL for scheme")
    (cond
     ((string-prefix-p "git@" remote-url)
      (message "remote URI is a git url")
      (split-string (replace-regexp-in-string "git@" "" remote-url) ":"))
     ((string-prefix-p "http" remote-url)
      (message "https URI for remote")
      (let* ((schemeless-url (replace-regexp-in-string "https?://" "" remote-url))
             (components (split-string schemeless-url "/")))
        (list (car components)
              (replace-regexp-in-string "\.git" "" (string-join (cdr components) "/")))))
     ('t
      (message "not a recognized remote URI type")
      (error "not a recognized remote URI type"))
     )))

(defun java--get-repo-args (&optional prompt-for-repo prompt-for-ref)
  (let (hostname repository ref remote-parts)
    (setq remote-parts (java-get-remote-and-userrepo "origin"))
    (setq hostname (read-string "hostname: " (car remote-parts)))
    (if prompt-for-repo
        (setq repository (read-string "repository: " (cadr remote-parts))))
    (if prompt-for-ref
        (setq ref (read-string "branch or ref: " (magit-get-current-branch))))
    (flatten-list (list hostname repository ref))))

(defun java-open-new-pull-request-this-branch (host repo ref)
  "Open a new pull request for the current branch"
  (interactive (java--get-repo-args 'prompt-for-repo 'prompt-for-ref))
  (call-process "open" nil nil nil "-a" "Google Chrome"
                (format "https://%s/%s/pull/new/%s" host repo ref)))

(defun java-open-github-pull-requests-this-repo (host repo)
  "Open the Pull Requests for the given repo"
  (interactive (java--get-repo-args 'prompt-for-repo))
  (call-process "open" nil nil nil "-a" "Google Chrome"
                (format "https://%s/%s/pulls" host repo)))

(defun java-open-github-pull-requests-this-user (host)
  (interactive
   (let (host remote-parts)
     (setq remote-parts (java-get-remote-and-userrepo "origin"))
     (setq host (read-string "hostname: " (car remote-parts)))
     (list host)))
  (call-process "open" nil nil nil "-a" "Google Chrome"
                (format "https://%s/pulls" host)))

(defun java-with-github-url-do (host repo ref fn)
  (let* ((toplevel (magit-toplevel))
         (filename (buffer-file-name))
         (path (string-remove-prefix toplevel filename))
         (line (if (region-active-p)
                   (line-number-at-pos (region-beginning) 'absolute)
                 (line-number-at-pos nil 'absolute)))
         (end-line (if (region-active-p)
                       (line-number-at-pos (region-end) 'absolute)
                     nil)))
    (funcall fn (java-github-url host repo ref path line end-line))))

(defun java-open-github-url-at-point (host repo ref)
  "Open a link to github at the current line in the current file. Prompts
for domain, user/repo, and branch."
  (interactive (java--get-repo-args 'prompt-for-repo 'prompt-for-ref))
  (java-with-github-url-do host repo ref #'chrome-open))

(defun java-copy-github-url-as-kill (host repo ref)
  "Add a link to the current line/region in the current file to
the kill ring. Prompts for domain, user/repo, and branch."
  (interactive (java--get-repo-args 'prompt-for-repo 'prompt-for-ref))
  (java-with-github-url-do host repo ref #'kill-new))

(defun java-github-url (host repo ref file &optional line end-line)
  (cond ((and (null line) (null end-line))
         (format "https://%s/%s/blob/%s/%s" host repo ref file))
        ((null end-line)
         (format "https://%s/%s/blob/%s/%s#L%s" host repo ref file line))
        ('t
         (format "https://%s/%s/blob/%s/%s#L%s-L%s" host repo ref file line end-line))))

(defun java-make-elisp-open-snippet ()
  "make an elisp snippent that opens the current file at the current line"
  (interactive)
  (let (filename line snippet)
    (setq filename (buffer-file-name))
    (setq line (line-number-at-pos))
    (setq snippet (format "(progn (find-file \"%s\")\n       (goto-line %s))"
                          filename line))
    (kill-new snippet)))

(defun java-snip-file-and-line ()
  (if java-snip-mode
      (save-excursion
        (goto-char 0)
        (let ((first-line (buffer-substring (point-at-bol) (point-at-eol))))
          (message (string-remove-prefix (concat "// " (magit-toplevel))
                                         first-line))))
    ""))

(defun java-finder-menu ()
  "open an interactive menu of helper functions for java files"
  (interactive)
  (let (absolute-pos external-pos internal-pos-point this-window)
    ;; as a compromise, open the menu somewhere within the screen region of the
    ;; active frame. Normally these menus pop at wherever the mouse pointer is,
    ;; because they're meant to be clicked into existence
    (setq absolute-pos (window-absolute-pixel-position))
    (setq external-pos (alist-get 'outer-position (frame-geometry)))
    (setq internal-pos-point (list (- (car absolute-pos) (car external-pos))
                                   (- (cdr absolute-pos) (cdr external-pos))))
    (setq this-window (buffer-window-if-visible (buffer-name (current-buffer))))
    ;;(setq internal-pos-point (list (car external-pos) (cdr external-pos)))
    (setq internal-pos-point (list 0 0))
    (force-mode-line-update)
    (popup-menu (java-finder-menu-keymap) (list internal-pos-point this-window))))

(defun java-jdb-menu ()
  (interactive)
  (let (absolute-pos external-pos internal-pos-point this-window)
    (setq absolute-pos (window-absolute-pixel-position))
    (setq external-pos (alist-get 'outer-position (frame-geometry)))
    (setq internal-pos-point (list (- (car absolute-pos) (car external-pos))
                                   (- (cdr absolute-pos) (cdr external-pos))))
    (setq this-window (buffer-window-if-visible (buffer-name (current-buffer))))
    ;;(setq internal-pos-point (list (car external-pos) (cdr external-pos)))
    (setq internal-pos-point (list 0 0))
    (force-mode-line-update)
    (popup-menu (java-jdb-menu-keymap) (list internal-pos-point this-window))))

(defun java-gradle-menu ()
  (interactive)
  (let (absolute-pos external-pos internal-pos-point this-window)
    (setq absolute-pos (window-absolute-pixel-position))
    (setq external-pos (alist-get 'outer-position (frame-geometry)))
    (setq internal-pos-point (list (- (car absolute-pos) (car external-pos))
                                   (- (cdr absolute-pos) (cdr external-pos))))
    (setq this-window (buffer-window-if-visible (buffer-name (current-buffer))))
    ;;(setq internal-pos-point (list (car external-pos) (cdr external-pos)))
    (setq internal-pos-point (list 0 0))
    (force-mode-line-update)
    (popup-menu (java-gradle-menu-keymap) (list internal-pos-point this-window))))

;; (java-finder-menu)
;; (frame-geometry)
;; (posnp (window-absolute-pixel-position))
;; (posn-x-y (window-absolute-pixel-position))
;; (keymapp java-finder-menu-keymap)

;;(call-interactively #'rgrep 'record (vector (kbd "i")))
(defun java-shared-path-prefix (path1 path2)
  "Returns the longest prefix shared by the strings PATH1 and PATH2, minus the
base directory stored in the variable java-source-directory"
  (if (or (null path1) (null path2) (string-equal path1 "") (string-equal path2 ""))
      nil
    (if (string-prefix-p path1 path2)
        path1 ;; if path1 is a strict prefix of path2, just return path1 as a candidate
      (let ((newpath1 
             (string-join (nreverse (cdr (nreverse (split-string path1 "/")))) "/")))
        ;; otherwise remove one element off the end and try again
        (java-shared-path-prefix newpath1 path2)))))

(let ((path1 "/MyProject/MySubbroj/src/test/java/my/project/subproject/testng/SomeTest.java")
      (path2 "/MyProject/MySubbroj/src/test/java/my/project/subproject/testng/SomeOtherTest.java"))
  (java-shared-path-prefix path1 path2))

(let ((path1 "/MyProject/MySubbroj/src/test/java/my/project/subproject/testng/SomeTest.java")
      (path2 "/MyProject/MySubbroj/src/test/java/my/project/subproject/testng/SomeOtherTest.java")
      (path3 "/MyProject/MySubbroj/src/test/java/my/project/othersubproject/SomeTest.java"))
  (cl-assert (string-equal (java-shared-path-prefix path1 path2)
                           "/MyProject/MySubbroj/src/test/java/my/project/subproject/testng"))
  (cl-assert (string-equal (java-shared-path-prefix path1 path3)
                           "/MyProject/MySubbroj/src/test/java/my/project"))
  (cl-assert (string-equal (java-shared-path-prefix path2 path3)
                           "/MyProject/MySubbroj/src/test/java/my/project")))

(defun java-select-gradle-process (buffer-or-path &optional command)
  "Select a gradle-process for the given buffer's project to the supplied 
command. Reuses existing processes if they exist."
  (message "java-select-gradle-process (%s %s)" buffer-or-path command)
  (if (null java-gradle-processes) (message "no live registered processes"))
  (let (trimmed-path selection)
    (setq trimmed-path (magit-toplevel))
    (setq selection (cl-reduce
                     (lambda (selected pair)
                       (let ((path (car pair))
                             (buf (cdr pair))
                             (cmd (process-name (cdr pair))))
                         (cond ((and (string-equal path trimmed-path)
                                     (stringp command)
                                     (string-equal cmd command))
                                pair) ;; exact match
                               ((and (string-equal path trimmed-path)
                                     (null command)
                                     (yes-or-no-p (format "Select task '%s'?" (process-name (cdr pair)))))
                                pair) ;; user chose this task
                               ('t selected))))
                     java-gradle-processes
                     :initial-value (cons "" nil)))
    (cdr selection)))

(defun java-gradle-process-name (gradle-command)
  (replace-regexp-in-string "[ ][ ]*" "+" gradle-command))

(defun java-gradle-process-start (&optional gradle-command rerun-tasks stacktrace)
  (interactive (list (read-string "gradle task: " java-gradle-task)
                     (message-y-or-n-p "force-rerun gradle tasks? " 'show)
                     (message-y-or-n-p "stacktrace? " 'show)))
  (java-gradle-process-cleanup)
  (let* ((buffer-name (format "gradle-%s" (java-gradle-process-name gradle-command)))
         (path (magit-toplevel))
         (selection (java-select-gradle-process path gradle-command)))
    (setq java-test-shell-buffer buffer-name)
    (setq java-gradle-task gradle-command)
    (java-get-test-shell-current-point-max)
    (when (not (and selection (process-live-p selection)))
      (save-excursion
        (let ((process (java-gradle-start-process-in-buffer gradle-command buffer-name rerun-tasks stacktrace)))
          (unless selection
            (setq java-gradle-processes
                  (cons (cons path process) java-gradle-processes))))))))

(defun java-gradle-buffer-file (buffer-name)
  (concat ".tmp-" buffer-name "-output"))

(defun java-gradle-testng-highlight ()
  (interactive)
  (hi-lock-face-buffer "SUCCESSFUL" 'hi-green-b)
  (hi-lock-face-buffer "FAILED" 'hi-red-b)
  (hi-lock-face-buffer "SKIPPED" 'hi-yellow))

(defun java-gradle-start-process-in-buffer (gradle-command buffer-name &optional rerun-tasks stacktrace)
  (message "java-gradle-start-process-in-buffer (%s %s)" gradle-command buffer-name)
  (save-excursion
    ;;(cd (magit-toplevel))
    (let (proc cmd toplevel)
      (setq toplevel (magit-toplevel))
      (setq cmd
            (format "source ~/.bashrc ; source ~/.bash_profile ; cd %s ; TOR_REDSHIFT_USERNAME=tor_redshift_master TOR_REDSHIFT_PASSWORD='TOR_staging_rs_1' AWS_REGION='us-east-1' HOST_OVERRIDE='http://tor-beacon.aor.staging.hulu.com' JAVA_HOME=`jenv javahome` ./gradlew -t %s --info %s --no-daemon > ./%s 2>&1"
                    toplevel
                    gradle-command
                    (if rerun-tasks
                        (if stacktrace "--rerun-tasks --stacktrace"
                          "--rerun-tasks")
                      (if stacktrace "--stacktrace"
                        ""))
                    (java-gradle-buffer-file buffer-name)))
      (message "running: %s" cmd)
      (setq proc
            (start-process gradle-command
                           buffer-name
                           "/bin/bash" "-c" "--"
                           cmd))
      (find-file (concat toplevel (java-gradle-buffer-file buffer-name)))
      proc)))

(defun java-gradle-process-kill ()
  (interactive)
  (let* ((selection (java-select-gradle-process (current-buffer))))
    (message "selection is %s"  (process-name selection))
    (when (and (processp selection) (process-live-p selection))
      (kill-process selection))
    (java-gradle-process-cleanup)
    (unless (processp selection) (message "couldn't find a corresponding process"))))

(defun java-gradle-process-cleanup ()
  "clean up dead procs in \\[java-gradle-processes]"
  (setq java-gradle-processes
        (cl-reduce (lambda (acc item)
                     (cond ((null item) acc)
                           ((process-live-p (cdr item)) (cons item acc))
                           ('t acc)))
                   java-gradle-processes
                   :initial-value nil))
  'done)

(defun java-gradle-buffer-goto-end ();;&optional java-gradle-process)
  (interactive)
  (save-excursion
    (let (original-buffer buffer)
      (setq original-buffer (current-buffer))
      (if (and (processp java-gradle-process)
               (process-live-p java-gradle-process))
          (set-buffer (process-buffer java-gradle-process))
        (setq java-gradle-process
              (java-select-gradle-process (magit-toplevel))))
      (when (and java-gradle-process
                 (processp java-gradle-process)
                 (process-live-p java-gradle-process))
        (setq buffer (process-buffer java-gradle-process))
        (setq java-test-shell-buffer (buffer-name buffer))
        (select-window (buffer-window-if-visible
                        (java-gradle-buffer-file
                         java-test-shell-buffer))
                       'norecord)
        (goto-char (point-max))
        (select-window (buffer-window-if-visible
                        (buffer-name original-buffer))))
      (when (or (get-buffer
                 (java-gradle-buffer-file java-test-shell-buffer))
                (process-buffer java-gradle-process))
        (message "process exists")
        (let (buffer)
          (setq buffer (get-buffer
                        (java-gradle-buffer-file
                         java-test-shell-buffer)))
          (message "buffer is %s" (buffer-name buffer))
          (message "window is %s"
                   (buffer-window-if-visible buffer))
          (select-window (buffer-window-if-visible buffer)
                         'norecord)
          (message "selected window")
          (goto-char (point-max))
          (message "moved point to end")
          (select-window (buffer-window-if-visible
                          (buffer-name original-buffer))))))
    (if (null java-gradle-process) (message "java-gradle-process is null"))))
      
(defun java-gradle-buffer-previous-failure (&optional buffer name)
  (interactive
   (cond (java-gradle-process
          (let* ((buffer (process-buffer java-gradle-process))
                 (name (buffer-name buffer)))
            (list buffer name)))
         (java-test-shell-buffer
          (list (get-buffer java-test-shell-buffer)
                java-test-shell-buffer))
         ('t (list nil nil))))
  (save-mark-and-excursion
    (when (not (or (null buffer) (null name)))
      (setq java-test-shell-buffer (buffer-name buffer))
      (select-window (buffer-window-if-visible (buffer-name buffer)) 'norecord)
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
  (if (and (eq major-mode 'java-mode)
           (bufferp (get-buffer (java-gradle-buffer-file java-test-shell-buffer))))
      (save-excursion
        (set-buffer (java-gradle-buffer-file java-test-shell-buffer))
        (setq java-test-shell-begin (point-max)))))

(defun java-search-for-completed-tests ()
  (interactive)
  (if nil
      (progn (java-tail-output-file)
             (set-buffer java-test-shell-buffer)
             (goto-char (point-max))
             (unless (get-buffer ".tmp-gradle-tail")
               (find-file (concat (magit-toplevel) ".tmp-gradle-tail"))
               (bury-buffer ".tmp-gradle-tail"))
             (set-buffer (get-buffer ".tmp-gradle-tail")))
    (set-buffer (java-gradle-buffer-file java-test-shell-buffer)))
  (revert-buffer 'ignore-auto 'noconfirm)
  (save-excursion
    (let (pmax success fail)
      (setq pmax (point-max))
      (goto-char pmax)
      (setq success (search-backward-regexp "BUILD SUCCESSFUL" (- pmax 500) 'noerror))
      (setq fail (search-forward-regexp "BUILD FAILED" nil 'noerror))
      (cond ((eq java-test-shell-begin pmax) 'pending)
            (success 'passed)
            (fail 'failed)
            ('t nil)))))

(defun java-tail-output-file ()
  (let ((file (concat (magit-toplevel) (java-gradle-buffer-file java-test-shell-buffer)))
        (output (concat (magit-toplevel) ".tmp-gradle-tail")))
    (call-process "bash" nil nil nil "-c" "--" "tail" "-n" "8" file (format "> %s" output))
    ;;(call-process "tail" nil nil (list :file output) "-n" "8" file))
  ))

(defun java-check-shell ()
  "Alert the user in the modeline to the status of the tests"
  (interactive)
  (when (buffer-is-visible-p (java-gradle-buffer-file java-test-shell-buffer))
    (message "canceling function timers")
    (cancel-function-timers #'java-idle-test-check)
    (let ((win (buffer-window-if-visible (java-gradle-buffer-file java-test-shell-buffer)))
          (toplevel (magit-toplevel)))
      (when win
        (save-excursion
          (set-buffer (java-gradle-buffer-file java-test-shell-buffer))
          (set-window-point win (point-max))))))
  (let ((status (java-search-for-completed-tests)))
    (cond ((eq status 'passed) (message "BUILD SUCCESSFUL (C-u C-x t for full results)"))
          ((eq status 'failed) (java-handle-build-failure))
          ((eq status 'pending)
           (message "Tests not yet started")
           (java-watch-shell))
          ('t (message "Tests not finished (C-u C-x t to switch)")
              (java-watch-shell)))
    (if (consp status) (car status)
      status)))

(defun java-handle-build-failure()
  (if (eq (java-handle-compile-failure) 'none)
      (if (eq (java-handle-test-failure) 'none)
          (if (eq (java-handle-checkstyle-failure) 'none)
              (message "BUILD FAILED (C-u C-x t for full results)")))))

(defun java-handle-checkstyle-failure ()
  (let (regex failure pmax limit context)
    (setq regex "\\[ant:checkstyle\\] \\[ERROR\\]")
    (setq limit 10000)
    (message "getting point-max")
    (setq pmax (point-max))
    (message "finding previous checkstyle failure")
    (goto-char pmax)
    (setq failure (search-backward-regexp regex (- pmax limit) 'noerror))
    (when failure
      (message "found checkstyle error")
      (let (line width)
        (setq line (search-forward-regexp "\n" pmax 'noerror))
        (setq width (+ failure (window-width (minibuffer-window))))
        (setq context (min line width))
        (setq context line)
        ))
    (if context (message "found error"))
    (if (and context (> context failure)) (message "got the message"))
    (if (and context failure (> context failure))
        (message "%s(C-u C-x t for full results)" (buffer-substring failure context))
      'none)))

(defun java-handle-compile-failure ()
  (let (failure pmax context)
    (message "getting point-max")
    (setq pmax (point-max))
    (message "finding previous compile failure")
    (goto-char pmax)
    (setq failure (search-backward-regexp "error: " (max 0 (- pmax 10000)) 'noerror))
    (if failure
        (setq failure (+ (search-backward-regexp "\n" (- failure 512) 'noerror) 1)))
    (if failure (message "found compile error, gathering context lines"))
    (if failure
        (let (newlines by-width)
          (loop repeat 7 do
                (setq newlines (search-forward-regexp "
" pmax 'noerror)))
          (setq by-width (+ failure (* 7 (window-width (minibuffer-window)))))
          (setq context (min newlines by-width))))
    (if context (message "found content"))
    (if (and context (> context failure)) (message "successfully gathered lines"))
    (if (and context failure (> context failure))
        (message "%s(C-u C-x t for full results)"
                 (buffer-substring failure context))
      'none)))

(defun java-handle-test-failure ()
  (let (failure pmax context lookback lines)
    (setq lookback 1000000)
    (setq lines 9)
    (setq pmax (point-max))
    (message "finding previous test failure")
    (goto-char pmax)
    ;; search for TestNG failure message

    ;; note, passing anything but nil or 't for NOERROR moves point, so
    ;; if you failed to find "FAILED:" you would move point to the
    ;; lookback point and search FURTHER back

    (setq failure (search-backward-regexp "FAILED: " (- pmax lookback) 't))
    (if failure
        (message "found TestNG failure, gathering context lines") 
      (progn
        (message "No TestNG failure, looking for suite failures")
        (setq failure (search-backward-regexp "\n[^\n][^\n]*) FAILED" (max (point-min) (- pmax lookback)) 't)))
        (if failure
            (progn
              (message "found suite failure, gathering context lines starting at char %s" failure)
              ;; add 1 to the match point to skip leading newline
              (setq failure (+ failure 1)))
          (message "No test failure")))
    (if failure
        (let (newlines by-width)
          (loop repeat lines do
                (setq newlines (search-forward-regexp "
" pmax 'noerror)))
          (setq by-width (+ failure (* lines (window-width (minibuffer-window)))))
          (setq context (min newlines by-width))))
    (if context
        (message "found newlines")
      (message "no newline"))
    (if (and context (> context failure))
        (message "successfully gathered test failure context")
      (message "no context lines"))
    (if (and context failure (> context failure))
        (message "%s(C-u C-x t for full results)"
                 (buffer-substring failure context))
      (progn
        (message "no test failure to print")
        'none))))

;; note - this used to be run in a recursive function with a TTL, but now runs
;; via an idle timer so it doesn't lock emacs when tests run. The idle timer is
;; only cleared when #'java-check-test-results is run with a non-nil value for
;; the SWITCH-BUFFER prefix argument
(defun java-watch-shell ()
  "Wait for the java shell to show that tests are completed"
  (interactive)
  (if (eq major-mode 'java-mode)
      '()
      ;;(run-with-idle-timer 1.0 'repeat #'java-idle-test-check)
    ))

(defun java-idle-test-check (&optional bufname)
  "Timer function to check for tests results when Emacs is idle"
  (let* ((buffer-name (if (null bufname) (java-gradle-buffer-file java-test-shell-buffer) bufname)))
    (if (eq major-mode 'java-mode)
        (java-check-shell))))

(defun java-check-test-results (&optional switch-buffer)
  "Check the java-test-shell-buffer to see if the tests have finished.

If SWITCH-BUFFER is non nil, for instance if called with a prefix
argument, cancels function timers running #'java-idle-test-check,
on the assumption that that you've seen the results and don't
need to be reminded."
  (interactive "P")
  (when switch-buffer
    (let (toplevel)
      (setq toplevel (magit-toplevel))
      (cancel-function-timers #'java-idle-test-check)
      (if (eq (length (frame-list)) 1)
          (display-buffer (concat toplevel (java-gradle-buffer-file java-test-shell-buffer))
                          'display-buffer-in-previous-window)
        (display-buffer (java-gradle-buffer-file java-test-shell-buffer)))))
  (unless switch-buffer
    (java-check-shell)))

;;(setq debug-on-error 't)
;;(setq debug-on-error nil)

(defun java-default-highlights ()
  "adds some face-highlights to make certain comments and uses of standard
output or error streams for error-trapping stand out"
  ;; added to hooks below
  (interactive) ;; for debugging purposes
  (hi-lock-face-buffer "\\(Note\\|NOTE\\|note\\)\\b" 'hi-yellow)
  ;;(hi-lock-face-buffer "[Nn][Oo][Tt][Ee]" 'hi-yellow)
  (hi-lock-face-buffer "System[.]\\(out\\|err\\)[.]format" 'hi-yellow)
  (hi-lock-face-buffer "System[.]\\(out\\|err\\)[.]print" 'hi-yellow)
  (hi-lock-face-buffer "System[.]\\(out\\|err\\)[.]println" 'hi-yellow))

;; (setq before-save-hook '())
(add-hook 'before-save-hook #'java-get-test-shell-current-point-max)
(add-hook 'after-save-hook #'java-watch-shell)
;; (add-hook 'after-save-hook (lambda nil (interactive) (when (eq major-mode 'java-mode) (sleep-for 2) (java-jdb-start))))

;; TODO make these a single keychord that toggles between source/test

;; (setq java-mode-hook nil) ;; reset the java-mode-hook, if you're developing

;; add hooks
(add-hook 'java-mode-hook #'java-default-highlights)
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

;; finder menu
(add-hook 'java-mode-hook
          #'(lambda () (local-set-key (kbd "C-x m") #'java-finder-menu)))
(add-hook 'magit-mode-hook
          #'(lambda () (local-set-key (kbd "C-x m") #'java-finder-menu)))

(add-hook 'java-mode-hook
          #'(lambda () (local-set-key (kbd "C-x d") #'java-jdb-menu)))

;; gradle menu
(add-hook 'java-mode-hook
          #'(lambda () (local-set-key (kbd "C-x c") #'java-gradle-menu)))
(add-hook 'magit-mode-hook
          #'(lambda () (local-set-key (kbd "C-x c") #'java-gradle-menu)))

(add-hook 'terraform-mode-hook
          #'(lambda () (local-set-key (kbd "C-x m") #'java-finder-menu)))

;; (setq after-save-hook (cdr after-save-hook))

;;(require 'javaimp)
;;(add-to-list 'javaimp-import-group-alist '("\\`\\(my\\.company\\.\\|my\\.company2\\.\\)" . 80))
;; (setq javaimp-additional-source-dirs '("generated-sources/thrift"))
;; (add-hook 'java-mode-hook
;; 	  (lambda ()
;; 	    (local-set-key "\C-ci" 'javaimp-add-import)
;; 	    (local-set-key "\C-co" 'javaimp-organize-imports)))
;; (global-set-key (kbd "C-c j v") 'javaimp-visit-project)

(require 'lsp-java)
(add-hook 'java-mode-hook #'lsp)
