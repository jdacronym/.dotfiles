(require 'cookiejar)

;; convenience functions I wrote while working on Java code. Hopefully these
;; will be useful at work. They're not pretty and need a LOT of refactoring.

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

(defun java-source-to-test-name (filename)
  (java-class-to-test-name (java-source-to-class-name filename)))

(defun java-source-to-class-name (filename)
  (let ((sans-ext (replace-regexp-in-string ".java" "" filename)))
    (concat sans-ext ".class")))

(defun java-class-to-test-name (filename)
  (let ((sans-ext (replace-regexp-in-string ".class" "" filename)))
    (concat sans-ext "Test.java")))

(defun java-test-to-source-name (filename)
  (replace-regexp-in-string "Test.java" ".java" filename))
    

(cl-assert (string-equal (java-source-to-class-name "Example.java")
			 "Example.class"))
(cl-assert (string-equal (java-class-to-test-name "Example.class")
			 "ExampleTest.java"))
(cl-assert (string-equal (java-test-to-source-name "ExampleTest.java")
			 "Example.java"))

(add-hook 'java-mode-hook
	  #'(lambda () (local-set-key (kbd "C-c m") #'java-create-method-menu)))
(add-hook 'java-mode-hook
	  #'(lambda () (local-set-key (kbd "C-c RET") #'java-compile-buffer)))
(add-hook 'java-mode-hook
	  #'(lambda () (local-set-key (kbd "C-c t") #'java-run-junit)))

;; TODO make these a single keychord that toggles between source/test
(add-hook 'java-mode-hook
	  #'(lambda () (local-set-key (kbd "C-c T") #'java-open-test-file)))
(add-hook 'java-mode-hook
	  #'(lambda () (local-set-key (kbd "C-c S") #'java-open-source-file)))
