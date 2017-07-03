(require 'ruby-mode)
(require 'cl-lib)

(defun bundle-exec-rspec ()
  (interactive)
  (async-shell-command
   (concat "source ~/.bashrc;"
	   "cd " (ruby-project-root-for-file (buffer-file-name)) ";"
	   "bundle exec rspec;")
   "*RSpec*"))

(add-to-list 'ruby-mode-hook (lambda () (interactive)
			       (local-set-key (kbd "C-x t") #'bundle-exec-rspec)))

(defun ruby-lib-or-app-file-path (filename)
  (cond ((string-match-p "app/" filename) (replace-regexp-in-string "app\/" "spec/" filename))
	((string-match-p "spec\/lib\/" filename) (replace-regexp-in-string "spec\/" "" filename))
	((string-match-p "lib\/" filename) (replace-regexp-in-string "lib\/" "spec/lib/" filename))
	((string-match-p "spec/" filename) (replace-regexp-in-string "spec\/" "app/" filename))
	('t (cl-assert (equal 1 nil)))))

;; note: changing *_spec.rb to *.rb and vice-versa happens elsewhere
(cl-assert (equal (ruby-lib-or-app-file-path "foo/spec/models/example_spec.rb")
		  "foo/app/models/example_spec.rb"))
(cl-assert (equal (ruby-lib-or-app-file-path "foo/app/models/example.rb")
		  "foo/spec/models/example.rb"))
(cl-assert (equal (ruby-lib-or-app-file-path "foo/spec/lib/example.rb")
		  "foo/lib/example.rb"))
(cl-assert (equal (ruby-lib-or-app-file-path "foo/lib/example.rb")
		  "foo/spec/lib/example.rb"))

(defun ruby-test-file-p (filename)
  (string-match-p "_spec\.rb" filename))

(defun ruby-source-file-p (filename)
  (not (ruby-test-file-p filename)))

(defun source-file-for-test (filename)
  (cond ((ruby-test-file-p filename)
	 (let ((sans-spec-extension (replace-regexp-in-string "_spec" "" filename)))
	   (ruby-lib-or-app-file-path sans-spec-extension)))
	('t (cl-assert (equal '("filename doesn't end in _spec.rb" . filename) nil)))))

(cl-assert (equal (source-file-for-test "foo/spec/models/example_spec.rb")
		  "foo/app/models/example.rb"))
(cl-assert (equal (source-file-for-test "foo/spec/lib/example_spec.rb")
		  "foo/lib/example.rb"))

(defun test-file-for-source (filename)
  (cond ((ruby-source-file-p filename)
	 (let ((with-spec-extension (replace-regexp-in-string "\.rb" "_spec.rb" filename)))
	   (ruby-lib-or-app-file-path with-spec-extension)))
	('t (cl-assert (equal '("This is a test already!" . filename) nil)))))

(cl-assert (equal (test-file-for-source "foo/app/models/example.rb")
		  "foo/spec/models/example_spec.rb"))
(cl-assert (equal (test-file-for-source "foo/lib/example.rb")
		  "foo/spec/lib/example_spec.rb"))

(defun ruby-complement-filename (filename)
  (if (ruby-test-file-p filename)
      (source-file-for-test filename)
    (test-file-for-source filename)))

(cl-assert (equal (ruby-complement-filename "foo/spec/models/example_spec.rb")
		  "foo/app/models/example.rb"))
(cl-assert (equal (ruby-complement-filename "foo/spec/lib/example_spec.rb")
		  "foo/lib/example.rb"))
(cl-assert (equal (ruby-complement-filename "foo/app/models/example.rb")
		  "foo/spec/models/example_spec.rb"))
(cl-assert (equal (ruby-complement-filename "foo/lib/example.rb")
		  "foo/spec/lib/example_spec.rb"))

(defun ruby-complementary-file ()
  (interactive)
  (let ((fname (ruby-complement-filename (buffer-file-name))))
    (find-file fname)))

(defun ruby-complementary-file-other-window ()
  (interactive)
  (let ((fname (ruby-complement-filename (buffer-file-name))))
    (find-file-other-window fname)))

(add-to-list 'ruby-mode-hook (lambda () (interactive)
			       (local-set-key (kbd "C-x s") #'ruby-complementary-file)))
(add-to-list 'ruby-mode-hook (lambda () (interactive)
			       (local-set-key (kbd "C-x S") #'ruby-complementary-file-other-window)))

(defvar ruby-last-good-root ""
  "The last good value for project root")
(defun ruby-project-root-for-file (filename)
  (setq ruby-last-good-root
	(cond ((null filename) ruby-last-good-root)
	      ((string-match-p "spec\/" filename) (replace-regexp-in-string "spec\/.*" "" filename))
	      ((string-match-p "app\/" filename) (replace-regexp-in-string "app\/.*" "" filename))
	      ((string-match-p "lib\/" filename) (replace-regexp-in-string "lib\/.*" "" filename))
	      ('t filename))))

(cl-assert (equal (ruby-project-root-for-file
		   "/usr/home/foobar/src/some-project/spec/models/some_model_spec.rb")
		  "/usr/home/foobar/src/some-project/"))
(cl-assert (equal (ruby-project-root-for-file
		   "/usr/home/foobar/src/some-project/app/models/some_model.rb")
		  "/usr/home/foobar/src/some-project/"))
(cl-assert (equal (ruby-project-root-for-file
		   "/usr/home/foobar/src/some-project/lib/services/some_service.rb")
		  "/usr/home/foobar/src/some-project/"))
