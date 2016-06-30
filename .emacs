(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(add-to-list 'load-path (concat (getenv "HOME") "/.emacs.d/lisp"))

;; load convenience functions and secret stuff
(load "cookiejar")
(load "secrets")

(global-set-key (kbd "C-x p") 'ping)
(global-set-key (kbd "C-x g") 'magit-status)

;; make widnow-navigation easier
(global-set-key (kbd "M-<left>") #'windmove-left)
(global-set-key (kbd "M-<right>") #'windmove-right)
(global-set-key (kbd "M-<up>") #'windmove-up)
(global-set-key (kbd "M-<down>") #'windmove-down)

(setq browse-url-browser-function 'w3m-goto-url-new-session)

;; packaging
(require 'package)
(setq package-archives
      '(("melpa" . "http://melpa.milkbox.net/packages/")
	("melpa-stable" . "http://stable.melpa.org/packages/")))
(package-initialize)

;;;;; which-key (where does #'use-package come from?)
;;(use-package which-key :config (which-key-mode))

(autoload 'w3m-browse-url "w3m" "Ask a WWW browser to fetch and render a URL." t)

;; add bin/ and sbin/ to the path
(add-to-list 'exec-path "/usr/local/bin")
(add-to-list 'exec-path "/usr/local/sbin")

(add-to-list 'exec-path "/opt/local/bin")
(add-to-list 'exec-path "/opt/local/sbin")

(setq orig-path (getenv "PATH"))
(setq orig-gem-home (getenv "GEM_HOME"))
(setq orig-gem-root (getenv "GEM_ROOT"))
(setq orig-gem-path (getenv "GEM_PATH"))

(defun reset-paths ()
  (setenv "PATH" orig-path)
  (setenv "GEM_PATH" orig-gem-path))

(defun env-fix-path ()
  (interactive)
  (setenv "PATH"
	  (concat "/opt/local/bin:/opt/local/sbin:/usr/local/bin:/usr/local/sbin:"
		  "/usr/local/Cellar/erlang/18.2.1/lib/erlang/man/:"
		  (getenv "PATH"))))

(add-to-list 'woman-manpath
	     "/usr/local/Cellar/erlang/18.2.1/lib/erlang/man/")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(inhibit-startup-screen t))

(setq speak-command "say")

(defun pry ()
  (interactive)
  (async-shell-command "pry" "*pry*")
  (switch-to-buffer-other-window "*pry*"))

(defun iex ()
  (interactive)
  (async-shell-command "iex" "*iex*")
  (switch-to-buffer-other-window "*iex*"))

(defun clj ()
  "Start the clojure repl using `lein repl`"
  (interactive)
  (async-shell-command "lein repl" "*clojure*")
  (switch-to-buffer-other-window "*clojure*"))

(load "idg")

;; if this doesn't happen, something went wrong above!
(amber-mode)

;; rcirc
(require 'rcirc)
;; from the rcirc manual
(eval-after-load 'rcirc
  '(defun-rcirc-command reconnect (arg)
     "Reconnect the server process."
     (interactive "i")
     (unless process
       (error "There's no process for this target"))
     (let* ((server (car (process-contact process)))
	    (port (process-contact process :service))
	    (nick (rcirc-nick process))
	    channels query-buffers)
       (dolist (buf (buffer-list))
	 (with-current-buffer buf
	   (when (eq process (rcirc-buffer-process))
	     (remove-hook 'change-major-mode-hook
			  'rcirc-change-major-mode-hook)
	     (if (rcirc-channel-p rcirc-target)
		 (setq channels (cons rcirc-target channels))
	       (setq query-buffers (cons buf query-buffers))))))
       (delete-process process)
       (rcirc-connect server port nick
		      rcirc-default-user-name
		      rcirc-default-full-name
		      channels))))

(add-hook 'rcirc-mode-hook (lambda () (rcirc-track-minor-mode 1)))

(setq rcirc-freenode '("irc.freenode.net" :channels ("#rcirc"))
