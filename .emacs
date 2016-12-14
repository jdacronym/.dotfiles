(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(column-number-mode 1)

(set-face-attribute 'default nil :height 180)
(set-face-attribute 'mode-line nil :height 160)
(set-face-attribute 'mode-line-inactive nil :height 140)

(setq-default indent-tabs-mode -1)

(add-to-list 'load-path (concat (getenv "HOME") "/.emacs.d/lisp"))

;; load convenience functions and secret stuff
(load "cookiejar")
(load "secrets")

(global-set-key (kbd "C-x p") #'ping)

;; make widnow-navigation easier
(global-set-key (kbd "C-'") #'other-window)
(global-set-key (kbd "C-;") #'(lambda () (interactive) (switch-to-buffer (other-buffer))))
(global-set-key (kbd "C-:") #'(lambda () (interactive) (display-buffer (other-buffer))))
(global-set-key (kbd "C->") #'switch-to-next-buffer)
(global-set-key (kbd "C-<") #'switch-to-prev-buffer)

(setq browse-url-browser-function #'w3m-goto-url-new-session)

;; packaging
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))
(package-initialize)

;;;;; which-key (where does #'use-package come from?)
;;(use-package which-key :config (which-key-mode))

(autoload 'w3m-browse-url "w3m" "Ask a WWW browser to fetch and render a URL." t)

;; path stuff (kludgey, will be changed eventually)
(defconst *orig-path* (getenv "PATH") "The original value of the PATH variable")
(defvar current-paths (split-string *orig-path* ":") "The current path directories")
(defvar additional-paths
  '("/usr/local/sbin"
    "/usr/local/bin"
    "/opt/local/sbin"
    "/opt/local/bin")
  "paths to add for shells and binary search")

(defvar orig-gem-home (getenv "GEM_HOME"))
(defvar orig-gem-root (getenv "GEM_ROOT"))
(defvar orig-gem-path (getenv "GEM_PATH"))

;; add bin/ and sbin/ to the path
(mapcar #'(lambda (path) (add-to-list 'exec-path path)) additional-paths)

(defun reset-paths ()
  (interactive)
  (setenv "PATH" *orig-path*)
  (setq current-paths (split-string *orig-path* ":"))
  (setenv "GEM_PATH" orig-gem-path))

(defun env-fix-path ()
  (interactive)
  (mapcar #'(lambda (path) (add-to-list 'current-paths path))
	  ;; this list is in reverse order of how it will appear on the paths
	  additional-paths)
  (setenv "PATH" (cl-reduce #'(lambda (acc item) (concat acc ":" item)) current-paths)))

(require 'woman)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes (quote ("ed4b75a4f5cf9b1cd14133e82ce727166a629f5a038ac8d91b062890bc0e2d1b" "afbb40954f67924d3153f27b6d3399df221b2050f2a72eb2cfa8d29ca783c5a8" "bb4733b81d2c2b5cdec9d89c111ef28a0a8462a167d411ced00a77cfd858def1" "1e90834a232ff3b63c41b00e484754293a5c38d73080ddc6f77db72feb0b2f98" "49b36c626548d200f97144cedb44f0a48020fda221b9e2930dc7d95ef4013eb1" "40f6a7af0dfad67c0d4df2a1dd86175436d79fc69ea61614d668a635c2cd94ab" default)))
 '(inhibit-startup-screen t))

(setq speak-command "say")

(defun pry ()
  (interactive)
  (async-shell-command "pry" "*pry*")
  (select-buffer-window-or-switch "*pry*"))

(defvar iex-arg ""
  "Argument passed to IEx (say, a filename)
defaults to the empty string")

(defun iex ()
  (interactive)
  (async-shell-command "source ~/.bashrc; iex" "*iex*")
  (select-buffer-window-or-switch "*iex*"))

(defun mix-args-deps-compile ()
  (interactive)
  (setq mix-args "deps.compile"))

(defun mix-args-deps-get ()
  (interactive)
  (setq mix-args "deps.get"))

(defun mix-args-compile ()
  (interactive)
  (setq mix-args "compile"))

(defun mix-args-test ()
  (interactive)
  (setq mix-args "test"))

(defun mix-args-help ()
  (interactive)
  (setq mix-args "help"))

(defun mix ()
  (interactive)
  (let ((name "*mix*"))
    (async-shell-command (concat "cd " project-directory "; "
				 "source ~/.bashrc; "
				 "mix " mix-args)
			 name)
    (select-buffer-window-or-switch name)))

(defun clj ()
  "Start the clojure repl using `lein repl`"
  (interactive)
  (async-shell-command "lein repl" "*clojure*")
  (switch-to-buffer-other-window "*clojure*"))

(load-theme 'ubuntu)
;; (load-theme 'zenburn)
;; (load-theme 'ujelly) ; high-contrast alternative
;; (load-theme 'abyss)  ; high-contrast and slightly angry alternative

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

;; final user setup
(env-fix-path)

;(require 'magit) ;; need to test that this exists before requiring
(eval-after-load 'magit
  (global-set-key (kbd "C-x g") #'magit-status))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
