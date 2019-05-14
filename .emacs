(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(column-number-mode 1)

(require 'cl-lib)
(require 'ffap)
(require 'json)

;; (require 'webkit)

(set-face-attribute 'default nil :height 170)
(set-face-attribute 'mode-line nil :height 140)
(set-face-attribute 'mode-line-inactive nil :height 140)
;;(set-face-attribute 'default nil :height 140)
;;(set-face-attribute 'default nil :height 160)
;;(set-face-attribute 'default nil :height 170)
;;(set-face-attribute 'default nil :height 190)

(setq-default indent-tabs-mode nil)
(setq indent-tabs-mode nil)

(setq-default tab-width 4)
(setq tab-width 4)

(add-to-list 'load-path (concat (getenv "HOME") "/.emacs.d/lisp"))

;; load convenience functions and secret stuff
(load "cookiejar")
;(load "secrets")
(load "java")
(load "ruby")
(load "scala")
(when (eq system-type 'darwin)
  (setq browse-url-browser-function #'browse-url-generic)
  (setq browse-url-generic-program "open")
  (setq browse-url-generic-args (list "-a" "Firefox")))

(setq-default js-indent-level 2)
(setq js-indent-level 2)

(global-set-key (kbd "C-x C-g") #'rgrep)

;; make widnow-navigation easier
(global-set-key (kbd "C-'") #'other-window)
;; the above conflicts with a keybinding in org-mode, but by default
;; that command is also bound to "C-,", so we'll override it
;; TODO: make this work, somehow?
(add-hook 'org-mode-hook  #'(lambda () (interactive) (local-set-key (kbd "C-'") #'other-window)))
(global-set-key (kbd "C-;") #'(lambda () (interactive) (switch-to-buffer (other-buffer))))
(global-set-key (kbd "C-:") #'(lambda () (interactive) (display-buffer (other-buffer))))
(global-set-key (kbd "C->") #'switch-to-next-buffer)
(global-set-key (kbd "C-<") #'switch-to-prev-buffer)
;; get rid of isearch backward, which I never use
(global-set-key (kbd "C-r") nil)

;;(setq browse-url-browser-function #'w3m-goto-url-new-session)

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

(defun setup-work-session ()
  "Spawn three windows, put them on separate monitors, maximize
them, and show certain contents. The 'left' window will display
the *shell* buffer, the 'right' window some other buffer than the
one open when #'setup-work-session was called, and the 'main'
window will be left on its original buffer.

This function may require tweaking as your monitor arrangement
changes."
  (interactive)
  (let ((middle-frame (selected-frame))
        (left-frame (make-frame '((name . "left"))))
        (right-frame (make-frame '((name . "right")))))
    (set-frame-position left-frame -1000 100)
    (set-frame-position right-frame 2000 100)
    (select-frame left-frame)
    (toggle-frame-maximized)
    (let ((left-window-buffer (if java-test-shell-buffer java-test-shell-buffer "*shell*"))
          (jdb-buffer (if java-jdb-process (java-jdb-buffer) nil)))
      (switch-to-buffer left-window-buffer 'norecord 'force-same-window)
      (split-window-below)
      (if jdb-buffer (switch-to-buffer jdb-buffer 'norecord 'force-same-window)))
    (select-frame right-frame)
    (toggle-frame-maximized)
    (switch-to-buffer (other-buffer))
    (select-frame middle-frame)))

(defun teardown-work-session ()
  (interactive)
  (let ((main-monitor-frame (make-frame '((name . "main")))))
    (select-frame main-monitor-frame)
    (toggle-frame-maximized)
    (delete-other-frames)))

(if (eq system-type 'darwin)
    (add-to-list 'woman-manpath "/Library/Developer/CommandLineTools/usr/share/man"))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(browse-url-firefox-arguments nil)
 '(custom-safe-themes
   (quote
    ("bfdcbf0d33f3376a956707e746d10f3ef2d8d9caa1c214361c9c08f00a1c8409" "95f80c9b1ae8afcc2c8d66750252b4d6ae19aef46c2d458c5fe5911e6f09d0ce" "ed4b75a4f5cf9b1cd14133e82ce727166a629f5a038ac8d91b062890bc0e2d1b" "afbb40954f67924d3153f27b6d3399df221b2050f2a72eb2cfa8d29ca783c5a8" "bb4733b81d2c2b5cdec9d89c111ef28a0a8462a167d411ced00a77cfd858def1" "1e90834a232ff3b63c41b00e484754293a5c38d73080ddc6f77db72feb0b2f98" "49b36c626548d200f97144cedb44f0a48020fda221b9e2930dc7d95ef4013eb1" "40f6a7af0dfad67c0d4df2a1dd86175436d79fc69ea61614d668a635c2cd94ab" default)))
 '(inhibit-startup-screen t)
 '(package-selected-packages
   (quote
    (babel-repl urlenc http grapnel pdf-tools epc bitlbee helm-helm-commands helm cljr-helm ac-emoji abl-mode jira-markup-mode xml-rpc jira ucs-cmds ujelly-theme ensime scala-mode zenburn-theme anti-zenburn-theme abyss-theme erc-hipchatify copy-as-format w3m mastodon markdown-preview-mode markdown-mode+ markdown-mode gh-md flymd ubuntu-theme magit)))
 '(pocket-reader-open-url-default-function (quote w3m-goto-url-new-session)))

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

;; (load-theme 'ubuntu)
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

(eval-after-load 'rcirc
  '(global-set-key (kbd "C-c C-SPC") #'rcirc-next-active-buffer))
(global-set-key (kbd "C-c C-SPC") #'rcirc-next-active-buffer)


;; final user setup
(env-fix-path)

(eval-after-load 'magit (global-set-key (kbd "C-x g") #'magit-status))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(defvar latex-file "" "LaTeX filename to compile")
(defun latex-compile(texfile)
  (interactive "bbuffer:")
  (setq latex-file texfile)
  (shell-command (concat "pdflatex " texfile) "*PDFLaTeX*")
  (sleep-for 1)
  (find-file-other-window (concat (substring texfile 0 (string-match "\.tex$" texfile))
				 ".pdf")))

(add-hook 'php-mode-hook
	  #'(lambda ()
	      (auto-complete-mode t)
	      (require 'ac-php)
	      (setq ac-sources  '(ac-source-php ) )
	      (yas-global-mode 1)

	      (define-key php-mode-map  (kbd "C-]") 'ac-php-find-symbol-at-point)   ;goto define
	      (define-key php-mode-map  (kbd "C-t") 'ac-php-location-stack-back))) ;go back

;; TODO: set faces for different contexts in a more robust way

(if (not (equal "lintel" (system-name)))
    (set-face-attribute 'default nil :family "Monospace"))

;; jira markup mode
(autoload 'jira-markup-mode "jira-markup-mode"
       "Major mode for editing JIRA markup files" t)
    (setq auto-mode-alist
       (cons '("\\.text" . jira-markup-mode) auto-mode-alist))

(require 'bitlbee)

(require 'jira)

(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(require 'pocket-reader)
(require 'el-pocket)
(require 'greader)
;; use `M-x el-pocket-authorize` to capture pocket auth if pocket-reader breaks
(require 'say-what-im-doing)
(require 'urlenc)
(if (eq system-type 'darwin)
    (require 'work))
;;(require 'magit)
