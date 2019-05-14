;;; say-what-im-doing.el --- dictate what you're doing with text to speech
;;
;; Filename: say-what-im-doing.el
;; Description: Make emacs say what you're currently doing with text-to-speech
;; Author: Benaiah Mischenko
;; Maintainer: Benaiah Mischenko
;; Created: Tue May 10 2016
;; Version: 0.2
;; Package-Version: 0.2
;; Package-Requires: ()
;; Last-Updated: Tue May 12 2016
;;           By: Benaiah Mischenko
;;     Update #: 1
;; URL: http://github.com/benaiah/say-what-im-doing
;; Doc URL: http://github.com/benaiah/say-what-im-doing
;; Keywords: text to speech, dumb, funny
;; Compatibility: GNU Emacs: 24.x, 25.x
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; This makes Emacs say every command you perform out loud, using
;; text-to-speech. There's really no point.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(defvar say-what-im-doing-common-commands
  '(
    backward-char
    delete-backward-char
    backward-delete-char-untabify
    execute-extended-command
    forward-char
    keyboard-quit
    newline
    next-line
    previous-line
    self-insert-command
    greader-read
    greader-read-current-line
    greader-read-line-from-point
    recenter-top-bottom
    scroll-up-command
    scroll-down-command
    isearch-printing-char
    c-electric-backspace
    c-electric-delete
    c-electric-delete-forward
    c-electric-colon
    c-electric-paren
    c-electric-brace
    c-electric-star
    c-electric-slash
    c-electric-semi&comma
    isearch-printing-char
    term-send-raw
    term-send-backspace
    magit-section-backward
    magit-section-forward
    greader-read-word-at-point)
  "These comands will not be spoken out loud, as they occur so frequently and repeatedly.")
;;(add-to-list 'say-what-im-doing-common-commands 'greader-read-word-at-point)
;;(setq say-what-im-doing-common-commands (cdr say-what-im-doing-common-commands))

(defvar say-what-im-doing-shell-command "say"
  "This is the command-line program that will be used for text-to-speech.")
(setq say-what-im-doing-shell-command "espeak")

(defun say-what-im-doing-command-hook ()
  "This is the function that will be added to `post-command-hook'."
  (if (not (member this-command say-what-im-doing-common-commands))
      (start-process "say_what_im_doing_process"
                     nil say-what-im-doing-shell-command
                     (say-what-im-doing-shell-command-opts say-what-im-doing-shell-command)
                     (say-what-im-doing-string this-command))))

(defun say-what-im-doing-shell-command-opts (command)
  (cond ((string-equal command "say") "")
        ((string-equal command "espeak") "-a35")
        ('t "")))

(defvar say-what-im-doing-buffer-change-commands
  '(switch-to-buffer
    pop-to-buffer
    other-window
    other-frame
    find-file
    handle-switch-frame
    kill-buffer
    next-multiframe-window
    w3m-next-buffer
    w3m-previous-buffer)
  "the list of commands that change the active buffer: read the name of the buffer after these")
;;(add-to-list 'say-what-im-doing-buffer-change-commands 'w3m-previous-buffer)

(defvar say-what-im-doing-echo-area-commands
  '(eval-last-sexp
    eval-buffer
    java-check-test-results)
  "list of commands that should display something in the echo area: read this after saying the command name")
;;(add-to-list 'say-what-im-doing-echo-area-commands 'java-check-test-results)

(defun say-what-im-doing-string(this-command)
  (concat
   (replace-regexp-in-string "-" " " (format "%s" this-command))
   (if (member this-command say-what-im-doing-echo-area-commands)
       (let ((msg (current-message)))
         (if (null msg) ""
           (concat ". Echo is? " msg ". ")))
     "")
   (if (member this-command say-what-im-doing-buffer-change-commands)
       (if (not (say-what-im-doing-in-minibuffer-p))
           (say-what-im-doing-buffer-name)
         "")
     "")
   (say-what-im-doing-minibuffer-contents)
   (if (buffer-is-visible-p "*Completions*")
       (let ((buffer (buffer-name)))
         (set-buffer "*Completions*")
         (let ((contents (concat "There are multiple completions. Completions buffer: "
                                 (buffer-string))))
           (set-buffer buffer)
           contents))
     "")
   ))

(defun say-what-im-doing-in-minibuffer-p ()
  (s-starts-with-p " *Minibuf-" (buffer-name)))

(defun say-what-im-doing-insert-buffer-name ()
  (if (not (say-what-im-doing-in-minibuffer-p)) (say-what-im-doing-buffer-name) ""))

(defun say-what-im-doing-buffer-name ()
  (let ((name (buffer-name)))
    (format ". %s" (say-what-im-doing-normalize-extension-names name))))

(defun say-what-im-doing-minibuffer-contents ()
  (let ((name (buffer-name)))
    (if (say-what-im-doing-in-minibuffer-p)
        (let ((contents (buffer-string)))
          (if (string-match "\\([Ff]ile\\|[Dd]irectory\\):" contents)
              (concat (replace-regexp-in-string "/" "? " contents) ". ")
            (concat contents ". ")))
      "")))

(defun say-what-im-doing-normalize-extension-names (name)
  (cond ((s-ends-with? ".el" name) (replace-regexp-in-string "\.el" ".EeEl" name))
        ('t name)))

;;(say-what-im-doing-normalize-extension-names "say-what-im-doing.el")

;;;###autoload
(define-minor-mode say-what-im-doing-mode
  "This is a mode to make emacs say every command you invoke out
  loud. This uses OS X's \"say\" by default, but can be
  configured to use a different command line program - see
  say-what-im-doing-shell-command."
  :lighter " say"
  :global t
  (if say-what-im-doing-mode
      (add-hook 'post-command-hook 'say-what-im-doing-command-hook)
    (remove-hook 'post-command-hook 'say-what-im-doing-command-hook)))

(provide 'say-what-im-doing)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; say-what-im-doing.el ends here
