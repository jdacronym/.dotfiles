(require 'ruby-mode)

(defun bundle-exec-rspec ()
  (interactive)
  (async-shell-command
   "source ~/.bashrc; bundle exec rspec;"
   "*RSpec*"))

(add-to-list 'ruby-mode-hook (lambda () (interactive)
			       (local-set-key (kbd "C-x t") #'bundle-exec-rspec)))
