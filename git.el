;;; git.el --- a plugin for leetcodeme
;;; Commentary:
;;; code:
(require 'cl-lib)

(setq git--asyn-process-output "") ; This variable is using for save shell output
(make-local-variable 'git--asyn-process-output)

(defun remove-unuseful (str)
  "Remove some unuseful char in STR"
  (replace-regexp-in-string "[\015>=*]" "" (ansi-color-apply string)))

(defun git--output-to-local-buffer-variable (proc string)
  "Get Asyn process's output to git--asyn-process-output variable"
  (when (buffer-live-p (process-buffer proc))
    (setq git--asyn-process-output (concat git--asyn-process-output (remove-unuseful string)))))


(cl-defun run-git-cmd (command &optional &key sentinel options grep-regex)
  "Asyn run git shell command"
  (message (format "options: {%s}, grep-regex: {%s}" options grep-regex))
  (setq git--asyn-process-output "")
  (let* ((process-name (concat "git-" command))
         (process-buffer-name (concat "*git-" command "*"))
         (git-cmd (if grep-regex
                      (concat "git " command " " options " | grep -i " grep-regex)
                    (concat "git " command " " options)))
         (process (start-process-shell-command process-name 
                                               process-buffer-name 
                                               git-cmd
                                               )))
    (set-process-filter process 'git--output-to-local-buffer-variable)
    (when sentinel
      (set-process-sentinel process sentinel))))


(defun remove-remote-info (str)
  "Remove remote branch info"
  (if (string-prefix-p "remotes/" str)
    (mapconcat #'identity (cddr (split-string str "/" t " +")) "/")
    str))

(defun git-list-in-minibuffer (process signal)
  "List branch in minibuffer, And checkout branch when select a branch"
  (when (memq (process-status process) '(exit))
    (let* ((output (split-string git--asyn-process-output "\n" t " +"))
           (branch (remove-remote-info (completing-read "branch: " output)))
           (command (concat "git checkout " branch)))
      (start-process-shell-command "git-checkout" "git-checkout" command))))

(defun git-checkout ()
  "List all branch and checkout a selected item"
  (interactive)
  (run-git-cmd "branch" 
               :sentinel #'git-list-in-minibuffer 
               :options "-a" 
               :grep-regex "\"\""))

(defun git-show-diff-in-buffer (process signal)
  "List branch in minibuffer, And checkout branch when select a branch"
  (when (memq (process-status process) '(exit))
    (let ((diff-info git--asyn-process-output)
          (the-buffer (process-buffer process))
          )
      (with-current-buffer the-buffer
        (erase-buffer)
        (insert diff-info)
        (diff-mode))
      (switch-to-buffer the-buffer))))

(defun git-diff(&optional commit-id)
  "Show Diff between current status with the COMMIT-ID"
  (interactive "sCommit id: ")
  (run-git-cmd "diff" 
               :sentinel #'git-show-diff-in-buffer
               :options commit-id
               :grep-regex "\"\""))

(defun run-git-script (script)
  (start-process-shell-command "git-script" "git-script" script))


(defun git-submit-file-m(file-name msg)
  "Select a file that add to stage, and commit it with MSG, and push it to remote repository"
  (interactive "fSelect commit file: \nsCommit Info: ")
  (let ((cmd (concat "git add " file-name ";git commit -m \"" msg "\";git push")))
    (message cmd)
    (run-git-script cmd)))

(defun git-submit-current-file-m(msg)
  "add current file to stage, and commit it with MSG, and push it to remote repository"
  (interactive "sCommit Info: ")
  (git-submit-file-m (buffer-file-name) msg)
  )

(provide 'git)
;;; git.el ends here
