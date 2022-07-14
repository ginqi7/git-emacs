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


(cl-defun run-git-cmd (command &optional &key sentinel post-action item-title options grep-regex)
  "Asyn run git shell command"
  (setq git--asyn-process-output "")
  (let* ((process-name (concat "git-" command))
         (process-buffer-name (concat "*git-" command "*"))
         (git-cmd (if grep-regex
                      (concat "git " command " " options " | grep -i " grep-regex)
                    (concat "git " command " " options)))
         (process (start-process-shell-command process-name 
                                               process-buffer-name 
                                               git-cmd)))
    (set-process-filter process 'git--output-to-local-buffer-variable)
    (when post-action
      (process-put process 'post-action-key post-action))
    (when item-title
      (process-put process 'item-title-key item-title))
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
           (item-title (process-get process 'item-title-key))
           (post-action (process-get process 'post-action-key))
           (selected-item (completing-read (concat item-title ": ") output)))
      (when post-action
        (apply post-action (list selected-item))))))

(defun git-checkout-branch (selected-item)
  "git checkout selected branch"
  (let ((command (format "git checkout %s" (remove-remote-info selected-item))))
    (message command)
    (start-process-shell-command "git-checkout" "*git-checkout*" command)))

(defun git-checkout ()
  "List all branch and checkout a selected item"
  (interactive)
  (run-git-cmd "branch" 
               :sentinel #'git-list-in-minibuffer 
               :options "-a" 
               :item-title "branch"
               :post-action #'git-checkout-branch
               :grep-regex "\"\""))

(defun git-checkout-b (new-branch-name)
  "git checkout -b NEW-BRANCH-NAME"
  (interactive "sNew branch Name")
  (run-git-cmd "branch" 
               :options  (concat "-b " new-branch-name)))

(defun git-show-diff-in-buffer (process signal)
  "List branch in minibuffer, And checkout branch when select a branch"
  (when (memq (process-status process) '(exit))
    (let ((diff-info git--asyn-process-output)
          (the-buffer (process-buffer process)))
      (with-current-buffer the-buffer
        (erase-buffer)
        (insert diff-info)
        (diff-mode))
      (switch-to-buffer the-buffer))))


(defun git-diff ()
  "Select commit id and show diff between current status with the commit id"
  (interactive)
  (run-git-cmd "log" 
               :sentinel #'git-list-in-minibuffer 
               :options "--oneline" 
               :item-title "Commit Id"
               :post-action #'git-diff-by-id
               :grep-regex "\"\""))


(defun git-diff-by-id (selected-item)
  "Show Diff between current status with the COMMIT-ID"
  (run-git-cmd "diff" 
               :sentinel #'git-show-diff-in-buffer
               :options (car (split-string selected-item " +" nil))
               :grep-regex "\"\""))

(defun git-show ()
  "Select commit id and show commit detail by id"
  (interactive)
  (run-git-cmd "log" 
               :sentinel #'git-list-in-minibuffer 
               :options "--oneline" 
               :item-title "Commit Id"
               :post-action #'git-diff-by-id
               :grep-regex "\"\""))


(defun git-diff-by-id (selected-item)
  "Show commit detail by id"
  (run-git-cmd "show " 
               :sentinel #'git-show-diff-in-buffer
               :options (car (split-string selected-item " +" nil))
               :grep-regex "\"\""))


(defun run-git-script (script)
  (start-process-shell-command "git-script" "git-script" script))


(defun git-submit-file-m(file-name msg)
  "Select a file that add to stage, and commit it with MSG, and push it to remote repository"
  (interactive "fSelect commit file: \nsCommit Info: ")
  (let ((cmd (format "git add %s; git commit -m \"%s\";git push" file-name msg)))
    (message cmd)
    (run-git-script cmd)))

(defun git-submit-current-file-m(msg)
  "add current file to stage, and commit it with MSG, and push it to remote repository"
  (interactive "sCommit Info: ")
  (git-submit-file-m (buffer-file-name) msg))

(defun git-log--oneline ()
  "git log --oneline show in minibuffer"
  (interactive)
  (run-git-cmd "log" 
               :sentinel #'git-list-in-minibuffer 
               :options "--oneline" 
               :item-title "Commit Id"
               :grep-regex "\"\""))

(provide 'git)
;;; git.el ends here
