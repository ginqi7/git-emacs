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


(defun run-git-cmd (command sentinel &optional options)
  "Asyn run git shell command"
  (setq git--asyn-process-output "")
  (let* ((process-name (concat "git-" command))
         (process-buffer-name (concat "*git-" command "*"))
         (process (start-process-shell-command process-name process-buffer-name (concat "git " command " " options)))
         )
    (set-process-filter process 'git--output-to-local-buffer-variable)
    (set-process-sentinel process sentinel)))


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
      (message command)
      (start-process-shell-command "git-checkout" "git-checkout" command))))

(defun git-checkout ()
  "List all branch and checkout a selected item"
  (interactive)
  (run-git-cmd "branch" #'git-list-in-minibuffer "-a | grep \"\""))

(provide 'git)
;;; git.el ends here
