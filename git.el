;;; git.el --- Run asynchronously git shell.         -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Qiqi Jin

;; Author: Qiqi Jin <ginqi7@gmail.com>
;; Keywords: tools, lisp

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; 

;;; Commands:
;;
;; Below are complete command list:
;;
;;  `git-checkout'
;;    List all branch and checkout a selected item.
;;  `git-checkout-b'
;;    Git checkout -b NEW-BRANCH-NAME.
;;  `git-diff'
;;    Select commit id and show diff between current status with the commit id.
;;  `git-show'
;;    Select commit id and show commit detail by id.
;;  `git-submit-file-m'
;;    Select a file that add to stage.
;;  `git-submit-current-file-m'
;;    Add current file to stage.
;;  `git-log--oneline'
;;    Git log --oneline show in minibuffer.
;;  `git-pull'
;;    Git pull.
;;  `git-delete-branch'
;;    Select a branch and delete it.
;;
;;; Customizable Options:
;;
;; Below are customizable option list:
;;

;;; Code:

(require 'cl-lib)
(require 'ansi-color)

(defun remove-unuseful (str)
  "Remove some unuseful char in STR.
STR is output string."
  (replace-regexp-in-string "[\015>=*]" "" (ansi-color-apply str)))

(defun git--output-to-local-buffer-variable (proc string)
  "Get Asyn process's output to put it to process' propertity.
PROC is current process.
STRING is current output."
  (when (buffer-live-p (process-buffer proc))
    (process-put proc 'output
                 (concat (process-get 'output proc) string))))


(cl-defun run-git-cmd (command &optional &key sentinel post-action item-title options grep-regex)
  "Asyn run git shell command.
COMMAND is git shell command.
SENTINEL is process sentinel.
POST-ACTION is a function after run when shell is finish.
ITEM-TITLE is ctable's title.
OPTIONS is git shell commands.
GREP-REGEX is git shell output grep regex."
  (let* ((process-name (concat "git-" command))
         (process-buffer-name (concat "*git-" command "*"))
         (git-cmd
          (if grep-regex
              (concat "git " command " " options " | grep -i " grep-regex)
            (concat "git " command " " options)))
         (process
          (start-process-shell-command process-name
                                       process-buffer-name
                                       git-cmd)))
    (message git-cmd)
    (set-process-filter process 'git--output-to-local-buffer-variable)
    (when post-action
      (process-put process 'post-action-key post-action))
    (when item-title
      (process-put process 'item-title-key item-title))
    (when sentinel (set-process-sentinel process sentinel))))


(defun remove-remote-info (str)
  "Remove remote branch info.
STR is remote branch name."
  (if (string-prefix-p "remotes/" str)
      (mapconcat #'identity (cddr (split-string str "/" t " +")) "/")
    str))

(defun git-list-in-minibuffer (process signal)
  "List branch in minibuffer, And checkout branch when select a branch.
PROCESS is current running process.
SIGNAL is current running process' signal."
  (when (memq (process-status process) '(exit))
    (let* ((output
            (split-string (process-get process 'output) "\n" t " +"))
           (item-title (process-get process 'item-title-key))
           (post-action (process-get process 'post-action-key))
           (selected-item
            (completing-read (concat item-title ": ") output)))
      (when post-action (apply post-action (list selected-item))))))

(defun git-checkout-branch (selected-item)
  "Git checkout selected branch.
SELECTED-ITEM is a selected branch name."
  (let ((command
         (format "git checkout %s" (remove-remote-info selected-item))))
    (message command)
    (start-process-shell-command "git-checkout" "*git-checkout*" command)))

(defun git-checkout ()
  "List all branch and checkout a selected item."
  (interactive)
  (run-git-cmd "branch"
               :sentinel #'git-list-in-minibuffer
               :options "-a"
               :item-title "branch"
               :post-action #'git-checkout-branch
               :grep-regex "\"\""))

(defun git-checkout-b (new-branch-name)
  "Git checkout -b NEW-BRANCH-NAME."
  (interactive "sNew branch Name: ")
  (run-git-cmd "checkout" :options (concat "-b " new-branch-name)))

(defun git-show-diff-in-buffer (process signal)
  "List branch in minibuffer, And checkout branch when select a branch.
PROCESS is current running process.
SIGNAL is current running process' signal"
  (when (memq (process-status process) '(exit))
    (let ((diff-info (process-get process 'output))
          (the-buffer (process-buffer process)))
      (with-current-buffer the-buffer
        (erase-buffer)
        (insert diff-info)
        (diff-mode))
      (switch-to-buffer the-buffer))))


(defun git-diff ()
  "Select commit id and show diff between current status with the commit id."
  (interactive)
  (run-git-cmd "log"
               :sentinel #'git-list-in-minibuffer
               :options "--oneline"
               :item-title "Commit Id"
               :post-action #'git-diff-by-id
               :grep-regex "\"\""))

(defun git-show ()
  "Select commit id and show commit detail by id."
  (interactive)
  (run-git-cmd "log"
               :sentinel #'git-list-in-minibuffer
               :options "--oneline"
               :item-title "Commit Id"
               :post-action #'git-diff-by-id
               :grep-regex "\"\""))


(defun git-diff-by-id (selected-item)
  "Show commit detail by id.
SELECTED-ITEM contains commit it."
  (run-git-cmd "show "
               :sentinel #'git-show-diff-in-buffer
               :options (car (split-string selected-item " +" nil))
               :grep-regex "\"\""))


(defun run-git-script (script)
  "Run git script.
SCRIPT is a git shell script."
  (start-process-shell-command "git-script" "git-script" script))


(defun git-submit-file-m(file-name msg)
  "Select a file that add to stage.
And commit it with MSG.
And push it to remote repository.
FILE-NAME is file name needed to be committed.
MSG is commit message"
  (interactive "fSelect commit file: \nsCommit Info: ")
  (let ((cmd
         (format "git add %s; git commit -m \"%s\";git push" file-name msg)))
    (message cmd)
    (run-git-script cmd)))

(defun git-submit-current-file-m(msg)
  "Add current file to stage.
And commit it with MSG.
And push it to remote repository.
MSG is commit message."
  (interactive "sCommit Info: ")
  (git-submit-file-m (buffer-file-name) msg))

(defun git-log--oneline ()
  "Git log --oneline show in minibuffer."
  (interactive)
  (run-git-cmd "log"
               :sentinel #'git-list-in-minibuffer
               :options "--oneline"
               :item-title "Commit Id"
               :grep-regex "\"\""))

(defun git-pull () "Git pull." (interactive) (run-git-cmd "pull"))

(defun git-delete-branch-by-name (branch-name)
  "Delete branch by name.
BRANCH-NAME is the branch name needed to be deleted."
  (when (y-or-n-p
         (format "Are you sure you want to delete branch \"%s\" ?" branch-name))
    (run-git-cmd "branch" :options (concat "-D " branch-name))))

(defun git-delete-branch ()
  "Select a branch and delete it."
  (interactive)
  (run-git-cmd "branch"
               :sentinel #'git-list-in-minibuffer
               :item-title "Branch"
               :post-action #'git-delete-branch-by-name
               :grep-regex "\"\""))

(provide 'git)
;;; git.el ends here
