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
;;  `git-cherry-pick'
;;    Git cherry pick.
;;    Keybinding: M-x git-cherry-pick
;;  `git-checkout'
;;    Git checkout.
;;    Keybinding: M-x git-checkout
;;  `git-diff'
;;    Git diff.
;;    Keybinding: M-x git-diff
;;  `git-merge'
;;    Git merge.
;;    Keybinding: M-x git-merge
;;  `git-delete-branch'
;;    Git delete branch.
;;    Keybinding: M-x git-delete-branch
;;  `git-pull'
;;    Git pull.
;;    Keybinding: M-x git-pull
;;  `git-checkout-b'
;;    Git checkout -b NEW-BRANCH-NAME.
;;    Keybinding: M-x git-checkout-b
;;
;;; Customizable Options:
;;
;; Below are customizable option list:
;;

;;; Code:

(require 'cl-lib)
(require 'ansi-color)


;;; Some git base functions:
(defun git--remove-unuseful-str (str)
  "Remove some unuseful char in STR.
STR is output string."
  (replace-regexp-in-string "[\015>=*]" "" (ansi-color-apply str)))

(defun git--output-to-local-buffer-variable (proc string)
  "Get Asyn process's output to put it to process' propertity.
PROC is current process.
STRING is current output."
  (when (buffer-live-p (process-buffer proc))
    (process-put proc 'output
                 (concat
                  (git--remove-unuseful-str
                   (process-get proc 'output))
                  (git--remove-unuseful-str string)))))

(defun git--remove-remote-info (str)
  "Remove remote branch info.
STR is remote branch name."
  (if (string-prefix-p "remotes/" str)
      (mapconcat #'identity (cddr (split-string str "/" t " +")) "/")
    str))

(defun git--format-remote-info (str)
  "Format remote branch info.
STR is remote branch name."
  (string-trim-left str "remotes/"))

(defun git--run-actions-chain (cmd-link)
  "Run git cmd link."
  (apply (car cmd-link) :actions (list (cdr cmd-link))))

;;; Cl-defun some asynchronous cmd:
(cl-defun git--run-cmd (command &optional &key sentinel post-actions item-title options grep-regex)
  "Asyn run git shell command.
COMMAND is git shell command.
SENTINEL is process sentinel.
POST-ACTIONS is a function after run when shell is finish.
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
    (when post-actions
      (process-put process 'post-actions-key post-actions))
    (when item-title
      (process-put process 'item-title-key item-title))
    (when sentinel (set-process-sentinel process sentinel))))

(cl-defun git--diff-by-id (&optional &key arguments actions)
  "Show commit detail by id.
SELECTED-ITEM contains commit it."
  (git--run-cmd "show "
                :sentinel #'git-show-diff-in-buffer
                :options (car (split-string arguments " +" nil))
                :grep-regex "\"\""))

(cl-defun git--merge-branch (&optional &key arguments actions)
  "Git merge selected branch.
SELECTED-ITEM is a selected branch name."
  (let ((command
         (format "git merge --no-edit %s" (git--format-remote-info arguments))))
    (message command)
    (start-process-shell-command "git-merge" "*git-merge*" command)))

(cl-defun git--select-branch (&optional &key arguments actions)
  (git--run-cmd "branch"
                :sentinel #'git--completing-read-sentinel
                :options "-a"
                :item-title "branch"
                :post-actions actions
                :grep-regex "\"\""))

(cl-defun git--select-commit (&optional &key arguments actions)
  (git--run-cmd "log"
                :sentinel #'git--completing-read-sentinel
                :options (concat "--oneline " arguments)
                :item-title "commit id"
                :post-actions actions
                :grep-regex "\"\""))

(cl-defun git--cherry-pick (&optional &key arguments actions)
  (git--run-cmd "cherry-pick" :options arguments))

(cl-defun git--checkout-branch (&optional &key arguments actions)
  "Git checkout selected branch.
SELECTED-ITEM is a selected branch name."
  (let ((command
         (format "git checkout %s" (git--remove-remote-info arguments))))
    (message command)
    (start-process-shell-command "git-checkout" "*git-checkout*" command)))

(cl-defun git--delete-branch-by-name (&optional &key arguments actions)
  "Delete branch by name.
BRANCH-NAME is the branch name needed to be deleted."
  (when (y-or-n-p
         (format "Are you sure you want to delete branch \"%s\" ?" arguments))
    (git--run-cmd "branch" :options (concat "-D " arguments))))


;; Define some sentinel
(defun git--completing-read-sentinel (process signal)
  "List branch in minibuffer, And checkout branch when select a branch.
PROCESS is current running process.
SIGNAL is current running process' signal."
  (when (memq (process-status process) '(exit))
    (let* ((output
            (split-string (process-get process 'output) "\n" t " +"))
           (item-title (process-get process 'item-title-key))
           (post-actions (process-get process 'post-actions-key))
           (selected-item
            (completing-read (concat item-title ": ") output)))
      (when post-actions
        (print post-actions)
        (apply
         (car post-actions)
         :arguments selected-item
         :actions (list (cdr post-actions))
         )))))
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


;;; Some git interactive commands:
(defun git-cherry-pick ()
  "Git cherry pick.
1. Select another branch.
2. Select a commit in the branch.
3. Git cherry pick the commit."
  (interactive)
  (git--run-actions-chain
   '(git--select-branch git--select-commit git--cherry-pick)))

(defun git-checkout ()
  "Git checkout.
1. Select another branch.
2. Checkout the branch."
  (interactive)
  (git--run-actions-chain '(git--select-branch git--checkout-branch)))

(defun git-diff ()
  "Git diff.
1. Select a commit.
2. Show diff between current status with the commit."
  (interactive)
  (git--run-actions-chain '(git--select-commit git--diff-by-id)))

(defun git-merge ()
  "Git merge.
1. Select a branch.
2. Merge the branch."
  (interactive)
  (git--run-actions-chain '(git--select-branch git--merge-branch)))

(defun git-delete-branch ()
  "Git delete branch.
1. Select a branch.
2. Delete the branch."
  (interactive)
  (git--run-actions-chain
   '(git--select-branch git--delete-branch-by-name)))

(defun git-pull () "Git pull." (interactive) (git--run-cmd "pull"))

(defun git-checkout-b (new-branch-name)
  "Git checkout -b NEW-BRANCH-NAME."
  (interactive "sNew branch Name: ")
  (git--run-cmd "checkout" :options (concat "-b " new-branch-name)))

(provide 'git)
;;; git.el ends here
