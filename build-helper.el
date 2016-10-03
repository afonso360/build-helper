;;; build-helper --- Utilities to help build code -*- lexical-binding: t -*-

;; Copyright (C) 2016 Afonso Bordado

;; Author:  Afonso Bordado <afonsobordado@az8.co>
;; Version: 0.1
;; URL: http://github.com/afonso360/build-helper
;; Keywords: convenience

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; Goals:
;; Provide compile-history target per target (build, run, test, etc..)
;; Provide a recompile and compile function for each target
;; Provide a function to clear projects
;; Provide a target command number limit
;; Should be able to set comint option per target
;; Per project targets
;;   build-helper will switch to the project root dir before running any of the commands
;;   Require projectile and use that
;;   Per target comint setting defaults to nil
;;   Have a .build-helper-targets.el that maintains the target of these projects and their targets
;;     .build-helper-targets.el should be roughly like the following
;;     '(("project/root/dir" . ((c-mode . ((build . ("make" "gcc foo.c"))
;;     	                                   (run   . ("./a.out"))
;;     	                                   (test  . ("make run"))))
;;     			        (java-mode . ((build . ("javac main.java"))))))
;;
;;       ("project2/root/dir" . ((elisp-mode . ((test . ("elisp test command"))))))))
;;
;;
;;     From the example above, java-mode would never have run or test targets
;;     because those would be elisp functions that would never fail.  (this is just an example)
;;
;; Optionally allow compilation functions
;;   Compilation functions are based on the major-mode from which they are invoked
;;   The return value should be checked to allow multiple chains of compilation functions
;;   Maybe: Check for a modified compile-history (if that exists use that on the recompile, otherwise run it again)


;; Some code based on company-statistics.el (https://github.com/company-mode/company-statistics)
;;; Usage:

;;; Code:
(require 'projectile)
(require 'gv)

(defgroup build-helper nil
  "Helper functions to build files."
  :group 'build-helper)

(defcustom build-helper-file
  (concat user-emacs-directory ".build-helper-targets.el")
  "File to save build-helper command history."
  :type 'string)

(defcustom build-helper-capture-function nil
  "Whether we should capture compile commands called by external functions."
  :type 'boolean)

(defvar build-helper--targets '()
  "Build helper targets.")

(defun build-helper--save-targets ()
  "Save the targets to the build-helper-file."
  (with-temp-buffer
    (insert (format "(setq build-helper--targets %s)" build-helper--targets))
    (write-file build-helper-file)))

(defun build-helper--load-targets ()
  "Load the targets from build-helper-file."
  (load build-helper-file 'noerror nil 'nosuffix))

(defun build-helper-setup ()
  "Setup build-helper."
  (build-helper--load-targets)
  (add-hook 'kill-emacs-hook 'build-helper--save-targets))

(defun build-helper--add-to-target (project major target command)
  "Add COMMAND entry to PROJECT, MAJOR mode and TARGET list.
If any of PROJECT, MAJOR or TARGET are not found, create empty"
  (let ((project-list (gv-ref (assoc project build-helper--targets))))

    ;; Initialize an empty project if none is found
    (unless (gv-deref project-list)
      (let ((proj '(nil . ())))
	(setcar proj project)
	(push proj build-helper--targets)
	(setq project-list (gv-ref proj))))

    (let ((major-mode-list (gv-ref (assoc major (gv-deref project-list)))))
      ;; Initialize an empty major-mode in the project if none is found
      (unless (gv-deref major-mode-list)
	(let ((mm '(nil . ())))
	  (setcar mm major)
	  (push mm (cdr (gv-deref project-list)))
	  (setq major-mode-list (gv-ref mm))))

      (let ((target-list (gv-ref (assoc target (gv-deref major-mode-list)))))
	;; Initialize an empty target-list in the major-mode-list if none is found
	(unless (gv-deref target-list)
	  (let ((targ '(nil . ())))
	    (setcar targ target)
	    (push targ (cdr (gv-deref major-mode-list)))
	    (setq target-list (gv-ref targ))))

	(push command (cdr (gv-deref target-list)))))))

(defun build-helper--get-target (project major target)
  "Get `compile-history' list for PROJECT for MAJOR mode and TARGET.
If any of those is not found return nil."
  (let ((project-target (assoc project build-helper--targets)))
    (when project-target
      (let ((major-mode-targets (assoc major (cdr project-target))))
	(when major-mode-targets
	  (let ((final-target (assoc target major-mode-targets)))
	    (when final-target
	      (cdr final-target))))))))

(defun build-helper-run (target)
  "Run a TARGET.
This includes functions associated with the current `major-mode'.
If none of those work, a `compile' prompt with a target and `major-mode' based history.
This compile command will be executed from the projectile root directory."
  (interactive)
  ;; ask for target type interactively
  ;; Run functions, remember to check `build-helper-capture-function'
  (let* ((compile-history (build-helper--get-target (projectile-project-root)
						    major-mode
						    target))
	 (command (completing-read (format "'%s' command: " target)
				   nil
				   nil
				   nil
				   (car  compile-history)
				   '(compile-history . 1))))
    ;; Add command to history if it's different from the last command executed
    (unless (string-equal (car compile-history) (cadr compile-history))
      (build-helper--add-to-target (projectile-project-root) major-mode target command))
    ;; comint!
    (let ((default-directory (projectile-project-root)))
      (compile command nil))))

;; (build-helper-run 'run)

;; (setq build-helper--targets '())
;; (setq build-helper--targets
;;       '(("/home/afonso/git/build-helper/" . ((c-mode . ((build . ("make" "gcc foo.c"))
;; 							(run   . ("./a.out"))
;; 							(test  . ("make run"))))
;; 					     (java-mode . ((build . ("javac main.java"))))))
;; 	("project2/root/dir" . ((elisp-mode . ((test . ("elisp test command"))))))))
;; (build-helper--add-to-target (projectile-project-root) major-mode 'run "command")


(provide 'build-helper)
;;; build-helper.el ends here
