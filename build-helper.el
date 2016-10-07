;;; build-helper.el --- Utilities to help build code -*- lexical-binding: t -*-

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
;; Optionally allow compilation functions
;;   Compilation functions are based on the major-mode from which they are invoked
;;   The return value should be checked to allow multiple
;;chains of compilation functions
;; Todo:
;; build-helper--targets gets a duplicate entry every time we add a command
;; can't run commands with spaces

;; Some code based on company-statistics.el (https://github.com/company-mode/company-statistics)
;;; Usage:

;;; Code:
(require 'projectile)

(defgroup build-helper nil
  "Helper functions to build files."
  :group 'build-helper)

(defcustom build-helper-file
  (concat user-emacs-directory ".build-helper-targets.el")
  "File to save build-helper command history."
  :type 'string)

(defvar build-helper--functions '()
  "Build helper functions.")

(defvar build-helper--comint '()
  "Build helper comint state.")

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

(defun build-helper--get-comint (major target)
  "Get the comint value for TARGET in MAJOR mode or nil.
Unlike with targets these values are not saved"
  (car (alist-get target (alist-get major build-helper--comint  nil) nil)))

(defun build-helper--set-comint (major target value)
  "Set comint VALUE for TARGET in MAJOR mode.
By default the value is nil."
  (push value
	(alist-get target
		   (alist-get major build-helper--comint  nil) nil)))

(defun build-helper--get-target (project major target)
  "Get `compile-history' list for PROJECT for MAJOR mode and TARGET.
If any of those is not found return nil."
  (let ((nplist (assoc-string project build-helper--targets)))
    (when nplist
      (alist-get target (alist-get major (cdr nplist) nil) nil))))

(defun build-helper--get-target-string-list (project major)
  "Return a list of string targets for PROJECT and MAJOR mode."
  (mapcar
   #'(lambda (a)
       (symbol-name (car a)))
   (cdr (assoc major (cdr (assoc project build-helper--targets))))))

(defun build-helper--add-command-to-target (project major target command)
  "Add COMMAND entry to PROJECT, MAJOR mode and TARGET list.
If any of PROJECT, MAJOR or TARGET are not found, create empty"
  (let ((nplist (assoc-string project build-helper--targets)))
    (unless nplist
      (setq nplist (cons project '())))
    (push command (alist-get target (alist-get major (cdr nplist) nil) nil))
    (push nplist build-helper--targets)))

(defun build-helper--run-all-functions (major target)
    "Run all functions associated with a TARGET and MAJOR mode

Functions will be executed in the order that they were registered in.

Should any function return t halt the execution of the following functions
otherwise keep executing.

If the last function returns nil, or if there is no functions to be executed
return nil, otherwise return t"
    (let ((funlist (alist-get target
			      (alist-get major build-helper--functions nil) nil)))
      (when funlist
	(let (value)
	  (dolist (fun (reverse funlist) value)
	    (unless value
	      (setq value (funcall fun))))))))

(defun build-helper-add-function (major target function)
  "Add a FUNCTION to be executed when TARGET is run in MAJOR mode.

Functions are guaranteed to be executed in the order of registration.
If a function returns t no other functions will be executed.
Should the last function return nil, a compilation command will be asked."
  (push function
	(alist-get target
		   (alist-get major build-helper--functions nil) nil)))

;;;###autoload
(defun build-helper-setup ()
  "Setup build-helper."
  (build-helper--load-targets)
  (add-hook 'kill-emacs-hook 'build-helper--save-targets))

;;;###autoload
(defun build-helper-re-run (target)
  "Run the last command or functions associated with a TARGET."
  (interactive
   (list (completing-read "Target: "
			  (build-helper--get-target-string-list
			   (projectile-project-root)
			   major-mode))))
  (when (stringp target)
    (setq target (intern target)))
  (let* ((compile-history (build-helper--get-target (projectile-project-root)
						    major-mode
						    target))
	 (comint (build-helper--get-comint major-mode target)))
    (let ((default-directory (projectile-project-root)))
      (compile (car compile-history) comint))))

;;;###autoload
(defun build-helper-run (target)
  "Run a TARGET.
This includes functions associated with the current `major-mode'.
If none of those work, a `compile' prompt with a target and `major-mode' based history.
This compile command will be executed from the projectile root directory."
  (interactive
   (list (completing-read "Target: "
			  (build-helper--get-target-string-list
			   (projectile-project-root)
			   major-mode)
			  nil)))
  (when (stringp target)
    (setq target (intern target)))
  (let* ((compile-history (build-helper--get-target (projectile-project-root)
						    major-mode
						    target))
	 (comint (build-helper--get-comint major-mode target))
	 (command (completing-read (format "'%s' command: " target)
				   nil
				   nil
				   nil
				   (car  compile-history)
				   '(compile-history . 1))))

    (unless (string-equal (car compile-history) (cadr compile-history))
      (build-helper--add-command-to-target (projectile-project-root)
					   major-mode
					   target command))

    (let ((default-directory (projectile-project-root)))
      (compile command comint))))

;;;###autoload
(defun build-helper-re-run-test ()
  "Run `build-helper-re-run' with target test."
  (interactive)
  (build-helper-re-run 'test))

;;;###autoload
(defun build-helper-re-run-build ()
  "Run `build-helper-re-run' with target build."
  (interactive)
  (build-helper-re-run 'build))

;;;###autoload
(defun build-helper-re-run-run ()
  "Run `build-helper-re-run' with target run."
  (interactive)
  (build-helper-re-run 'run))

;;;###autoload
(defun build-helper-run-test ()
  "Run `build-helper-run' with target test."
  (interactive)
  (build-helper-run 'test))

;;;###autoload
(defun build-helper-run-build ()
  "Run `build-helper-run' with target build."
  (interactive)
  (build-helper-run 'build))

;;;###autoload
(defun build-helper-run-run ()
  "Run `build-helper-run' with target run."
  (interactive)
  (build-helper-run 'run))

(provide 'build-helper)
;;; build-helper.el ends here
