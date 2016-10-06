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
;; Per target comint setting defaults to nil
;;
;; Optionally allow compilation functions
;;   Compilation functions are based on the major-mode from which they are invoked
;;   The return value should be checked to allow multiple chains of compilation functions
;;   Maybe: Check for a modified compile-history (if that exists use that on the recompile, otherwise run it again)


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

(defun build-helper--get-target (project major target)
  "Get `compile-history' list for PROJECT for MAJOR mode and TARGET.
If any of those is not found return nil."
  (alist-get target
	     (alist-get major
			(alist-get project build-helper--targets nil) nil) nil))

(defun build-helper--get-target-string-list (project major)
  "Return a list of string targets for PROJECT and MAJOR mode."
  (mapcar #'car
	  (alist-get major
			   (alist-get project build-helper--targets nil) nil)))

(defun build-helper--add-command-to-target (project major target command)
  "Add COMMAND entry to PROJECT, MAJOR mode and TARGET list.
If any of PROJECT, MAJOR or TARGET are not found, create empty"
  (push command
	(alist-get target
		   (alist-get major
			      (alist-get project build-helper--targets nil)
			      nil)
		   nil)))

;;;###autoload
(defun build-helper-re-run (target)
  "Run the last command executed on a TARGET."
  (interactive
    (list (completing-read "Target: "
		    (build-helper--get-target-string-list
		     (projectile-project-root)
		     major-mode))))
  (when (stringp target)
    (setq target (intern target)))
  (let* ((compile-history (build-helper--get-target (projectile-project-root)
						    major-mode
						    target)))
    (let ((default-directory (projectile-project-root)))
      (compile (car compile-history) t))))

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
		     major-mode))))
  (when (stringp target)
    (setq target (intern target)))
  (let* ((compile-history (build-helper--get-target (projectile-project-root)
						    major-mode
						    target))
	 (command (completing-read (format "'%s' command: " target)
				   nil
				   nil
				   nil
				   (car  compile-history)
				   '(compile-history . 1))))
    (unless (string-equal (car compile-history) (cadr compile-history))
      (build-helper--add-command-to-target (projectile-project-root) major-mode target command))
    (let ((default-directory (projectile-project-root)))
      (compile command t))))

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
