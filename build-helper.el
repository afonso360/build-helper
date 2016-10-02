;;; build-helper --- Utilities to help build code

;; Copyright (C) 2016 Afonso Bordado

;; Author:  Afonso Bordado <afonsobordado@az8.co>
;; Version: 0.1
;; URL: http://github.com/afonso360/build-helper

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
;; Provide multiple compile-history lists (build, run, test, etc..)
;; Provide a recompile and compile function for each list
;; Per project lists
;;   build-helper will switch to the project root dir before running any of the commands
;;   Require projectile and use that
;;   Have a .build-helper-lists.el that maintains the list of these projects and their lists
;;     .build-helper-list.el should be roughly like the following
;;     '(("project/root/dir" . ((c-mode . ((build . '("make" "gcc foo.c"))
;;     	                                   (run   . '("./a.out"))
;;     	                                   (test  . '("make run"))))
;;     			        (java-mode . ((build . '("javac main.java"))))))
;;
;;       ("project2/root/dir" . ((elisp-mode . ((test . '("elisp test command"))))))))
;;
;;
;;     From the example above, java-mode would never have run or test commands
;;     because those would be elisp functions.  (this is just an example)
;;
;; Optionally allow compilation functions
;;   Compilation functions are based on the major-mode from which they are invoked
;;   The return value should be checked to allow multiple chains of compilation functions
;;   Check for a modified compile-history (if that exists use that on the recompile, otherwise run it again)


;; Some code based on company-statistics.el (https://github.com/company-mode/company-statistics)
;;; Usage:

;;; Code:
(defgroup build-helper nil
  "Helper functions to build files."
  :group 'build-helper)

(defcustom build-helper-file
  (concat user-emacs-directory ".build-helper-lists.el")
  "File to save build-helper command history."
  :type 'string)

(defcustom build-helper-capture-function nil
  "Whether we should capture compile commands called by external functions."
  :type 'boolean)

(defvar build-helper--lists '()
  "Build helper state lists.")

(defun build-helper--save-lists ()
  "Save the lists to the build-helper-file."
  (with-temp-buffer
    (insert (format "(setq build-helper--lists %s)" build-helper--lists))
    (write-file build-helper-file)))

(defun build-helper--load-lists ()
  "Load the lists from build-helper-file."
  (load build-helper-file 'noerror nil 'nosuffix))

(defun build-helper-setup ()
  "Setup build-helper."
  (build-helper--load-lists)
  (add-hook 'kill-emacs-hook 'build-helper--save-lists))

(provide 'build-helper)
;;; build-helper.el ends here
