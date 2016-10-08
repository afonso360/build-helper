;;; build-helper-test.el --- Utilities to help build code -*- lexical-binding: t -*-

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

;;; Usage:

;;; Code:
(require 'build-helper)
(require 'ert)
(require 'gv)

(ert-deftest set-get-comint-test ()
  "Test both `build-helper--set-comint' and `build-helper--get-comint'.
Should default to nil
Should be settable to nil
Should be able to set multiple times the same value, return the latest one"
  (setq build-helper--comint nil)
  (should (eq (build-helper--get-comint 'c-mode 'run) nil))
  (build-helper--set-comint 'c-mode 'run nil)
  (should (eq (build-helper--get-comint 'c-mode 'run) nil))
  (build-helper--set-comint 'c-mode 'run t)
  (should (eq (build-helper--get-comint 'c-mode 'run) t))
  (build-helper--set-comint 'c-mode 'run nil)
  (should (eq (build-helper--get-comint 'c-mode 'run) nil)))

(ert-deftest get-target-string-list-test ()
  "Test the `build-helper--get-target-string-list' function."
  (setq build-helper--targets nil)
  (should (equal (build-helper--get-target-string-list "/" 'c-mode) nil))
  (build-helper--add-command-to-target "/" 'c-mode 'run "run")
  (should (equal (build-helper--get-target-string-list "/" 'c-mode) '("run")))
  (build-helper--add-command-to-target "/" 'c-mode 'test "test")
  (should (equal (build-helper--get-target-string-list "/" 'c-mode) '("test" "run"))))

(ert-deftest add-target-project-length-test ()
  "Test `build-helper--add-command-to-target'.
The objective of the test is to test the length of the project list"
  (setq build-helper--targets nil)
  (build-helper--add-command-to-target "/" 'c-mode 'run "command")
  (build-helper--add-command-to-target "/" 'c-mode 'run "command")
  (build-helper--add-command-to-target "/" 'c-mode 'test "command")
  (build-helper--add-command-to-target "/" 'elisp-mode 'test "command")
  (should (equal (length build-helper--targets) 1))
  (build-helper--add-command-to-target "/a" 'elisp-mode 'test "command")
  (should (equal (length build-helper--targets) 2)))

(ert-deftest add-target-target-length-test ()
  "Test `build-helper--add-command-to-target'.
The objective of the test is to test the length of the target list"
  (setq build-helper--targets nil)
  (build-helper--add-command-to-target "/" 'c-mode 'run "command")
  (should (equal (length (cdr (assoc 'c-mode (assoc-string "/" build-helper--targets)))) 1))
  (build-helper--add-command-to-target "/" 'c-mode 'test "command")
  (should (equal (length (cdr (assoc 'c-mode (assoc-string "/" build-helper--targets)))) 2)))

(ert-deftest add-target-major-length-test ()
  "Test `build-helper--add-command-to-target'.
The objective of the test is to test the length of the major list"
  (setq build-helper--targets nil)
  (build-helper--add-command-to-target "/" 'c-mode 'run "command")
  (build-helper--add-command-to-target "/" 'c-mode 'run "command")
  (build-helper--add-command-to-target "/" 'c-mode 'test "command")
  (should (equal (length (cdr (assoc-string "/" build-helper--targets))) 1))
  (build-helper--add-command-to-target "/" 'elisp-mode 'test "command")
  (should (equal (length (cdr (assoc-string "/" build-helper--targets))) 2)))

(ert-deftest add-target-command-length-test ()
  "Test `build-helper--add-command-to-target'.
The objective of the test is to test the length of the command list"
  (setq build-helper--targets nil)
  (build-helper--add-command-to-target "/" 'c-mode 'run "command")
  (should (equal (length (cdr (assoc 'run (assoc 'c-mode (assoc-string "/" build-helper--targets))))) 1))
  (build-helper--add-command-to-target "/" 'c-mode 'run "command")
  (should (equal (length (cdr (assoc 'run (assoc 'c-mode (assoc-string "/" build-helper--targets))))) 2)))

(ert-deftest add-get-target-test ()
  "Test both `build-helper--get-target' and `build-helper--add-command-to-target'."
  (setq build-helper--targets nil)
  (should (eq (build-helper--get-target "/" 'c-mode 'run) nil))
  (build-helper--add-command-to-target "/" 'c-mode 'run "command")
  (should (equal (build-helper--get-target "/" 'c-mode 'run) '("command")))
  (build-helper--add-command-to-target "/" 'c-mode 'run "command2")
  (should (equal (build-helper--get-target "/" 'c-mode 'run) '("command2" "command")))
  (build-helper--add-command-to-target "/" 'elisp-mode 'run "elisp")
  (should-not (equal (build-helper--get-target "/" 'elisp-mode 'run) '("command2" "command")))
  (should (equal (build-helper--get-target "/" 'elisp-mode 'run) '("elisp"))))


(ert-deftest add-target-function ()
  "Test `build-helper-add-function'."
  (setq build-helper--functions nil)
  (build-helper-add-function 'c-mode 'run #'car)
  (should (equal build-helper--functions '((c-mode (run car)))))
  (build-helper-add-function 'c-mode 'run #'cdr)
  (should (equal build-helper--functions '((c-mode (run cdr car)))))
  (build-helper-add-function 'c-mode 'test #'cdr)
  (should (equal build-helper--functions '((c-mode (test cdr) (run cdr car)))))
  (build-helper-add-function 'elisp-mode 'test #'cdr)
  (should (equal build-helper--functions
		 '((elisp-mode (test cdr)) (c-mode (test cdr) (run cdr car))))))

(ert-deftest run-all-functions ()
  "Test `build-helper--run-all-functions'."
  (setq build-helper--functions nil)
  (let* ((a 0)
	 (p (gv-ref a)))
    (build-helper-add-function ;; should run first every time
     'c-mode
     'run
     #'(lambda () (setf (gv-deref p) (+ 1 (gv-deref p))) nil))

    (should (equal (build-helper--run-all-functions 'c-mode 'run) nil))
    (should (equal a 1))

    (build-helper-add-function ;; should run second every time
     'c-mode
     'run
     #'(lambda () (setf (gv-deref p) (+ 2 (gv-deref p))) nil))

    (should (equal (build-helper--run-all-functions 'c-mode 'run) nil))
    (should (equal a 4))

    (build-helper-add-function ;; should run last every time, no further functions executed
     'c-mode
     'run
     #'(lambda () (setf (gv-deref p) (+ 4 (gv-deref p))) t))

    (should (equal (build-helper--run-all-functions 'c-mode 'run) t))
    (should (equal a 11))
    (should (equal (build-helper--run-all-functions 'c-mode 'run) t))
    (should (equal a 18))

    ;; never should run, this catches issues such as
    ;; execution in the reverse order
    (build-helper-add-function
     'c-mode
     'run
     #'(lambda () (setf (gv-deref p) (+ 100 (gv-deref p))) nil))

    (should (equal (build-helper--run-all-functions 'c-mode 'run) t))
    (should (equal a 25))))

(ert-deftest file-save-load-tests ()
  "Test `build-helper--save-targets' and `build-helper--load-targets' functions."
  (setq build-helper-file "./test-build-helper-file.el")
  (setq build-helper-sample-target '("/example/dir" (c-mode (run ("stuff")))))
  (setq build-helper--targets build-helper-sample-target)
  (build-helper--save-targets)
  (setq build-helper--targets nil)
  (build-helper--load-targets)
  (should (equal build-helper--targets build-helper-sample-target))
  (when (file-exists-p build-helper-file)
    (delete-file build-helper-file)))

(provide 'build-helper-test)
;;; build-helper-test.el ends here
