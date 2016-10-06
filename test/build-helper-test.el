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

(ert-deftest set-get-comint-test ()
  "Test both `build-helper--set-comint' and `build-helper--get-comint'.
Should default to nil
Should be settable to nil
Should be able to set multiple times the same value, return the latest one"
  (should (eq (build-helper--get-comint 'c-mode 'run) nil))
  (build-helper--set-comint 'c-mode 'run nil)
  (should (eq (build-helper--get-comint 'c-mode 'run) nil))
  (build-helper--set-comint 'c-mode 'run t)
  (should (eq (build-helper--get-comint 'c-mode 'run) t))
  (build-helper--set-comint 'c-mode 'run nil)
  (should (eq (build-helper--get-comint 'c-mode 'run) nil)))



(provide 'build-helper-test)
;;; build-helper-test.el ends here
