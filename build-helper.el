;;; build-helper --- Utilities to help build code

;;; Commentary:
;; Goals:
;; Provide multiple compile-history lists (build, run, test, etc..)
;; Provide a recompile and compile function for each list
;; Per project lists
;;   build-helper will switch to the project root dir before running any of the commands
;;   Require projectile and use that
;;   Have a .build-helper-list.el that maintains the list of these projects and their lists
;;     .build-helper-list.el should be roughly like the following
;;     '(("project/root/dir" . (('c-mode . (('build . "make" "gcc foo.c")
;;     				          ('run   . "./a.out")
;;     				          ('test  . "make run")))
;;                              ('java-mode . (('build . "javac main.java")))))
;;
;;       ("project2/root/dir" . (('elisp-mode . (('test . "elisp test command"))))))
;;
;;     From the example above, java-mode would never have run or test commands
;;     because those would be elisp functions.  (this is just an example)
;;
;; Optionally allow compilation functions
;;   Compilation functions are based on the major-mode from which they are invoked
;;   The return value should be checked to allow multiple chains of compilation functions
;;   Check for a modified compile-history (if that exists use that on the recompile, otherwise run it again)

;;; Code:

;;;### autoload

(provide 'build-helper)
;;; build-helper.el ends here
