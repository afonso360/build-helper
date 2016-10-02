;;; build-helper --- Utilities to help build code

;;; Commentary:
;; Goals:
;; Provide a recompile and compile function for each list
;; Provide multiple compile-history lists (build, run, test, etc..)
;; Optionally allow a compilation function
;;   The return value should be checked to allow multiple chains of compilation functions
;;   Check for a modified compile-history (if that exists use that on the recompile, otherwise run it again)

;;; Code:

;;;### autoload

(provide 'build-helper)
;;; build-helper.el ends here
