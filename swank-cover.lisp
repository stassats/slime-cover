;;; swank-cover.lisp

;;; To use:
;;; - configure slime to load the desired version of slime-cover.el in ~/loc/slime
;;; - start sbcl
;;; - compile cover.lisp into the running image
;;; - compile the desired test file(s)
;;; - switch to the REPL
;;; - select the desired cover function from the menu

(in-package #:swank)

#+sbcl
(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :sb-cover))

(defvar *coverage* nil)

#+sbcl
(progn

  (defslimefun swank-cover-start ()
	nil)

  (defslimefun swank-cover-stop ()
	nil)

  (defslimefun swank-cover-report ()
	(multiple-value-bind (report-file file-info) (sb-cover:report "/tmp/sb-cover-report/" :suppress-html-p t)
	  (declare (ignore report-file))
	  (mapcar (lambda (f)
                (destructuring-bind (source-filename report-filename expression-stats branch-stats locations) f
                    (declare (ignore report-filename))
                    (list
                     (enough-namestring (pathname source-filename) (pathname source-filename))
                     source-filename
                     (list (sb-cover::ok-of expression-stats)
                           (sb-cover::all-of expression-stats)
                           (sb-cover::percent expression-stats))
                     (list (sb-cover::ok-of branch-stats)
                           (sb-cover::all-of branch-stats)
                           (sb-cover::percent branch-stats))
                     locations)))
              file-info)))

  (defslimefun swank-cover-reset ()
	(sb-cover:reset-coverage))

  )

(provide :swank-cover)
