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
    (proclaim '(optimize sb-cover:store-coverage-data)))

  (defslimefun swank-cover-stop ()
    (proclaim '(optimize (sb-cover:store-coverage-data 0))))

  (defslimefun swank-cover-report ()
    (let ((info (sb-cover:report-for-slime)))
      (mapcar (lambda (f)
                (destructuring-bind (source-filename expression-stats branch-stats locations) f
                  (list
                   (list
                    source-filename
                    (enough-namestring source-filename (pathname source-filename))
                    (sb-cover::ok-of expression-stats)
                    (sb-cover::all-of expression-stats)
                    (sb-cover::percent expression-stats)
                    (sb-cover::ok-of branch-stats)
                    (sb-cover::all-of branch-stats)
                    (sb-cover::percent branch-stats))
                   locations)))
              info)))

  (defslimefun swank-cover-reset ()
	(sb-cover:reset-coverage))

  )

(provide :swank-cover)
