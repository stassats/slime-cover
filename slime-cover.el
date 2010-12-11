(define-slime-contrib slime-cover
  "Integration with SBCL's sb-cover."
  (:authors "Jonathon McKitrick")
  (:license "MIT")
  (:swank-dependencies swank-cover)
  (:on-load
   (let ((C '(and (slime-connected-p)
                  (equal (slime-lisp-implementation-type) "SBCL"))))
     ;; This does not work correctly, and is probably in the wrong menu location.
     (setf (cdr (last (assoc "Profiling" slime-easy-menu)))
           `("--"
             [ "Start cover"  slime-cover-start ,C ]
             [ "Stop cover"   slime-cover-stop ,C ]
             [ "Reset cover"  slime-cover-reset ,C ]
             [ "Report index" slime-cover-report ,C ])))))

(defvar slime-cover-file-map (make-hash-table :test 'equal)
  "Map of files with coverage data.")
(make-variable-buffer-local 'slime-cover-file-map)

(defface slime-cover-face-executed
  '((t (:background "#aaffaa")))
  "Face for showing code coverage."
  :group 'slime-mode-faces)

(defface slime-cover-face-conditionalized-out
  '((t (:background "#eeeeee" :foreground "#aaaaaa")))
  "Face for showing code coverage."
  :group 'slime-mode-faces)

(defface slime-cover-face-not-executed
  '((t (:background "#ffaaaa")))
  "Face for showing code coverage."
  :group 'slime-mode-faces)

(defface slime-cover-face-not-instrumented
  '((t (:background "#eeeeee")))
  "Face for showing code coverage."
  :group 'slime-mode-faces)

(defface slime-cover-face-both-branches-taken
  '((t (:background "#44dd44")))
  "Face for showing code coverage."
  :group 'slime-mode-faces)

(defface slime-cover-face-one-branch-taken
    '((t (:background "#ffffaa")))
  "Face for showing code coverage."
  :group 'slime-mode-faces)

(defface slime-cover-face-neither-branch-taken
  '((t (:background "#ee6666")))
  "Face for showing code coverage."
  :group 'slime-mode-faces)

(defface slime-cover-face
  '((t (:background "lightgray")))
  "Face for showing code coverage."
  :group 'slime-mode-faces)

(define-minor-mode slime-cover-mode
    "Minor mode to highlight code coverage."
  nil nil
  '(("v" . slime-cover-view-file))
  (setq buffer-undo-list t))

(define-derived-mode slime-cover-index-mode fundamental-mode
  "slcover"
  "Mode for browsing coverage data"
  :syntax-table text-mode-syntax-table
  (setq buffer-read-only t
        buffer-undo-list t))

(defun slime-cover-next-file ()
  "Move to the next file in cover list."
  (interactive)
  (forward-line))

(defun slime-cover-previous-file ()
  "Move to the previous file in cover list."
  (interactive)
  (forward-line -1))

(defun slime-cover-view-file ()
  (interactive)
  (when buffer-file-name
    (let ((position (point)))
      (with-current-buffer (find-file-other-window buffer-file-name)
        (goto-char position)))))

;;(set-keymap-parent slime-cover-index-mode-map slime-parent-map)

(slime-define-keys slime-cover-index-mode-map
  ("n" 'slime-cover-next-file)
  ("p" 'slime-cover-previous-file)
  ("g" 'slime-cover-update-index)
  ((kbd "RET") (lambda () (interactive)
                 (slime-cover-format-file (gethash (thing-at-point 'filename) slime-cover-file-map)))))

(defun slime-cover-start ()
  (interactive)
  (slime-eval `(swank:swank-cover-start)))

(defun slime-cover-stop ()
  (interactive)
  (slime-eval `(swank:swank-cover-stop)))

(defun slime-cover-reset ()
  (interactive)
  (slime-eval `(swank:swank-cover-reset)))

(defun slime-cover-report ()
  "Show coverage report in a buffer."
  (interactive)
  (slime-with-popup-buffer ("*slime-cover-index*"
                            :connection t
                            :select t
                            :mode 'slime-cover-index-mode)
    (slime-cover-update-index)))

(defun slime-cover-update-index ()
  "Get coverage report and file list."
  (interactive)
  (slime-eval-async `(swank:swank-cover-report)
    'slime-cover-format-index))

(defun slime-cover-format-index (lines)
  "Show the list of coverage files and stats."
  (let ((inhibit-read-only t))
	(erase-buffer)
	;;(insert (format "stuff: %s\n" slime-cover-file-map))
	(insert (format "Cover Report:\n"))
    (insert (format "\t\t\t\t\t\tExpression\t\t\tBranch\n"))
    (insert (format "\tSource File\t"))
    (insert (format "\tCovered\tTotal\t%%"))
    (insert (format "\tCovered\tTotal\t%%"))
    (dolist (line lines)
      (destructuring-bind (short-path long-path expression-stats branch-stats locations) line
        (puthash short-path (list long-path locations) slime-cover-file-map)
        (insert (format "\n%s" short-path))
        (insert (format "\t\t%s\t\t%s\t%s" (first expression-stats) (second expression-stats) (third expression-stats)))
        (insert (format "\t%s\t%s\t%s" (first branch-stats) (second branch-stats) (third branch-stats)))))
    (beginning-of-buffer)
    ;;(goto-char (point-min))
    ;;(forward-char 10)
    (forward-line 3)
    ))

(defun slime-cover-format-file (info)
  "Display swank coverage info LINES into coverage mode buffer."
  (destructuring-bind (filename lines) info
    (when filename
      (slime-with-popup-buffer ("*slime-cover-file*"
                                :select t
                                :mode 'slime-cover-mode)
        (setq buffer-file-name filename)
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert-file-contents filename)
          (slime-cover-test-face lines))))))

(defun slime-cover-test-face (locations)
  "Mark source code with LOCATIONS in coverage buffer."
  (loop for (start end state) in locations
        do (slime-cover-mark-text-state (1+ start) (1+ end) state))
  (goto-char (point-min)))

(defun slime-cover-mark-text-state (start end state)
  (goto-char start)
  (loop with face =  (slime-cover-get-face-for-state state)
        for line-end = (min (line-end-position) end)
        for overlay = (make-overlay (point) line-end)
        do
        (overlay-put overlay 'face face)
        (overlay-put overlay 'priority state)
        (goto-char (1+ line-end))
        until (or (= line-end end)
                  (= (point) (point-max)))))

(defvar slime-cover-state-faces
  (let ((vector (make-vector 16 nil))
        (list '((0 slime-cover-face-not-instrumented)
                (1 slime-cover-face-executed)
                (2 slime-cover-face-not-executed)
                (5 slime-cover-face-both-branches-taken)
                (6 slime-cover-face-one-branch-taken)
                (9 slime-cover-face-one-branch-taken)
                (10 slime-cover-face-neither-branch-taken)
                (15 slime-cover-face-conditionalized-out))))
    (loop for (index face) in list
          do (setf (elt vector index) face))
    vector))

(defun slime-cover-get-face-for-state (state)
  (interactive)
  (or (and (< state (length slime-cover-state-faces))
           (elt slime-cover-state-faces state))
      slime-cover-face))

(provide 'slime-cover)
