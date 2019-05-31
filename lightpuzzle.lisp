;;;; lightpuzzle.lisp

(in-package #:lightpuzzle)

#||
;;; Whoops.  Complicating the problem a bit...
(defun fix (index max)
  "Fix array indicies so they don't over- or under-flow.
   Implements wrap-arond to fix off-by-one.  Fails with larger changes."
  (cond
    ((<= 0 index max) index)
    ((< index 0) max)
    (t 0)))

(defun switch (x y source)
  "Since we use even/odd for states, just increment to toggle."
  (loop 
    with state = (alexandria:copy-array (cdr source))
    with dimensions = (array-dimensions state)
    with dx = (1- (car dimensions)) and dy = (1- (cadr dimensions))
    for i in '((-1 . 0) (0 . -1) (0 . 0) (0 . 1) (1 . 0))
    for xx = (fix (+ x (car i)) dx)
    and yy = (fix (+ y (cdr i)) dy)
    do (incf (aref state xx yy))
    finally (return (cons (cons (cons x y) (car source)) state))))
||#

(defun switch (x y source dx dy)
  "Since we use even/odd for states, just increment to toggle."
  (loop 
    with state = (alexandria:copy-array (car source))
;    with dimensions = (array-dimensions state)
;    with dx = (car dimensions) and dy = (cadr dimensions)
    for i in '((-1 . 0) (0 . -1) (0 . 0) (0 . 1) (1 . 0))
    for xx = (+ x (car i))
    and yy = (+ y (cdr i))
    do (when (and (< -1 xx dx) (< -1 yy dy)) (incf (aref state xx yy)))
    finally (return (cons state (cons (list x y) (cdr source))))))

(defun evaluate (source)
  "All odd?  Done."
  ;; No need to be sbcl-spcific here, I suppose...
  ; (every #'oddp (sb-ext:array-storage-vector (car source)))
  (every #'oddp (make-array (reduce #'* (array-dimensions (car source)))
                            :displaced-to (car source))))

(defun solve (state
               &optional
               (work (list (list state)))
               (head (car work))
               (tail work)
               (dimensions (array-dimensions state))
               (dx (car dimensions))
               (dy (cadr dimensions)))
  (loop
     while (not (evaluate head))
     do (loop
           for i from 0 below dx
           do (loop
                 for j from 0 below dy
                 for new = (switch i j head dx dy)
                 do (rplacd tail (cons new nil))
                 do (setf tail (cdr tail))))
     do (setf head (pop work))
     finally (return head)))

(defun lightpuzzle ()
  (let* ((col (progn (princ "How many columns? ") (force-output) (read)))
         (row (progn (princ "How many rows?    ") (force-output) (read)))
         (contents
           (progn
             (princ "Provide the state of the board.")
             (terpri)
             (princ "1 is the good state.  0 is the bad one.")
             (terpri)
             (loop repeat row collect (loop repeat col collect (read)))))
         (result (solve (make-array (list row col)
                                    :initial-contents contents))))
    (princ "Okay.  Hit the following spaces: ")
    (format t "~{~{(~*~A, ~2:*~A~*)~}~#[.~:;, ~]~}~%" (cdr result))))
