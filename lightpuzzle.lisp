;;;; lightpuzzle.lisp

(in-package #:lightpuzzle)

(defun switch (x y source dx dy)
  "Since we use even/odd for states, just increment to toggle."
  (loop 
    with state = (alexandria:copy-array (car source))
    for i in '((-1 . 0) (0 . -1) (0 . 0) (0 . 1) (1 . 0))
    for xx = (+ x (car i))
    and yy = (+ y (cdr i))
    do (when (and (< -1 xx dx) (< -1 yy dy)) (incf (aref state xx yy)))
    finally (return (cons state (cons (list x y) (cdr source))))))

(defun evaluate (source)
  "All odd?  Done."
  (every #'oddp (make-array (reduce #'* (array-dimensions (car source)))
                            :displaced-to (car source))))

(defun solve (state
              &optional
              (work (list (list state)))
              (head (car work))
              (tail nil)
              (dimensions (array-dimensions state))
              (dx (car dimensions))
              (dy (cadr dimensions)))
  (if (evaluate head)
      head
      (loop 
        named main
        do (loop
             for i from 0 below dx
             do (loop
                  for j from 0 below dy
                  for new = (switch i j head dx dy)
                  do (if (evaluate new)
                         (return-from main new)
                         (unless (find (list i j) (cdr head) :test #'equal)
                           (if tail
                               (progn 
                                 (rplacd tail (list new))
                                 (setf tail (cdr tail)))
                               (progn
                                 (rplacd work (list new))
                                 (setf tail (cdr work))))))))
        do (setf head (pop work)))))

(defun interpret (source)
  (loop with out = (make-array (array-dimensions (car source))
                               :initial-element 0)
        for move in (cdr source)
        do (setf (aref out (car move) (cadr move)) 1)
        finally (return out)))

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
    (princ "Okay.  Hit these spaces:")
    (terpri)
    (loop with output = (interpret result)
          for i below col
          do (loop for j below row
                   do (format t "~a " (aref output i j)))
          (terpri))))
