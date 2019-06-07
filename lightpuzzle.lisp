;;;; lightpuzzle.lisp

(in-package #:lightpuzzle)

(defun switch (x y source dx dy)
  "Toggle the switch located at (x y)."
  (loop 
    with state = (alexandria:copy-array (car source))
    for i in '((-1 . 0) (0 . -1) (0 . 0) (0 . 1) (1 . 0))
    for xx = (+ x (car i))
    and yy = (+ y (cdr i))
    do (when (and (< -1 xx dx) (< -1 yy dy))
         (setf (bit state xx yy)
               (if (zerop (bit state xx yy)) 1 0)))
    finally (return (cons state (cons (list x y) (cdr source))))))

(defun evaluate (source)
  "All 1?  Done."
  (not (some #'zerop (make-array (reduce #'* (array-dimensions (car source)))
                                 :element-type 'bit
                                 :displaced-to (car source)))))

(defun paths (source)
  (let* ((dimensions (array-dimensions (car source)))
         (dx (car dimensions))
         (dy (cadr dimensions))
         (previous (cadr source))
         (px (car previous))
         (py (cadr previous)))
    (loop
      with result
      for i from (or px 0) below dx
      do (loop
           for j from (if (eql px i) (1+ py) 0) below dy
           do (setf result (cons (list i j) result)))
      finally (return-from paths result))))

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
        do (loop
             for path in (paths head)
             for new = (switch (car path) (cadr path) head dx dy)
             do (if (evaluate new)
                    (return-from solve new)
                    (if tail
                        (progn 
                          (rplacd tail (list new))
                          (setf tail (cdr tail)))
                        (progn
                          (rplacd work (list new))
                          (setf tail (cdr work))))))
        do (progn
             (setf head (pop work))
             (unless (car head) (return-from solve nil))))))

(defun interpret (source)
  (loop with out = (make-array (array-dimensions (car source))
                               :element-type 'bit
                               :initial-element 0)
        for move in (cdr source)
        do (setf (bit out (car move) (cadr move)) 1)
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
                                    :element-type 'bit
                                    :initial-contents contents))))
    (if result
        (progn
          (princ "Okay.  Hit these spaces:")
          (terpri)
          (loop with output = (interpret result)
                for i below row
                do (loop for j below col
                         do (format t "~a " (bit output i j)))
                   (terpri))
          t)
        (progn
          (format t "There is no solution to that puzzle.  Sorry.")
          (terpri)
          nil))))

(defun make-puzzle (state)
  (loop
    with dimensions = (array-dimensions state)
    with dx = (car dimensions)
    and dy = (cadr dimensions)
    with result = (list (make-array dimensions
                                    :element-type 'bit
                                    :initial-element 1))
    for i below dx
    do (loop
         for j below dy
         do (when (= (bit state i j) 1)
              (setf result (switch i j result dx dy))))
    finally (return (car result))))
