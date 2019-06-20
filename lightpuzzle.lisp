;;;; lightpuzzle.lisp

(in-package #:lightpuzzle)

(defun check (state)
  "All 1?  Done."
  (not (some #'zerop state)))

(defun just-switch (x y state dx dy)
  "Just hit switch (x x) in state from puzzle of dimensions (dx dy)."
  (declare (optimize debug)
           (type (integer 0 100) x y dx dy))
  (loop
     with output of-type (simple-bit-vector *)
       = (alexandria:copy-array state)
     for i in '((-1 . 0) (0 . -1) (0 . 0) (0 . 1) (1 . 0))
     for xx of-type (integer -1 100) = (+ x (the (integer -1 1) (car i)))
     for yy of-type (integer -1 100) = (+ y (the (integer -1 1) (cdr i)))
     for index = (+ (* xx dy) yy)
     do (when (and (< -1 xx dx) (< -1 yy dy))
          (setf (bit output index)
                (if (zerop (bit output index)) 1 0)))
     finally (return output )))

(defun make-controls (dx dy)
  "Create array of bit vectors representing effects of switches."
  (declare (optimize debug)
           (type (integer 1 100) dx dy))
  (loop
     with d of-type (integer 1 10000) = (* dx dy)
     with output = (make-array
                    (list 2 d)
                    :element-type '(simple-bit-vector *)
                    :initial-element #*)
     for source = (make-array
                   d
                   :element-type 'bit
                   :initial-element 0)
     for i below d
     for (y x) = (multiple-value-list (floor i dx))
     do
       (setf (aref output 0 i) (just-switch x y source dx dy))
       (setf (aref source i) 1)
       (setf (aref output 1 i) source)
     finally (return output)))

(defun show-state (state dx dy)
  (loop
     for y below dy
     do (loop
           for x below dx
           do (princ (bit state (+ (* x dy) y)))
           finally (terpri))))

(defun try (states
            controls
            allowed
            &key
              (current 0)
              (remaining (- (array-dimension (car states) 0) current)))
  (declare (optimize debug))
  (cond ((and (= allowed 0) (check (car states)))
         (or (cdr states)
             (make-array (array-dimension (car states) 0)
                         :element-type 'bit
                         :initial-element 0)))
        ((= allowed 0) nil)
        ((> allowed remaining) nil)
        ((try (cons (bit-xor (car states) (aref controls 0 current))
                    (if (cdr states)
                        (bit-xor (cdr states) (aref controls 1 current))
                        (aref controls 1 current)))
              controls
              (1- allowed)
              :current (1+ current)
              :remaining (1- remaining)))
        ((try states
              controls
              allowed
              :current (1+ current)
              :remaining (1- remaining)))))

(defun solve (state controls)
  (loop
     with states = (list state)
     with results
     for i to (array-dimension state 0)
     do
       (format t "Attempting to solve with ~A switches... " i)
       (force-output)
       (setf results (try states controls i))
       (if results
           (progn (format t "SUCCESS!~%")
                  (when (= i 0)
                    (format t "It was already solved, ya cheeky bugger!~%"))
                  (force-output)
                  (return results))
           (format t "Failed.~%"))
     finally (return nil)))

(defun lightpuzzle ()
  (let* ((col (progn (princ "How many columns? ") (force-output) (read)))
         (row (progn (princ "How many rows?    ") (force-output) (read)))
         (size (* row col))
         (contents
           (progn
             (princ "Provide the state of the board.")
             (terpri)
             (princ "1 is the good state.  0 is the bad one.")
             (terpri)
             (loop repeat row collect (loop repeat col collect (read)))))
         (template (make-array (list row col)
                               :element-type 'bit
                               :initial-contents contents))
         (interface (make-array size
                                :element-type 'bit
                                :displaced-to template))
         (result (solve interface (make-controls col row))))
    (if result
        (progn
          (princ "Okay.  Hit these spaces:")
          (terpri)
          (show-state result col row)
          t)
        (progn
          (format t "There is no solution to that puzzle.  Sorry.")
          (terpri)
          nil))))

(defun make-puzzle (state dx dy)
  (loop
     with size = (* dx dy)
     with controls = (make-controls dx dy)
     with result = state
     for i below size
     if (= 1 (bit state i)) do (setf result (bit-xor result
                                                     (aref controls 0 i)))
     finally (progn (show-state result dx dy)
                    (return result))))
