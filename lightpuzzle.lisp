;;;; lightpuzzle.lisp

(in-package #:lightpuzzle)

(defun flow (states dx dy controls)
  (loop with barrier = (* (1- dy) dx)
     for i from 0 below (* dx dy)
     do (cond ((not (zerop (bit (car states) i))))
              ((< i barrier)
               (setf states (cons (bit-xor
                                   (car states)
                                   (aref controls 0 (+ i dx)))
                                  (bit-xor
                                   (cdr states)
                                   (aref controls 1 (+ i dx))))))
              (t (return nil)))
     finally (let* ((solution (cdr states))
                    (steps (count 1 solution)))
               (format t
                       "Located solution that requires ~A moves:~%"
                       steps)
               (show-state solution dx dy)
               (terpri)
               (return (cons solution steps)))))

(defun flow-solve (states dx dy controls &optional (current 0))
  (if (= current dx)
      (flow states dx dy controls)
      (let ((zero (flow-solve states dx dy controls (1+ current)))
            (one (flow-solve (cons (bit-xor (car states)
                                            (aref controls 0 current))
                                   (bit-xor (cdr states)
                                            (aref controls 1 current)))
                             dx
                             dy
                             controls
                             (1+ current))))
        (cond ((null zero) one)
              ((null one) zero)
              ((< (cdr zero) (cdr one)) zero)
              (t one)))))

(defun just-switch (x y state dx dy)
  "Just hit switch (x y) in state from puzzle of dimensions (dx dy)."
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
     finally (return output)))

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
     for interface = (make-array (list dx dy)
                                 :element-type 'bit
                                 :displaced-to source)
     for i below d
     for (y x) = (multiple-value-list (floor i dx))
     do
       (setf (aref output 0 i) (just-switch y x source dy dx))
       (setf (aref source i) 1)
       (setf (aref output 1 i) source)
     finally (return output)))

(defun show-state (state dx dy)
  (loop
     for y below dy
     do (loop
           for x below dx
           do (princ (bit state (+ (* y dx) x)))
           finally (terpri))))

(defun lightpuzzle ()
  (declare (optimize debug))
  (let* ((col (progn (princ "How many columns? ") (force-output) (read)))
         (row (progn (princ "How many rows?    ") (force-output) (read)))
         (size (* row col))
         (contents
           (progn
             (princ "Provide the state of the board.")
             (terpri)
             (princ "1 is the good state.  0 is the bad one.")
             (terpri)
             (loop repeat size collect (read))))
         (state (make-array size
                            :element-type 'bit
                            :initial-contents contents))
         (result (flow-solve (cons state
                                   (make-array size
                                               :element-type 'bit
                                               :initial-element 0))
                             col
                             row
                             (make-controls col row))))
    (if result
        (progn
          (princ "I would recommend hitting the following spaces:")
          (terpri)
          (show-state (car result) col row)
          t)
        (progn
          (format t "There is no solution to that puzzle.  Sorry.")
          (terpri)
          nil))))

(defun make-puzzle (state dx dy)
  (loop
     with size = (* dx dy)
     with controls = (make-controls dx dy)
     with result = (make-array size :element-type 'bit :initial-element 0)
     for i below size
     if (= 1 (bit state i)) do (setf result (bit-xor result
                                                     (aref controls 0 i)))
     finally (progn (show-state result dx dy)
                    (return result))))
