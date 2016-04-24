(load "./package.lisp")
(load "./gradient-descent.lisp")
(in-package #:logistic-regression)

(defun logistic-regression (z)
  "calculate the g(z), when g(z) larger than 0.5, return 1, else return 0"
  (let ((g (/ 1 (+ 1 (exp (* -1 z))))))
    g))

(declaim (inline logistic-regression))

(defun compute-cost (X theta y)
  "calculate the cost"
  (let ((result)
        (rowNum (1- (elt (array-dimensions X) 0))))
    (setf result
          (loop for r from 0 to rowNum for temp = (array-slice X r) sum
               (if (= (nth r y) 1)
                   (- (log (logistic-regression (array-multiply temp theta)) 10))
                   (- (log (- 1 (logistic-regression (array-multiply temp theta))) 10)))))
    (/ result rowNum)))
