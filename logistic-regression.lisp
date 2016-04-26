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

(defun partial-derivative-lr (X theta y)
  "X is a r*c matrix"
  (let ((result)
        (colNum (1- (elt (array-dimensions X) 1)))
        (rowNum (1- (elt (array-dimensions X) 0))))
    (setf result
          (loop for c from 0 to colNum collect
                (loop for r from 0 to rowNum for temp = (array-slice X r) sum
                     (* (- (logistic-regression (array-multiply temp theta)) (elt y r))
                        (elt temp c)))))
    result))

(defun gradient-descent-lr (X theta y alpha iterTime)
  (dotimes (i iterTime theta)
    (let ((pd (partial-derivative-lr X theta y)))
      (loop for ind from 0 to (1- (length theta)) do
           (setf (elt theta ind)
                 (- (elt theta ind) (* alpha (elt pd ind)))))
      (print theta))))

;;; exercise below

(defvar *X*)
(setf *X*
      (let ((temp
             (loop for ar in (read-data "./testdata/ex2data1.txt") collect
                  (list 1 (elt ar 0) (elt ar 1)))))
        (make-array (list (length temp) (length (car temp))) :initial-contents temp)))

(defvar *y*)
(setf *y*
      (loop for ar in (read-data "./testdata/ex2data1.txt") collect
           (elt ar 2)))


