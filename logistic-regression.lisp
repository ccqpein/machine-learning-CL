(load "./package.lisp")
(load "./gradient-descent.lisp")
(in-package #:logistic-regression)

(setf *read-default-float-format* 'long-float)
(defun logistic-regression (z)
  "calculate the g(z), when g(z) larger than 0.5, return 1, else return 0"
  (let ((g 0.0l0))
    (setf g
          (+ 1.0l0 (exp (- z))))
    (if (= g 1.0)
        (return-from logistic-regression (- 1 1.0e-16))
        (return-from logistic-regression (/ 1.0l0 g)))))

(declaim (inline logistic-regression))

(defun compute-cost (X theta y)
  "calculate the cost"
  (let ((result 1)
        (rowNum (- (elt (array-dimensions X) 0) 1)))
    (setf result
          (loop for r from 0 to rowNum sum
               (+ (* (nth r y)
                     (log (logistic-regression (array-multiply (array-slice X r) theta))))
                  (* (- 1 (nth r y))
                     (log (- 1 (logistic-regression (array-multiply (array-slice X r) theta))))))))
    (print result)
    (/ (- result) (1+ rowNum))))

(defun compute-cost-2 (X theta y)
  (let ((result 0))
    (dotimes (r (elt (array-dimensions X) 0)
              (/ (- result) (elt (array-dimensions X) 0)))
      (print r)
      (setf result (+ result
                      (+ (* (nth r y)
                            (log (logistic-regression (array-multiply (array-slice X r) theta))))
                         (* (- 1 (nth r y))
                            (log (- 1 (logistic-regression (array-multiply (array-slice X r) theta)))))))))
    ))

(defun partial-derivative-lr (X theta y)
  "X is a r*c matrix"
  (let ((result)
        (colNum (1- (elt (array-dimensions X) 1)))
        (rowNum (1- (elt (array-dimensions X) 0))))
    (setf result
          (loop for c from 0 to colNum collect
               (/
                (loop for r from 0 to rowNum for temp = (array-slice X r) sum
                     (* (- (logistic-regression (array-multiply temp theta)) (elt y r))
                        (elt temp c)))
               (1+ rowNum))))
    result))


;; Sbcl dont have the fminunc function which in matlab, so I need figure out the new
;; method to fix that
;; the answer is theta = -24.9330 0.2044 0.1996 cost = 0.2035
(defun gradient-descent-lr (X theta y alpha iterTime)
  "use some functions get mini value"
  (let ((thetaRe)
        (miniV))
    (dotimes (i iterTime)
      (let* ((pd (partial-derivative-lr X theta y))
            (p (print i))
            (pp (print pd))
            (temp (compute-cost X theta y)))
        (setf miniV 
              (cond ((= i 0) (setf miniV temp))
                    ((< temp miniV) (setf miniV temp))
                    (t miniV))
              thetaRe
              (cond ((= i 0) (setf thetaRe theta))
                    ((< temp miniV) (setf thetaRe theta))
                    (t thetaRe)))
        (loop for ind from 0 to (1- (length theta)) do
             (setf (elt theta ind)
                   (- (elt theta ind) (* alpha (elt pd ind)))))
        (print thetaRe)))
    ))


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

(defvar *theta*)
(setf *theta* (make-array 3 :initial-element 0))

;(compute-cost *X* *theta* *y*)
;(partial-derivative-lr *X* *theta* *y*)
