(load "./package.lisp")
(load "./gradient-descent.lisp")
(in-package #:logistic-regression)

(defun logistic-regression (z)
  "calculate the g(z), when g(z) larger than 0.5, return 1, else return 0"
  ;(print z)
  ;(print "here")
  (let ((g))
    (setf g
          (1+ (exp (- z))))
    ;(print g)
    (/ 1 g)))

(declaim (inline logistic-regression))

(defun compute-cost (X theta y)
  "calculate the cost"
  (let ((result 0)
        (rowNum (1- (elt (array-dimensions X) 0))))
    (loop for r from 0 to rowNum for temp = (array-slice X r) do
         (progn
           (print (log (logistic-regression (array-multiply temp theta))))
         (setf result
               (+ result
                  (+ (* (nth r y)
                        (log (logistic-regression (array-multiply temp theta))))
                     (* (- 1 (nth r y))
                        (log (- 1 (logistic-regression (array-multiply temp theta))))))))))
    (/ result (- rowNum))))

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

(defun gradient-descent-lr (X theta y alpha iterTime)
  "use some functions get mini value"
  (let ((thetaRe)
        (miniV))
    (dotimes (i iterTime thetaRe)
      (let ((pd (partial-derivative-lr X theta y))
            (pp "ii")
            #|(pppp (print X))
            (ppppp (print theta))
            (pppppp (print y))|#
            (temp (compute-cost X theta y)))
        (print i) (print temp) (print pp)
        (setf miniV 
              (cond ((= i 0) (setf miniV temp))
                    ((< temp miniV) (setf miniV temp))
                    (t miniV))
              thetaRe
              (cond ((= i 0) (setf thetaRe theta))
                    ((< temp miniV) (setf thetaRe theta))
                    (t thetaRe)))
        (print miniV)
        (loop for ind from 0 to (1- (length theta)) do
             (setf (elt theta ind)
                   (- (elt theta ind) (* alpha (elt pd ind)))))))
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

(compute-cost *X* *theta* *y*)
(partial-derivative-lr *X* *theta* *y*)
