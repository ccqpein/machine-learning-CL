(load "./package.lisp")
(load "./gradient-descent.lisp")
(in-package #:logistic-regression)

(setf *random-state* (make-random-state t))

(defun logistic-regression (z)
  "calculate the g(z), when g(z) larger than 0.5, return 1, else return 0"
  (let ((g 0.0d0))
    (setf g
          (+ 1.0l0 (exp (- z))))
    (if (= g 1.0)
        (return-from logistic-regression (- 1.0d0 1.0e-5))
        (return-from logistic-regression (/ 1.0d0 g)))))

(declaim (inline logistic-regression))

(defun compute-cost (X theta y)
  "calculate the cost"
  (let ((result 1L0)
        (rowNum (- (elt (array-dimensions X) 0) 1)))
    (setf result
          (loop for r from 0 to rowNum sum
               (+ (* (nth r y)
                     (log (logistic-regression (array-multiply (array-slice X r)
                                                               theta))))
                  (* (- 1 (nth r y))
                     (log (- 1 (logistic-regression (array-multiply (array-slice X r)
                                                                    theta))))))))
                                        ;(print result)
    (setf result (/ (- result) (1+ rowNum)))
    result)
  )
  
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
;; method to fix that may 3 2016 fixed
;; the answer is theta = -24.9330 0.2044 0.1996 cost = 0.2035

(defun partial-derivative-ge (func arglist &key (which nil) (d (expt 2 -4)))
  "to calculate the partial-derivative. argspoint is which args need to caculate partial derivative. Use method: if function args are '(1 2), do not need which; if your function need '(#(1 2 3) #(1 3 3)), you need the which to point which arg you want to calculate partial derivative."
  (let* ((arglist (loop for i in arglist collect
                       (eval i)))
         (args (if (eql which nil)
                   arglist
                   (nth (1- which) arglist)))
         (argslen (length args)))
    (loop for i from 0 to (1- argslen) collect
         (let* ((result)
                (argsTemp (copy-seq args))
                (num (elt argsTemp i))
                (Temp1 (if (eql which nil)
                           (progn (setf (elt argsTemp i) (+ num d))
                                  (copy-seq argsTemp))
                           (progn (setf (elt argsTemp i) (+ num d))
                                  (let* ((newargs (copy-seq arglist)))
                                    (setf (nth (1- which) newargs)
                                          (copy-seq argsTemp))
                                    newargs))))
                (Temp2 (if (eql which nil)
                           (progn (setf (elt argsTemp i) (- num d))
                                  (copy-seq argsTemp))
                           (progn (setf (elt argsTemp i) (- num d))
                                  (let* ((newargs (copy-seq arglist)))
                                    (setf (nth (1- which) newargs)
                                          (copy-seq argsTemp))
                                    newargs)))))
           ;(print Temp1) (print Temp2) ;test result
           (setf result
                 (/ (- (apply func Temp1) (apply func Temp2))
                    (* 2 d)))
           (coerce result 'double-float)))
    ))

(defun find-function-min (func arglist &key (which nil) (alpha 0.005) (iterTime 6000))
  "find the min value in special function"
  (let ((result 0)
        (reArgs))
    (dotimes (tt iterTime)
      (let ((pd (partial-derivative-ge func arglist :which which))
            (costValue (apply func (loop for i in arglist collect (eval i))))
            (args (eval (elt arglist (1- which)))))
                                        ;(print pd)
                                        ;(print args)
        (setf (elt arglist (1- which))
              (make-array (length args) :initial-contents
                          (loop for i from 0 to (1- (length args)) collect
                               (cond ((< (elt pd i) 0) (+ (elt args i)
                                                          (* (random 2.0 *random-state*) alpha)))
                                     ((>= (elt pd i) 0) (- (elt args i)
                                                           (* (random 2.0 *random-state*) alpha)))
                                     ))))
        (if (< (apply func (loop for i in arglist collect (eval i))) costValue)
            (setf reArgs (elt arglist (1- which))
                  result (apply func (loop for i in arglist collect (eval i))))
            )
        ))
    (print "find the min value for function");(print reArgs)
    (print result)
    reArgs))

;;; exercise below
#|
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
|#
