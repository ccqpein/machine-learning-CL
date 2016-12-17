(require "asdf")
(push "./CLisp-toolbox-ccQ/" asdf:*central-registry*)
(asdf:load-system :ccQ-toolbox)

(defpackage #:logistic-regression
  (:use #:CL #:MT #:MXT)
  (:nicknames #:LR)
  )

(in-package #:logistic-regression)

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
;; 2016-5-18 the FIND-FUNCTION-MIN function move to toolsbox

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
