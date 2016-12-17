(require "asdf")
(push "./CLisp-toolbox-ccQ/" asdf:*central-registry*)
(asdf:load-system :ccQ-toolbox)

(defpackage #:gradient-descent
  (:use #:CL #:MT #:MXT)
  (:nicknames #:GD)
  )

(in-package #:gradient-descent)

(defun compute-cost (X theta y)
  "X is matrix. y is result."
  (let ((result)
        (rowNum (1- (elt (array-dimensions X) 0))))
    (setf result 
          (loop for r from 0 to rowNum for temp = (array-slice X r) sum
               (expt
                (- (array-multiply temp theta) (elt y r)) 2)))
    (/ result (* 2 rowNum))))

(defun partial-derivative (X theta y)
  "X is a r*c matrix"
  (let ((result)
        (colNum (1- (elt (array-dimensions X) 1)))
        (rowNum (1- (elt (array-dimensions X) 0))))
    (setf result 
          (loop for c from 0 to colNum collect
               (/
               (loop for r from 0 to rowNum for temp = (array-slice X r) sum
                    (* (- (array-multiply temp theta) (elt y r))
                       (elt temp c)))
               (1+ rowNum))))
    result))

(defun gradient-descent (X theta y alpha iterTime)
  (dotimes (i iterTime theta)
    (let ((pd (partial-derivative X theta y)))
      (loop for ind from 0 to (1- (length theta)) do
           (setf (elt theta ind)
                 (- (elt theta ind) (* alpha (elt pd ind)))))
      (print theta))))

;;; exercise below
;;;
;;;
#|
(defvar *X*)
(setf *X*
      (let ((temp
             (loop for ar in (read-data "./testData.txt") collect
                  (list 1 (elt ar 0)))))
        (make-array (list (length temp) (length (car temp))) :initial-contents temp)))

(defvar *y*)
(setf *y*
      (loop for ar in (read-data "./testData.txt") collect
           (elt ar 1)))

(defvar *theta*)
(setf *theta* (make-array 2 :initial-element 0))

(compute-cost *X* *theta* *y*)
(gradient-descent *X* *theta* *y* 0.01 1500)|#
