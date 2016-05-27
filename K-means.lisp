(load "./package.lisp")
(load "./toolsbox.lisp")
(in-package #:K-mean)

(defun point-distance (p1 p2)
  "calculate the distance of two points, point input by array and have same length"
  (let ((len (length p1)))
    (if (/= (length p2) len)
        (print "You need make sure arrays length be same")
        (sqrt (loop for i from 0 to (1- len) sum
                   (expt (- (elt p1 i) (elt p2 i)) 2))))))
        
                 
(defun K-mean (matrix K &key (iterTime 1000))
  "matrix is input matrix included all train data set, K is cluster number"
  (let ((result '())   ;result is a list for all cluster center point
        (rowNum (array-dimension matrix 0))
        (colNum (array-dimension matrix 1)))
    (dotimes (tt iterTime)
      (let ((initKArray (loop for i from 0 to (1- K) collect
                             (array-slice matrix (random rowNum *random-state*)))))
        
