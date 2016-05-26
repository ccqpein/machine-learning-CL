(load "./package.lisp")
(load "./toolsbox.lisp")
(in-package #:K-mean)

(defun point-distance (p1 p2)
  "calculate the distance of two points, point input by array and have same length"
  (let ((len (length p1)))
    (if (/= (length p2) len)
        (print "You need make sure arrays length are same")
        (sqrt (loop for i from 0 to (1- len) sum
                   (expt (- (elt p1 i) (elt p2 i)) 2))))))
        
                 
