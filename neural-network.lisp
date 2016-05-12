(load "./package.lisp")
(load "./gradient-descent.lisp")
(load "./logistic-regression.lisp")
(in-package  #:neural-network)

(defun computate-single-layer (xArray theta)
  "theta is a matrix"
  (let* ((rowNum (1- (elt (array-dimensions theta) 0)))
         (nextLayer (make-array (+ rowNum 2) :initial-contents
                               (append '(1)
                                     (loop for i from 0 to rowNum collect
                                          (array-multiply xArray (array-slice theta i)))))))
    nextLayer))
    
