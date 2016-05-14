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
                                             (logistic-regression (array-multiply xArray
                                                                                  (array-slice theta i))))))))
    nextLayer))

(defun Tmatrix (matrix)
  "return the T-Matrix"
  (let ((newRowNum (array-dimensions matrix 1))
        (newColNum (array-dimensions matrix 0)))
  (make-array (list newRowNum newColNum)
              :initial-contents
              (loop for

(defun backpropagation (y finalLayer)
  (let ((lamEnding (make-array (length y) :initial-contents
                               (loop for i from 0 to (1- (length y)) collect
                                    (- (elt y i) (elt finalLayer i))))))
    (print lamEnding)))
