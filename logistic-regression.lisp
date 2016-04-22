(load "./package.lisp")
(load "./gradient-descent.lisp")
(in-package #:logistic-regression)

(defun logistic-regression (z)
  "calculate the g(z), when g(z) larger than 0.5, return 1, else return 0"
  (let ((g (/ 1 (+ 1 (exp (* -1 z))))))
    (if (>= g 0.5) 1 0)))

