(load "./package.lisp")
(load "./toolsbox.lisp")
(in-package #:anomaly-detection)


;; Can use toolsbox:points-average
(defun standard-deviation (numlist)
  "return the value include the average and standard deviation of the numlist"
  (let* ((sum (apply #'+ numlist))
         (len (length numlist))
         (aver (/ sum len))
         (SD (sqrt (loop for i in numlist sum
                       (expt (- i aver) 2)))))
    (list aver SD)))


(defun anomaly-detection (matrix)
  "as usual, the input var is matrix include every attributions of sample and get the average and the standard deviation"
  
