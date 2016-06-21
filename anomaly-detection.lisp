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
  (let* ((matrixDim (array-dimensions matrix))
         (rowNum (nth 0 matrixDim))
         (colNum (nth 1 matrixDim))
         (result '()))
    (do* ((colIndex 0 (1+ colIndex))
          (thisCol (loop for i from 0 to (1- rowNum) collect
                        (aref matrix i colIndex))
                   (loop for i from 0 to (1- rowNum) collect
                        (aref matrix i colIndex))))
         ((= colIndex (1- colNum))
          (aappend result (standard-deviation thisCol)))  ;last time
      ;(print thisCol) (print colIndex) (print (standard-deviation thisCol))
      (aappend result (standard-deviation thisCol)))
    result))
      
