(load "./package.lisp")
(load "./toolsbox.lisp")
(in-package #:anomaly-detection)


(defun standard-deviation (numlist)
  "return the value include the average and standard deviation of the numlist"
  (let* ((sum (apply #'+ numlist))
         (len (length numlist))
         (aver (/ sum len))
         (SD (sqrt (loop for i in numlist sum
                       (expt (- i aver) 2)))))
    (list aver SD)))

(defmacro array-slice-col (m i)
  (with-gensyms (mm ii)
    `(let* ((,mm ,m)
            (,ii ,i)
            (dimens (array-dimensions ,mm))
            (rowNum (nth 0 dimens)))
       (loop for i from 0 to (1- rowNum) collect
            (aref ,mm i ,ii)))))


(defun anomaly-detection (matrix)
  "as usual, the input var is matrix include every attributions of sample and get the average and the standard deviation"
  (let* ((matrixDim (array-dimensions matrix))
         (colNum (nth 1 matrixDim))
         (result '()))
    (do* ((colIndex 0 (1+ colIndex))
          (thisCol (array-slice-col matrix colIndex)
                   (array-slice-col matrix colIndex)))
         ((= colIndex (1- colNum))
          (aappend result (standard-deviation thisCol)))  ;last time
      ;(print thisCol) (print colIndex) (print (standard-deviation thisCol))
      (aappend result (standard-deviation thisCol)))
    result))
      
