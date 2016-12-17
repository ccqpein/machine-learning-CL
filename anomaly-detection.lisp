(require "asdf")
(push "./CLisp-toolbox-ccQ/" asdf:*central-registry*)
(asdf:load-system :ccQ-toolbox)

(defpackage #:anomaly-detection
  (:use #:CL #:MT #:MXT)
  (:nicknames #:AD)
  )

(in-package #:anomaly-detection)


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
      
