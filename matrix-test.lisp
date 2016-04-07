(defun computeCost (X y theta)
  (let ((temp)
        (result))
    (setf temp 
          (loop for i-x across X
             for i-theta across theta sum
               (* i-x i-theta)))
    (setf result (* (- temp y) (- temp y)))
    result))
