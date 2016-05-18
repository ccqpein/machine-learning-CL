(load "./package.lisp")
(load "./gradient-descent.lisp")
(load "./logistic-regression.lisp")
(in-package #:support-vector-machine)

(defun compute-cost (X theta y lamd)
  "SVM cost function just has bit diffrences from logistic-regression"
  (let ((result 1L0)
        (rowNum (- (elt (array-dimensions X) 0) 1)))
    (setf result
          (loop for r from 0 to rowNum sum
               (+ (* (nth r y)
                     (log (logistic-regression (array-multiply (array-slice X r)
                                                               theta))))
                  (* (- 1 (nth r y))
                     (log (- 1 (logistic-regression (array-multiply (array-slice X r)
                                                                    theta))))))))
                                        ;(print result)
    (setf result (/ (- result) lamd))
    result)
  )

