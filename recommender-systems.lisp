(load "./package.lisp")
(load "./toolsbox.lisp")
(in-package #:recommender-systems)


;; Assume r(r, j) is 1, that means user j has rated movie i. Kinds of bool
;; This algorithm looks like required Gradient Descent

;;; Given parameter vector for user to learn feature vector for movie
;;; Assume the matrix's row is movie, row is user's data

(defun get-feature-vector (matrix mIndex)
  "matrix is users' rating form, mIndex means which movie be learned"
  
  )


(defmacro sigma (funcExp indPara paraList times)
  (with-gensyms (result paraPool inFuncExp)
    `(let ((,result 0)
           (,paraPool ,paraList)
           (,inFuncExp ,funcExp))
       (do* ((tt 0 (incf tt))
             (thisPara (elt ,paraPool tt)
                       (elt ,paraPool tt)))
            ((= tt ,times))
         (setf (elt ,inFuncExp ,indPara) thisPara)
         (setf ,result (+ ,result (eval ,inFuncExp))))
       ,result
       )))


