(load "./package.lisp")
(load "./toolsbox.lisp")
(in-package  #:neural-network)

;; Machine Learning week 4 classic Neural Network
;; Assume X is vector included n elements, so theta should m*(n+1) matrix

(setf testX (make-array 3 :initial-contents '(1 1 3)))
(setf testM (make-array '(3 4) :initial-contents '((1 1 0 0)
                                                   (1 2 0 0)
                                                   (1 3 0 0))))

(defun next-layer (X matrix)
  (loop for i from 1 to (array-dimension matrix 0)
     for thisRow = (array-slice matrix (1- i))
     for thisX = (make-array (1+ (length X))
                             :initial-contents (list* '1 (*array-to-list X)))
     collect (array-multiply thisX thisRow)))
