(load "./package.lisp")
(load "./toolsbox.lisp")
(in-package #:K-mean)


(defun org-matrix (matrix arrayList)
  (let* ((len (length arrayList))
         (arraysReList (make-list len :initial-element '())))
    (loop for i from 0 to (1- (array-dimension matrix 0)) do
         (let* ((thisArray (array-slice matrix i))
                (distanceList (loop for a from 0 to (1- len) collect
                                   (point-distance thisArray (nth a arrayList))))
                (minVal (car (sort (copy-seq distanceList) #'<)))
                (po (position minVal distanceList)))
           (setf (nth po arraysReList) (push thisArray (nth po arraysReList)))
           ;(print distanceList) (print po) (print arraysReList)
           ))
    arraysReList))
           
(defun K-mean (matrix K &key (iterTime 1000))
  "matrix is input matrix included all train data set, K is cluster number"
  (let* ((result)   ;result is a list for all cluster center point
         (rowNum (array-dimension matrix 0))
         (colNum (array-dimension matrix 1))
         (nn (gen-random-num rowNum K))
         (initKArray (loop for i from 0 to (1- K) collect
                          (array-slice matrix (elt nn i)))))
    ;(print initKArray)
    (do* ((i 1 (incf i))
          (poitsList initKArray
                     (loop for ii from 0 to (1- K) collect
                          (*list-to-array (points-average (nth ii arrayGroupList) colNum))))
          (arrayGroupList (org-matrix matrix poitsList)
                          (org-matrix matrix poitsList))
          )
         ((> i iterTime)
          (print "done") ;(print arrayGroupList) (print poitsList)
          (print poitsList)))
    result))
