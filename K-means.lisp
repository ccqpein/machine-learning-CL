(load "./package.lisp")
(load "./toolsbox.lisp")
(in-package #:K-mean)

(defun point-distance (p1 p2)
  "calculate the distance of two points, point input by array and have same length"
  (let ((len (length p1)))
    (if (/= (length p2) len)
        (print "You need make sure arrays length be same")
        (sqrt (loop for i from 0 to (1- len) sum
                   (expt (- (elt p1 i) (elt p2 i)) 2))))))

(defmacro *min (l)
  (let ((ll (gensym)))
  `(let ((,ll (coerce ,l 'list)))
     (min ,@ll)
     )))

(defun org-matrix (matrix arrayList)
  (let* ((len (length arrayList))
         (arraysList (make-list len :initial-element '())))
    (loop for i from 0 to (1- (array-dimension matrix 0)) do
         (let* ((thisArray (array-slice matrix i))
                (distanceList (coerce (loop for a from 0 to (1- len) collect
                                   (point-distance thisArray (nth a arrayList))) 'list))
                ;(minVal (*min distanceList))
                ;(po (position minVal distanceList)))
                )
                                        ;(setf (nth po arraysList) (append (nth po arrayList) thisArray))
           (print (type-of distanceList))
           (print distanceList)
                ))))

#|
(defun K-mean (matrix K &key (iterTime 1000))
  "matrix is input matrix included all train data set, K is cluster number"
  (let ((result '())   ;result is a list for all cluster center point
        (rowNum (array-dimension matrix 0))
        (colNum (array-dimension matrix 1))
        (initKArray (loop for i from 0 to (1- K) collect
                             (array-slice matrix (random rowNum *random-state*)))))
    (do ((i 1 (incf i))
         ))))|#
