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

(defmacro *array-to-list (ar)
  (let ((a (gensym)))
    `(let ((,a ,ar))
       (loop for i across ,a collect i))))

(defmacro point-distance-multi (pn num)
  "pn should be a list of lists, num means how many digits you want to calculate to average value. For example if pn is (list '(2 3 4) '(1 2 3)), result equal (3/2 5/2 7/2) if num is 3, the result will be (3/2 5/2) if num is 2"
  (let* ((arrayNum (gensym))
         (len (gensym))
         (arrayList (gensym)))
    `(let* ((,len ,num)
            (,arrayList ,pn)
            (,arrayNum (length ,arrayList)))
       ;(print ,len) (print ,arrayList) (print ,arrayLen)
       (loop for i from 0 to (1- ,len) collect
            (/ (loop for ar in ,arrayList sum
                    (elt ar i)) ,arrayNum)))))
    
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
           ;(print distanceList) (print po)(print arraysReList)
           ))
    arraysReList))
#|
(defun K-mean (matrix K &key (iterTime 1000))
  "matrix is input matrix included all train data set, K is cluster number"
  (let ((result '())   ;result is a list for all cluster center point
        (rowNum (array-dimension matrix 0))
        (colNum (array-dimension matrix 1))
        (initKArray (loop for i from 0 to (1- K) collect
                             (array-slice matrix (random rowNum *random-state*)))))
    (do* ((i 1 (incf i))
          (poitsList initKArray ()

|#
