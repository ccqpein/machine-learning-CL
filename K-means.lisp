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

(defmacro point-distance-multi (num pn)
  (let* ((pnn (eval pn))
         (pnnn (loop for i in pnn collect (cons 'quote (list i))))
         (symbols (loop for i from 1 to num collect
                       (gensym))))
    (print symbols)
    (print pnn)
    (print pnnn)
    `(maplist (lambda (,@symbols) (/ (+ ,@symbols) ,num))
          ,@pnnn)))

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
