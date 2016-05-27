;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Merge function which been used often be a single file. Let the packages structure simplier
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(load "./package.lisp")
(in-package #:toolsbox)

(defun parse-string-to-float (line)
  "read the data from file, each line is x1 x2 ... y1, no comma in data"
  (with-input-from-string (s line)
    (loop for num = (read s nil nil)
       while num
       collect num)))
(declaim (inline parse-string-to-float))

(defun get-num-out (str)
  "change the string, which from parse-string-to-float, to number"
  (do* ((pos (position #\  str) (position #\  (subseq str (+ 1 poss))))
        (strr (subseq str 0 pos) (subseq str (1+ poss) (if (eql pos nil) nil (+ 1 poss pos))))
        (num (car (parse-string-to-float strr)) (car (parse-string-to-float strr)))
        (nums (list num) (append nums (list num)))
        (poss pos (if (eql pos nil) nil (+ 1 poss pos))))
       ((eql pos nil)
        nums)))

(defun *list-to-array (ll)
  (let* ((len (length ll))
         (ar (make-array len :initial-contents ll)))
    ar))
(declaim (inline *list-to-array))

(defun read-data (path)
  (let ((result))
    (with-open-file (f path
                       :direction :input)
      (do* ((l (read-line f) (read-line f nil))
            (x (get-num-out l) (if (not l) nil (get-num-out l)))
            (ar (*list-to-array x) (*list-to-array x))
            (p (push ar result) (push ar result)))
           ((not l) (print "Finsh read data" ))))
    (cdr result)))

#| Rewrite this function to macro
(defun array-slice (m i)
  "only work for two dimensions matrix. i is index number"
  (let* ((dim (array-dimensions m))
         (colNum (cadr dim)))
    (return-from array-slice (make-array colNum :initial-contents 
                        (loop for id from 0 to (1- colNum) collect
                             (aref m i id))))))
(declaim (inline array-slice))|#

(defmacro array-slice (m i)
  (let ((mm (gensym))
        (ii (gensym)))
    `(let* ((,mm ,m)
            (,ii ,i)
            (dim (array-dimensions ,mm))
            (colNum (cadr dim)))
       (make-array colNum :initial-contents
                   (loop for id from 0 to (1- colNum) collect
                        (aref ,mm ,ii id))))))

(defun array-multiply (array1 array2)
  (let ((result
         (loop for i across array1
            for ii across array2 sum
              (* i ii))))
    result))
(declaim (inline array-multiply))

(defun logistic-regression (z)
  "calculate the g(z), when g(z) larger than 0.5, return 1, else return 0"
  (let ((g 0.0d0))
    (setf g
          (+ 1.0l0 (exp (- z))))
    (if (= g 1.0)
        (return-from logistic-regression (- 1.0d0 1.0e-5))
        (return-from logistic-regression (/ 1.0d0 g)))))
(declaim (inline logistic-regression))

(defun partial-derivative-ge (func arglist &key (which nil) (d (expt 2 -4)))
  "to calculate the partial-derivative. argspoint is which args need to caculate partial derivative. Use method: if function args are '(1 2), do not need which; if your function need '(#(1 2 3) #(1 3 3)), you need the which to point which arg you want to calculate partial derivative."
  (let* ((arglist (loop for i in arglist collect
                       (eval i)))
         (args (if (eql which nil)
                   arglist
                   (nth (1- which) arglist)))
         (argslen (length args)))
    (loop for i from 0 to (1- argslen) collect
         (let* ((result)
                (argsTemp (copy-seq args))
                (num (elt argsTemp i))
                (Temp1 (if (eql which nil)
                           (progn (setf (elt argsTemp i) (+ num d))
                                  (copy-seq argsTemp))
                           (progn (setf (elt argsTemp i) (+ num d))
                                  (let* ((newargs (copy-seq arglist)))
                                    (setf (nth (1- which) newargs)
                                          (copy-seq argsTemp))
                                    newargs))))
                (Temp2 (if (eql which nil)
                           (progn (setf (elt argsTemp i) (- num d))
                                  (copy-seq argsTemp))
                           (progn (setf (elt argsTemp i) (- num d))
                                  (let* ((newargs (copy-seq arglist)))
                                    (setf (nth (1- which) newargs)
                                          (copy-seq argsTemp))
                                    newargs)))))
           ;(print Temp1) (print Temp2) ;test result
           (setf result
                 (/ (- (apply func Temp1) (apply func Temp2))
                    (* 2 d)))
           (coerce result 'double-float)))
    ))

(setf *random-state* (make-random-state t))

(defun find-function-min (func arglist &key (which nil) (alpha 0.005) (iterTime 6000))
  "find the min value in special function"
  (let ((result 0)
        (reArgs))
    (dotimes (tt iterTime)
      (let ((pd (partial-derivative-ge func arglist :which which))
            (costValue (apply func (loop for i in arglist collect (eval i))))
            (args (eval (elt arglist (1- which)))))
                                        ;(print pd)
                                        ;(print args)
        (setf (elt arglist (1- which))
              (make-array (length args) :initial-contents
                          (loop for i from 0 to (1- (length args)) collect
                               (cond ((< (elt pd i) 0) (+ (elt args i)
                                                          (* (random 2.0 *random-state*) alpha)))
                                     ((>= (elt pd i) 0) (- (elt args i)
                                                           (* (random 2.0 *random-state*) alpha)))
                                     ))))
        (if (< (apply func (loop for i in arglist collect (eval i))) costValue)
            (setf reArgs (elt arglist (1- which))
                  result (apply func (loop for i in arglist collect (eval i))))
            )
        ))
    (print "find the min value for function");(print reArgs)
    (print result)
    reArgs))
