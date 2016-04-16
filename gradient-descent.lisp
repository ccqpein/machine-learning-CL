(defun parse-string-to-float (line)
  (with-input-from-string (s line)
    (loop
       :for num := (read s nil nil)
       :while num
       :collect num)))

(defun get-num-out (str)
  (declare (inline parse-string-to-float))
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

(defun readData ()
  (with-open-file (f #P"./testData.txt"
                     :direction :input)
    (declare (inline *list-to-array))
    (do* ((l (read-line f) (read-line f nil 'eof))
          (x (get-num-out l) (if (eql l 'eof) '(0) (get-num-out l)))
          (ar (*list-to-array x) (*list-to-array x))
          (pp (print ar) (print ar)))
        ((eql l 'eof) "Finsh read data"))))

(defun computeCost (X theta y)
  "X is sample, and is a array. y is result."
  (let ((temp)
        (result)
        (len-sample (length x)))
    (declare (special temp))
    (setf temp ;There is the array multify methond below
          (loop for i-x across X
             for i-theta across theta sum
               (* i-x i-theta)))
    (setf result (* (- temp y) (- temp y)))
    result))

(defun partial-derivative (X theta y)
  (let ((temp))
    (/
    (- (loop for i-x across X
          for i-theta across theta sum
            (* i-x i-theta))
       y)
    ))

#|
(defmacro partial-derivative (argspoint func args &optional (d (expt 2 -100)))
  "to calculate the partial-derivative. argspoint is which args need to caculate partial derivative."
  (let ((realArgs (cadr args))
        (tempargs (nth argspoint (cadr args))))
    (declare (inline *list-to-array))
    (cond ((typep tempargs 'array) 
           (let* ((len-array (length tempargs))
                  (copyArgs (copy-list realArgs))
                   (temp1 (*list-to-array
                           (loop for i from 0 to (1- len-array) collect
                                (+ (elt tempargs i) d))))
                   (temp2 (*list-to-array
                           (loop for i from 0 to (1- len-array) collect
                                (- (elt tempargs i) d))))
                   (newArg1 (progn (setf (nth argspoint copyArgs) temp1) copyArgs))
                  (newArg2 (progn (setf (nth argspoint realArgs) temp2) realArgs)))
              `(/ (- (funcall ,func ,@newArg1)
                     (funcall ,func ,@newArg2))
                    (* 2 ,d))))
          
          ((typep tempargs 'integer) 
           (let* ((copyArgs (copy-list realArgs))
                   (newArg1 (progn (setf (nth argspoint realArgs) (+ tempargs d))
                                  realArgs))
                  (newArg2 (progn (setf (nth argspoint copyArgs) (- tempargs d))
                                  copyArgs)))
              `(/ (- (funcall ,func ,@newArg1)
                          (funcall ,func ,@newArg2))
                    (* 2 ,d)))))))
|#



