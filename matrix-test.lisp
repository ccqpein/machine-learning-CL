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
    (setf temp 
          (loop for i-x across X
             for i-theta across theta sum
               (* i-x i-theta)))
    (setf result (/ (* (- temp y) (- temp y))
                    (* 2d0 len-sample)))
    result))

(defun partial-derivative (func x &optional (d (expt 2 -100)) &rest other)
  "to calculate the partial-derivative. x and is array."
  (let* ((len-array (length x))
;         (temp-array (make-array len-array :element-type 'double-float))
         (temp-y)
         (result))
    (setf temp-y (funcall func x other)) ;need more
    (loop for i from 1 to len-array do
         (setf (elt x i) (+ (elt x i) d)))
    (setf result (- (funcall func x) temp-y))))
