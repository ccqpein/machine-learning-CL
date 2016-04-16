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

(defun read-data ()
  (with-open-file (f #P"./testData.txt"
                     :direction :input)
    (declare (inline *list-to-array))
    (do* ((l (read-line f) (read-line f nil 'eof))
          (x (get-num-out l) (if (eql l 'eof) '(0) (get-num-out l)))
          (ar (*list-to-array x) (*list-to-array x))
          (pp (print ar) (print ar)))
        ((eql l 'eof) "Finsh read data"))))

(defun array-slice (m i)
  "only work for two dimensions matrix. i is index number"
  (let* ((dim (array-dimensions m))
         (colNum (cadr dim)))
    (return-from array-slice (make-array colNum :initial-contents 
                        (loop for id from 0 to (1- colNum) collect
                             (aref m i id))))))

(declaim (inline array-slice))

(defun array-multiply (array1 array2)
  (return-from array-multiply
    (loop for i across array1
       for ii across array2 sum
         (* i ii))))

(declaim (inline array-multiply))

(defun compute-cost (X theta y)
  "X is matrix. y is result."
  (let ((result)
        (rowNum (1- (elt (array-dimensions X) 0))))
    (setf result 
    (loop for r from 0 to rowNum for temp = (array-slice X r) sum
         (expt
          (- (array-multiply temp theta) (elt y r)) 2)))
    result))

(defun partial-derivative (X theta y)
  "X is a r*c matrix"
  (let ((result)
        (colNum (1- (elt (array-dimensions X) 1)))
        (rowNum (1- (elt (array-dimensions X) 0))))
    (setf result 
          (loop for c from 0 to colNum collect
               (/
               (loop for r from 0 to rowNum for temp = (array-slice X r) sum
                    (* (- (array-multiply temp theta) (elt y r))
                       (elt temp c)))
               (1+ rowNum))))
    result))
