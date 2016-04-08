(defun parse-string-to-float (line)
  (with-input-from-string (s line)
    (loop
       :for num := (read s nil nil)
       :while num
       :collect num)))

(defun get-num-out (str)
  (do* ((pos (position #\  str) (position #\  (subseq str (1+ pos))))
        (strr (subseq str 0 pos) (subseq str (1+ poss) (+ 1 poss pos)))
        (num (parse-string-to-float strr) (append num (parse-string-to-float strr)))
        (len (1+ pos) (+ pos len))
        (poss pos (+ poss pos)))
       ((eql (length num) 7) (print num) (print len) (print (length str)))))

(defun readData ()
  (with-open-file (f #P"./testData.txt"
                     :direction :input)
    (do* ((l (read-line f) (read-line f nil 'eof))

          (tt (print l) (print l)))

        
        ((eql l 'eof) "okok"))))

(defun computeCost (X y theta)
  "X is sample, and is a array. y is result."
  (let ((temp)
        (result))
    (setf temp 
          (loop for i-x across X
             for i-theta across theta sum
               (* i-x i-theta)))
    (setf result (* (- temp y) (- temp y)))
    result))


