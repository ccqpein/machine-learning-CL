;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Merge function which been used often be a single file. Let the packages structure simplier
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(load "./package.lisp")
(in-package #:toolsbox)

(defmacro with-gensyms ((&rest names) &body body)
  `(let ,(loop for n in names collect `(,n (gensym)))
          ,@body))

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

(defmacro *list-to-array (l)
  (with-gensyms (ll)
    `(let* ((,ll ,l)
            (len (length ,ll))
         (ar (make-array len :initial-contents ,ll)))
    ar)))

(defmacro *array-to-list (ar)
  (with-gensyms (a)
    `(let ((,a ,ar))
       (loop for i across ,a collect i))))

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

(defmacro array-slice (m i)
  "get array from matrix, m is matrix i is index"
  (with-gensyms (mm ii)
    `(let* ((,mm ,m)
            (,ii ,i)
            (dim (array-dimensions ,mm))
            (colNum (cadr dim)))
       (make-array colNum :initial-contents
                   (loop for id from 0 to (1- colNum) collect
                        (aref ,mm ,ii id))))))

(defmacro array-slice-col (m i)
  (with-gensyms (mm ii)
    `(let* ((,mm ,m)
            (,ii ,i)
            (dimens (array-dimensions ,mm))
            (rowNum (nth 0 dimens)))
       (loop for i from 0 to (1- rowNum) collect
            (aref ,mm i ,ii)))))

(defmacro array-multiply (array1 array2)
  (with-gensyms (arr1 arr2)
    `(let ((,arr1 ,array1)
           (,arr2 ,array2))
       (loop for i across ,arr1
          for ii across ,arr2 sum
            (* i ii)))))

(defun logistic-regression (z)
  "calculate the g(z), when g(z) larger than 0.5, return 1, else return 0"
  (let ((g 0.0d0))
    (setf g
          (+ 1.0l0 (exp (- z))))
    (if (= g 1.0)
        (return-from logistic-regression (- 1.0d0 1.0e-5))
        (return-from logistic-regression (/ 1.0d0 g)))))

(declaim (inline logistic-regression))

(defun standard-deviation (numlist)
  "return the value include the average and standard deviation of the numlist"
  (let* ((sum (apply #'+ numlist))
         (len (length numlist))
         (aver (/ sum len))
         (SD (sqrt (loop for i in numlist sum
                       (expt (- i aver) 2)))))
    (list aver SD)))

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

(defun point-distance (p1 p2)
  "calculate the distance of two points, point input by array and have same length"
  (let ((len (length p1)))
    (if (/= (length p2) len)
        (print "You need make sure arrays length be same")
        (sqrt (loop for i from 0 to (1- len) sum
                   (expt (- (elt p1 i) (elt p2 i)) 2))))))

(defmacro points-average (pn num)
  "pn should be a list of lists, num means how many digits you want to calculate to average value. For example if pn is (list '(2 3 4) '(1 2 3)), result equal (3/2 5/2 7/2) if num is 3, the result will be (3/2 5/2) if num is 2"
  (with-gensyms (arrayNum len arrayList)
    `(let* ((,len ,num)
            (,arrayList ,pn)
            (,arrayNum (length ,arrayList)))
       ;(print ,len) (print ,arrayList) (print ,arrayLen)
       (loop for i from 0 to (1- ,len) collect
            (/ (loop for ar in ,arrayList sum
                    (elt ar i)) ,arrayNum)))))

(defun gen-random-num (n times)
  "return result is list. n is number limit, times is number of results. No same number in the result list"
  (let ((result '())
        (x))
    (dotimes (i times result)
      (tagbody
         (setf x (random n *random-state*))
         (go tag-b)
       tag-a
         (setf x (random n *random-state*))
         (go tag-b)
       tag-b
         (if (find x result)
             (go tag-a)
             (go tag-c))
       tag-c
         (setf result (append result (list x)))))
    ))

(defmacro aappend (l &rest eles)
  "l must be symbol not expression. For example, (aappend a 2 3 4) is fine, (aappend '(1 2) 2 3) and (aappend (list 2 3) 2 2) will issue error"
  (with-gensyms (elel)
    `(let ((,elel (list ,@eles)))
       (loop for i in ,elel do
            (setf ,l (append ,l (list i))))
       ,l)))

(defmacro sigma ((&rest exp) indPara times &rest paraList)
  "Improve the sigma macto from second version, this marco can do the several parameters at same time now:
For example (sigma (+ 1 2) (1 2) 4 '(1 2 3 4 5) '(5 4 3 2))
             => (+ (+ 1 5) (+ 2 4) (+ 3 3) (+ 4 2))"
  `(+ ,@(loop for tt from 0 to (1- times)
           for expT = (copy-list exp)
           do
             (loop for id from 0 to (1- (length indPara))
                  for paraTT = (eval (nth id paraList))
                do (setf (nth (nth id indPara) expT)
                         (nth tt paraTT)))
           collect expT)))

(defmacro sigma3 ((&rest exp) indPara paraList times)
  "Do simga calculation, for example if paraExp = '(2 3): (sigma (+ 1 2) 1 paraExp 2) => (+ (+ 2 2) (+ 3 2))"
  (let ((para (eval paraList)))
      `(+ ,@(loop for tt from 0 to (1- times)
               for expT = (copy-list exp)
               do (setf (nth indPara expT) (nth tt para))
               collect expT))))

;;; The marco below come from http://stackoverflow.com/questions/39048561/issues-when-write-loop-collect-in-macro/39052748#39052748, but it works not follow the purpose.

(defmacro sigma2 (exp indPara ll)
  `(+ ,@(loop for i in ll and exp1 = (copy-list exp)
              do (setf (elt exp1 indPara) i)
              collect exp1)))


