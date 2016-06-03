(in-package #:CL-USER)

(defpackage #:toolsbox
  (:use #:CL)
  (:nicknames #:TB)
  (:export #:parse-string-to-float
           #:get-num-out
           #:*list-to-array
           #:*array-to-list
           #:read-data
           #:array-slice
           #:array-multiply
           #:logistic-regression
           #:partial-derivative-ge
           #:find-function-min
           #:point-distance
           #:points-average
           #:gen-random-num
           #:with-gensyms
           ))

(defpackage #:gradient-descent
  (:use #:CL #:TB)
  (:nicknames #:GD)
  )

(defpackage #:logistic-regression
  (:use #:CL #:TB)
  (:nicknames #:LR)
  )

(defpackage #:neural-network
  (:use #:CL #:TB)
  (:nicknames #:NN)
  )

(defpackage #:support-vector-machine
  (:use #:CL #:TB)
  (:nicknames #:SVM)
  )

(defpackage #:K-mean
  (:use #:CL #:TB)
  (:nicknames #:KM)
  )
