(in-package :CL-USER)

(defpackage #:gradient-descent
  (:use #:CL)
  (:nicknames #:GD)
  (:export #:parse-string-to-float
           #:get-num-out
           #:*list-to-array
           #:read-data
           #:array-slice
           #:array-multiply
           ))

(defpackage #:logistic-regression
  (:use #:CL #:GD)
  (:nicknames #:LR)
  )
