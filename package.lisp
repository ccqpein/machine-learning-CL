(in-package #:CL-USER)

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
  (:export #:logistic-regression
           #:partial-derivative-ge
           #:find-function-min
           ))

(defpackage #:neural-network
  (:use #:CL #:GD #:LR)
  (:nicknames #:NN)
  )
