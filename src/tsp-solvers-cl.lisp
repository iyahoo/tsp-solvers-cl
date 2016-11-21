(in-package :cl21-user)
(defpackage tsp-solvers-cl
  (:use #:cl21
        #:aco))
(in-package :tsp-solvers-cl)

(defun main ()
  (aco:aco 5 10 0.5 10 100 0 1000))
