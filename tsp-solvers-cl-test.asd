#|
  This file is a part of tsp-solvers-cl project.
  Copyright (c) 2016 cl-yaho (s1200191@gmail.com)
|#

(in-package :cl-user)
(defpackage tsp-solvers-cl-test-asd
  (:use :cl :asdf))
(in-package :tsp-solvers-cl-test-asd)

(defsystem tsp-solvers-cl-test
  :author "cl-yaho"
  :license "LLGPL"
  :depends-on (:tsp-solvers-cl
               :prove)
  :components ((:module "t"
                :components
                ((:test-file "tsp-solvers-cl"))))
  :description "Test system for tsp-solvers-cl"

  :defsystem-depends-on (:prove-asdf)
  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run-test-system) :prove-asdf) c)
                    (asdf:clear-system c)))
