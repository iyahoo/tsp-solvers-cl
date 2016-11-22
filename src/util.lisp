(in-package :cl21-user)
(defpackage util
  (:use #:cl21
        #:cl-annot))
(in-package :util)

(cl-annot:enable-annot-syntax)

;; Random state
(defvar *random-state-path* (merge-pathnames #P"data/random-state"))

@export
(defvar *my-random-state*)

;; Graph
(defvar *graph-path* (merge-pathnames #P"data/graph"))

@export
(defvar *G*)

;; Accessor
;; For reduce cost of function call and it will be handled by `setf`,
;; each process is defined by macro.
@export
(defmacro cost-of (i j)
  "Return cost between ith-node and jth-node."
  `(aref *G* ,i ,j))

;; Read from files functions
(defun read-file-from (path)
  (with-open-file (in path :direction :input)
    (read in)))

@export
(defun read-graph-data ()
  (read-file-from *graph-path*))

@export
(defun read-random-state ()
  (read-file-from *random-state-path*))

;; make graph
(defmacro set-array (array i j num)
  `(setf (aref ,array ,i ,j) ,num))

(defun make-graph (n &rest argv)
  (declare (ignore argv))
  (let* ((nth (parse-integer n))
         (adj-mat (make-array (list nth nth))))
    (loop :for i :to (1- nth) :do
      (loop :for j :from i :to (1- nth) :do
        (if (= i j)
            (set-array adj-mat i j 0)
            (let ((random-number (1+ (random 9))))
              (set-array adj-mat i j random-number)
              (set-array adj-mat j i random-number)))))))
