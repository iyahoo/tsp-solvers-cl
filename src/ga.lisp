(in-package :cl21-user)
(defpackage ga
  (:use #:cl21
        #:cl-annot
        #:util))
(in-package :ga)

(cl-annot:enable-annot-syntax)

(defvar *n* 10)                 ; Number of nodes.
(defvar *m* :unbound)           ; Number of agents.
(defvar *init-pos* :unbound)    ; Initial position of node of agents.

;; Define class
;; The route is represented by ordered-representation (0 origin).
;; See `http://aidiary.hatenablog.com/entry/20110109/1294581648'
(defun make-route-randomly (n &optional (route '()) (candidate-list (iota n)))
  "Recursive function."
  (print #"n is ${n}, route is ${route}, candidate-list is ${candidate-list}")
  (if (null candidate-list)
      (nreverse route)
      (let* ((chosen-node-idx (random n *my-random-state*))
             (chosen-node (nth candidate-list chosen-node-idx)))
        (make-route-randomly (1- n) (cons chosen-node-idx route)
                             (remove chosen-node candidate-list)))))

(defclass agent ()
  ((fitness :accessor agent-fitness :initarg :fitness)
   (route :accessor agent-route :initform (make-route-randomly *n*))))

(defmethod print-object ((obj agent) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream "fitness: ~a route: ~a" (agent-fitness obj) (agent-route obj))))

(defun make-agents ()
  "Depends on *n* "
  (loop :repeat *n*
        :collect (make-instance 'agent :fitness 0)))

;; Route representaiton
(defun ordered->normal (ordered-list &optional route (rest-graph (iota *n*)))
  "Recursive function.
   Convert ordered list to normal list with is route of walking graph."
  (if (null ordered-list)
      (nreverse route)
      (let* ((next-node-idx (first ordered-list))
             (node (nth rest-graph next-node-idx)))
        (ordered->normal (rest ordered-list) (cons node route) (remove node rest-graph)))))

;; Main
(defun initialization (&key (m 10) (init-pos 0))
  (setf *G* (read-graph-data)
        *m* m
        *init-pos* init-pos))

(defun GA (m init-pos last-generation)
  (declare (ignorable last-generation))
  (setf *my-random-state* (read-random-state))
  (initialization :m m :init-pos init-pos)
  (make-agents))
