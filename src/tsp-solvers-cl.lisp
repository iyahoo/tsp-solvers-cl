(in-package :cl21-user)
(defpackage tsp-solvers-cl
  (:use #:cl21))
(in-package :tsp-solvers-cl)

;; Graph
(defvar *graph-path* (merge-pathnames #P"data/graph"))
(defvar *G*)          ; Graph

;; Parameter
(defvar *alpha* 1)              ; Pheromone influence (is fixed to 1).
(defvar *tau* :unbound)         ; Initial value of pheromones.
(defparameter *tau-matrix* :unbound)
(defvar *beta* :unbound)        ; Heuristic influence (takes value from [1,15]).
(defvar *rho* :unbound)         ; Evaporation rate (takes value from [0,1]).
(defvar *m* :unbound)           ; Number of ants.
(defvar *n* 10)                 ; Numberofnodes.
(defvar *Q* :unbound)           ; Constant (amount of pheromone that can be delivered by each ant).
(defvar *init-pos* :unbound)    ; Initial position of node of ants.

;; Random state
(defvar *random-state-path* (merge-pathnames #P"data/random-state"))
(defvar *my-random-state*)

;; Define class
(defclass ant ()
  ((position :accessor ant-position :initarg :position)
   (route :accessor ant-route :initarg :route)))

(defmethod print-object ((obj ant) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream "position: ~a route: ~a" (ant-position obj) (ant-route obj))))

;; Read from files functions
(defun read-file-from (path)
  (with-open-file (in path :direction :input)
    (read in)))

(defun read-graph-data ()
  (read-file-from *graph-path*))

(defun read-random-state ()
  (read-file-from *random-state-path*))

;; Initialize function for parameters
(defun initialize-tau (init-tau)
  (setf *tau-matrix* (make-array (list *n* *n*) :initial-element init-tau)))

(defun initialization
    (&key (tau 5) (beta 10) (rho 0.5) (m 10) (q 100) (init-pos 0))
  (setf *G* (read-graph-data)
        *tau* tau
        *beta* beta
        *rho* rho
        *m* m
        *q* q
        *init-pos* init-pos)
  (initialize-tau *tau*))

;; Choose next node
(defun tau-and-heuristic (i j)
  (* (expt (aref *tau-matrix* i j) *alpha*)
     (expt (aref *G* i j) *beta*)))

(defun calc-sum-of-all-th (i unvisited-node)
  "Sum of all tau multiply heuristic on unvisited node"
  (assert (listp unvisited-node))
  (assert (integerp i))
  (float (loop :for k :in unvisited-node :sum (tau-and-heuristic i k))))

(defun probability-of-be-chosen (current-node next-node sum-of-all-th)
  "A node's probability of be chosen by ant."
  (/ (tau-and-heuristic current-node next-node) sum-of-all-th))

(defun choose-node (ant unvisited-node &optional (rest-probability 1.0))
  "Recursive function.
   if random value less than probability of be chosen, return the candidate node.
   Otherwise, this function will be recursion with rest unvisited nodes."
  (assert (> rest-probability 0))
  (assert (not (null unvisited-node)))
  (let ((sum-of-all-th (calc-sum-of-all-th (ant-position ant) unvisited-node))
        (candidate-node (first unvisited-node))
        (rand-val (random rest-probability *my-random-state*)))
    (if (< (- rand-val
              (probability-of-be-chosen (ant-position ant) candidate-node sum-of-all-th))
           0)
        candidate-node
        (choose-node ant (rest unvisited-node) (- rest-probability rand-val)))))

;; Core
(defun make-ants (initial-position)
  "Make *m*-th ants with initial-position."
  (loop :repeat *m*
        :collect (make-instance 'ant
                   :position initial-position :route (list initial-position))))

(defmacro update-ant (ant pos route)
  `(setf (ant-position ,ant) ,pos
         (ant-route ,ant) ,route))

(defun ant-go-node (ant &optional (unvisited-node (remove (ant-position ant) (iota *n*))))
  "Recursive function"
  (if (> (length unvisited-node) 0)
      (let ((a-choose-node (choose-node ant unvisited-node)))
        ;; (setf (ant-position ant) a-choose-node
        ;;       (ant-route ant) (cons a-choose-node (ant-route ant)))
        (update-ant ant a-choose-node (cons a-choose-node (ant-route ant)))
        (ant-go-node ant (remove a-choose-node unvisited-node)))
      ;; (setf (ant-position ant) *init-pos*
      ;;       (ant-route ant) (cons *init-pos* (ant-route ant)))
      (update-ant ant *init-pos* (cons *init-pos* (ant-route ant)))))

(defun pheromone-evaporation ()
  "Update tau-matrix (evaporation by rho)"
  (let ((indexes (iota *n*)))
    (loop :for i :in indexes :do
      (loop :for j :in indexes :do
        (setf (aref *tau-matrix* i j) (* (aref *tau-matrix* i j) *rho*))))))

(defun update-pheromon-matrix (ants)
  (declare (ignorable ants))
  (pheromone-evaporation))

(defun ACO ()
  (setf *my-random-state* (read-random-state))
  (initialization)
  (format t "Initial tau matrix:~%~a~%" *tau-matrix*)
  (let ((ants (make-ants 0)))
    (map ^(ant-go-node %1) ants)
    (update-pheromon-matrix ants)))
