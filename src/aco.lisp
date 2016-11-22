(in-package :cl21-user)
(defpackage aco
  (:use #:cl21
        #:cl-annot
        #:util))
(in-package :aco)

(cl-annot:enable-annot-syntax)

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

;; Define class
(defclass ant ()
  ((position :accessor ant-position :initarg :position)
   (route :accessor ant-route :initarg :route)))

(defmethod print-object ((obj ant) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream "position: ~a route: ~a" (ant-position obj) (ant-route obj))))

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
        *q* (float q)
        *init-pos* init-pos)
  (initialize-tau *tau*))

;; Accessor
;; For reduce cost of function call and it will be handled by `setf`,
;; each process is defined by macro.
(defmacro pheromone-of (i j)
  "Return cost between ith-node and jth-node."
  `(aref *tau-matrix* ,i ,j))

;; Choose next node
(defun tau-and-heuristic (i j)
  "Depends on Grapd *G*, *alpha*, *beta*, *tau-matrix*."
  (* (expt (pheromone-of i j) *alpha*)
     (expt (/ 1.0 (cost-of i j)) *beta*)))

(defun calc-sum-of-all-th (i unvisited-node)
  "Sum of all tau multiply heuristic on unvisited node"
  (assert (listp unvisited-node))
  (assert (integerp i))
  (float (loop :for k :in unvisited-node :sum (tau-and-heuristic i k))))

(defun probability-of-be-chosen (current-node next-node sum-of-all-th)
  "A node's probability of be chosen by ant."
  (/ (tau-and-heuristic current-node next-node) sum-of-all-th))

(defmethod choose-node ((ant ant) unvisited-node sum-of-all-th &optional (rest-probability 1.0))
  "Recursive function. Depends on random value.
   if random value less than probability of be chosen, return the candidate node.
   Otherwise, this function will be recursion with rest unvisited nodes."
  (assert (> rest-probability 0))
  (assert (not (null unvisited-node)))
  (if (= (length unvisited-node) 1)
      (first unvisited-node)
      (let* ((candidate-node (first unvisited-node))
             (rand-val (random rest-probability *my-random-state*))
             (chosen-probability (probability-of-be-chosen (ant-position ant)
                                                           candidate-node
                                                           sum-of-all-th)))
        (if (> (- chosen-probability rand-val) 0)
            candidate-node
            (choose-node ant (rest unvisited-node) sum-of-all-th
                         (- rest-probability chosen-probability))))))

;; For ant functions
(defun make-ants (initial-position)
  "Depends on *m*.
   Make *m* ants with initial-position."
  (loop :repeat *m*
        :collect (make-instance 'ant
                                :position initial-position :route (list initial-position))))

(defmacro update-ant (ant pos route)
  `(setf (ant-position ,ant) ,pos
         (ant-route ,ant) ,route))

(defmethod ant-go-node ((ant ant) &optional
                                    (unvisited-node (remove (ant-position ant) (iota *n*))))
  "Recursive function. Depends on *init-pos*"
  (if (> (length unvisited-node) 0)
      (let* ((sum-of-all-th (calc-sum-of-all-th (ant-position ant) unvisited-node))
             (chosen-node (choose-node ant unvisited-node sum-of-all-th)))
        (update-ant ant chosen-node (cons chosen-node (ant-route ant)))
        (ant-go-node ant (remove chosen-node unvisited-node)))
      (update-ant ant *init-pos* (cons *init-pos* (ant-route ant)))))

(defun %length-of-route (route &optional (acc 0))
  "Recursive function.
   Inner function for recursion of `length-of-route`."
  (assert (listp route))
  (let ((start-node (first route))
        (next-node (second route)))
    (if (and start-node next-node)
        (let ((cost (cost-of start-node next-node)))
          (%length-of-route (rest route) (+ acc cost)))
        acc)))

(defmethod length-of-route ((ant ant))
  (let ((route (ant-route ant)))
    (%length-of-route route)))

(defun %put-pheromone (route route-length)
  "Recursive function."
  (let ((start-node (first route))
        (next-node (second route)))
    (when (and start-node next-node)
      (let ((new-pheromone (+ (pheromone-of start-node next-node) (/ *Q* route-length))))
        (setf (pheromone-of start-node next-node) new-pheromone)
        (%put-pheromone (rest route) route-length)))))

(defmethod put-pheromone ((ant ant))
  (let ((route (ant-route ant))
        (route-length (length-of-route ant)))
    (%put-pheromone route route-length)))

;; pheromone functions
(defun pheromone-evaporation ()
  "Update tau-matrix (evaporation by rho)"
  (let ((indexes (iota *n*)))
    (doeach (i indexes)
      (doeach (j indexes)
        (setf (aref *tau-matrix* i j) (* (aref *tau-matrix* i j) *rho*))))))

(defun pheromone-reinforcement (ants)
  (map ^(put-pheromone %1) ants))

(defun update-pheromon-matrix (ants)
  (pheromone-evaporation)
  (pheromone-reinforcement ants))

;; Main
@export
(defun ACO (tau beta rho m q init-pos last-generation)
  "Returen final *tau-matrix* and ants"
  (setf *my-random-state* (read-random-state))
  (initialization :tau tau :beta beta :rho rho :m m :q q :init-pos init-pos)
  (let ((final-ants))
    (dotimes (_ last-generation (list *tau-matrix* final-ants))
      (let ((ants (make-ants *init-pos*)))
        (map ^(ant-go-node %1) ants)
        (update-pheromon-matrix ants)
        (setf final-ants ants)))))
