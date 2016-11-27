(in-package :cl21-user)
(defpackage ga
  (:use #:cl21
        #:cl-annot
        #:util))
(in-package :ga)

(cl-annot:enable-annot-syntax)

(defvar *n* 10)                         ; Number of nodes. (It require even value)
(defvar *m* :unbound)                   ; Number of agents.
(defvar *truncation-point*              ; Point of truncation point
  (/ *n* 2))
(defvar *crossover-point* :unbound)     ; Point of crossover (it have to over (/ *n* 2))
(defvar *mutation-rate* :unbound)

;; Define class
;; The route is represented by ordered-representation (0 origin).
;; See `http://aidiary.hatenablog.com/entry/20110109/1294581648'
(defun make-route-randomly (n &optional (route '()) (candidate-list (iota n)))
  "Recursive function."
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

(defun normal->ordered (normal-list &optional route (rest-graph (iota *n*)))
  (if (null normal-list)
      (nreverse route)
      (let* ((chosen-node (first normal-list))
             (the-node-pos (position chosen-node rest-graph)))
        (normal->ordered (rest normal-list) (cons the-node-pos route)
                         (remove chosen-node rest-graph)))))

;; agents methods
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

(defmethod length-of-route ((agent agent))
  (let* ((route (ordered->normal (agent-route agent)))
         (first-node (first route)))
    (%length-of-route (append route (list first-node)))))

(defmethod evaluate ((agent agent))
  "A fitness is difiend as a length of route."
  (setf (agent-fitness agent) (length-of-route agent)))

(defun sort-agents-by-fitness (agents)
  (stable-sort agents #'<= :key #'agent-fitness))

(defun get-parent-heritage (agent)
  (subseq (agent-route agent) *truncation-point*))

(defun get-child-heritage (agent)
  (subseq (agent-route agent) 0 *truncation-point*))

(defun cross-over (agent agents idx)
  (when (>= idx *truncation-point*)
    (let* ((parent (nth agents (- idx *truncation-point*)))
           (child-gene (get-child-heritage agent))
           (parent-gene (get-parent-heritage parent)))
      (setf (agent-route agent) (append child-gene parent-gene)))))

(defun random-exchange (lst)
  "This function do not allow duplicate of list elements."
  (let* ((count-lst (length lst))
         (first-rand-idx (random count-lst *my-random-state*))
         (second-rand-idx (random count-lst *my-random-state*))
         (first-elm (nth lst first-rand-idx))
         (second-elm (nth lst second-rand-idx)))
    (substitute first-elm second-elm (substitute second-elm first-elm lst) :from-end t :count 1)))

(defun mutation (agent)
  (when (< (random 100 *my-random-state*) *mutation-rate*)
    (let ((route (ordered->normal (agent-route agent))))
      (setf (agent-route agent) (normal->ordered (random-exchange route))))))

;; Main
(defun initialization (&key (m 10) (init-pos 0) (crossover-point 7) (mutation-rate 10))
  (setf *G* (read-graph-data)
        *m* m
        *init-pos* init-pos
        *crossover-point* crossover-point
        *mutation-rate* mutation-rate))

(defun GA (m crossover-point mutation-rate last-generation &optional file-out-p (file-name ""))
  (declare (ignorable last-generation))
  (setf *my-random-state* (read-random-state))
  (initialization :m m :crossover-point crossover-point :mutation-rate mutation-rate)
  (let ((agents (make-agents)))
    (map #'evaluate agents)
    (setf agents (sort-agents-by-fitness agents))
    (let ((final-agents))
      (with-open-file (output (merge-pathnames (append "results/" file-name))
                              :direction :output :if-does-not-exist :create :if-exists :supersede)
        (dotimes (g last-generation final-agents)
          (map ^(cross-over %1 agents %2) agents (iota *m*))
          (map #'mutation agents)
          (map #'evaluate agents)
          (setf agents (sort-agents-by-fitness agents))
          (when file-out-p
            (map (lambda (agent)
                   (format output "~{~a~^,~}~%" (ordered->normal (agent-route agent))))
                 agents))
          (when (= (1+ g) last-generation)
            (setf final-agents agents)))))))
