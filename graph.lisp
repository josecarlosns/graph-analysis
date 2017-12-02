(load "utils.lisp")
;; Class that represents a graph, it has 3 attributes:
;;     Properties -> a hash-table were in the pair (key value) the key is a label of a property of the graph
;;     and its value is the current value of the property.
;;         Keys:
;;             "description" -> description of the graph.
;;             "type" -> holds the type of the graph. 1 for directed, 2 for undirected.
;;             "density" -> the density of the graph.
;;             "weighted" -> t if the graph is weighted, nil otherwise.
;;             "average-distance" -> the average distance between nodes.
;;             "average-efficiency" -> the average efficiency of the graph.
;;             "expected-degree" -> the expected degree of the graph.
;;             "diameter" -> the diameter of the graph (longest of all shortest paths).
;;             "number-of-nodes" -> the number of nodes.
;;             "number-of-edges" -> the number of edges.
;;             "adj-list" -> the adjacency list used to represent the graph. Used as parameter by most algorithms.
;;             "degree-dist" -> the degree distribution of the graph.
;;     Nodes -> a list of all nodes of the graph
;;     Edges -> a list of all the edges of the graph
(defclass graph ()
    ((properties :accessor properties
        :initform (make-hash-table :test #'equal))
    (nodes :accessor nodes
        :initform nil
        :initarg :nodes)
    (edge :accessor edges
        :initform nil
        :initarg :edges)
    (adj-list :accessor adj-list
        :initform nil
        :initarg :adj-list)))

(defstruct node
    id
    index
    (adj-nodes nil)
    (properties nil))

(defstruct edge
    nodeout
    nodein
    (properties nil))

;; Returns the adjacency list for the graph
(defmethod generate-adj-list ((g graph))
    (let ((adj-list nil))
        (setf adj-list (make-array (gethash "number-of-nodes" (properties g)) :initial-element nil))
        (dotimes (x (length (nodes g)))
            (let ((node nil))
                (setf node (aref (nodes g) x))
                (setf (node-index node) x)))
        (dotimes (x (length (nodes g)))
            (let ((node nil))
                (setf node (aref (nodes g) x))
                (setf (aref adj-list (node-index node)) (if (null (node-adj-nodes node))
                                                            nil
                                                            (mapcar #'node-index (node-adj-nodes node))))))
        adj-list))

;; Returns an empty graph of the given type and weighted if weighted=t
(defmethod empty-graph (type weighted)
    (let ((graph nil))
        (setf graph (make-instance 'graph))
        (setf (gethash "type" (properties graph)) type)
        (setf (gethash "weighted" (properties graph)) weighted)
        (setf (gethash "number-of-nodes" (properties graph)) 0)
        (setf (gethash "number-of-edges" (properties graph)) 0)
        graph))

(defmethod nodep ((g graph) node)
    (dolist (n (nodes g))
        (when (equal (node-id n) node)
            (return t)))
    nil)

;; Returns t if the graph 'g' contains the edge 'edge', nil if otherwise
(defmethod edgep ((g graph) edge)
    (let ((adj-list nil))
        (setf adj-list (adj-list g))
        (if (find (second edge) (aref adj-list (first edge)) :test #'equal) t nil)))

;; Adds the given node to graph
;;     Parameters:
;;         g -> The graph for the node to be added.
;; (defmethod add-node ((g graph) node-id)
;;     (incf (gethash "number-of-nodes" (properties g)))
;;     (let ((new-node nil))
;;         (setf new-node (make-node :id node-id))
;;         (push new-node (nodes g))))

;; Adds the given edge to the graph
;;     Parameters:
;;         g -> the graph to be added the edge.
;;         edge -> the edge to be added
;;     Returns:
;;         The new edge added.
(defmethod add-edge ((g graph) edge)
    (incf (gethash "number-of-edges" (properties g)))
    (push edge (edges g)))

;; Prints information about the given graph, like number of nodes and edges, to the given stream. 
;; If verbose, then it will print adicional information like average distance, eficiency etc.
(defmethod print-graph-info ((g graph) &optional &key (stream t) (verbose nil))
    (terpri)
    (dotimes (n 50)
        (princ "#"))
    (terpri)
    (format stream "Number of nodes: ~a~%Number of edges: ~a~%" 
        (gethash "number-of-nodes" (properties g)) (gethash "number-of-edges" (properties g)))
    (when verbose
        (progn
            (format stream "Type: ~a~%" (if (= 1 (gethash "type" (properties g))) "directed" "undirected"))
            (format stream "Weighted: ~a~%" (if (gethash "weighted" (properties g)) "yes" "no"))
            (format stream "Average distance: ~1,4f~%" (gethash "average-distance" (properties g)))
            (format stream "Average efficiency: ~1,4f~%" (gethash "average-efficiency" (properties g)))
            (format stream "Density: ~1,2f%~%" (* 100 (gethash "density" (properties g))))
            (let ((expt-degree nil))
                (setf expt-degree (gethash "expected-degree" (properties g)))
                (when (= 2 (gethash "type" (properties g)))
                    (setf expt-degree (list expt-degree expt-degree)))
                (format stream "Expected degree ( OUT IN ): ~{~f ~}~%" expt-degree))
            (format stream "Diameter: ~1,2f~%" (gethash "diameter" (properties g)))
            (format stream "Degree distribution: Degree=(outcoming incoming)~%")
            (let ((degree-dist nil))
                (setf degree-dist (gethash "degree-dist" (properties g)))
                (if (= 2 (gethash "type" (properties g)))
                    (loop for degree from 0 to (1- (gethash "number-of-nodes" (properties g))) do
                        (let ((dist nil))
                            (setf dist (aref degree-dist degree))
                            (when (> dist 0)
                                (progn
                                    (dotimes (n 5)
                                        (format stream "~t"))
                                    (format stream "~d=(~,5f% ~,5f%)~%" degree (* 100 dist) (* 100 dist))
                                    (terpri)))))
                    (loop for degree from 0 to (1- (gethash "number-of-nodes" (properties g))) do
                        (let ((out nil) (in nil))
                            (setf out (aref (first degree-dist) degree))
                            (setf in (aref (second degree-dist) degree))
                            (when (or (> out 0) (> in 0))
                                (progn
                                    (dotimes (n 5)
                                        (format stream "~t"))
                                    (format stream "~d=(~,5f% ~,5f%)~%" degree (* 100 out) (* 100 in))))))))))
    (terpri)
    (dotimes (n 50)
        (princ "#"))
    (terpri))

;; Creates an random graph using the model of Erdös and Rényi.
;;     Parameters:
;;         number-of-nodes -> The number of nodes for the graph.
;;         type -> The type of the graph, which is 1 for directed and 2 for undirected.
;;         edge-prob -> The probability of an edge to exist, ranging from 0 to 100.
;;         Optional:
;;             verbose -> If true, will print information like progress, estimated time left etc..
;;     Returns:
;;         An random graph
(defmethod random-graph (number-of-nodes type edge-prob &optional &key (verbose nil))
    (let ((graph nil) (total-time 0) (start-time nil) (end-time nil) (r-state (make-random-state t)) (node-list nil))
        (setf graph (empty-graph type nil))
        (setf (gethash "number-of-nodes" (properties graph)) number-of-nodes)
        (when verbose
            (progn
                (terpri)
                (dotimes (n 50)
                    (princ "#"))
                (terpri)
                (format t "Generating graph...")
                (terpri)))
        (setf (nodes graph) (make-array number-of-nodes :initial-element nil))
        (dotimes (node-id number-of-nodes)
            (let ((new-node nil))
                (setf new-node (make-node :id node-id))
                (setf (aref (nodes graph) node-id) new-node)))
        (dotimes (n number-of-nodes)
            (let ((node1 nil) (node2 nil))
                (setf start-time (get-internal-real-time))
                (setf node1 (aref (nodes graph) n))
                (dotimes (x (if (= 1 type) number-of-nodes (- number-of-nodes n)))
                    (let ((edge nil) (node2 nil))
                        (setf node2 (if (= 1 type) x (+ x n)))
                        (setf node2 (aref (nodes graph) node2))
                        (when (and (not (= 0 edge-prob)) (not (= (node-id node1) (node-id node2))) (<= (/ (random 100001 r-state) 1000) edge-prob))
                            (progn
                                (setf edge (list node1 node2))
                                (add-edge graph edge)
                                (if (= 1 type)
                                    (push node2 (node-adj-nodes node1))
                                    (progn
                                        (push node1 (node-adj-nodes node2))
                                        (push node2 (node-adj-nodes node1))))))))
                (setf end-time (get-internal-real-time))
                (decf end-time start-time)
                (incf total-time end-time)
                (when verbose
                    (print-progress (1+ n) number-of-nodes total-time))))
        (setf (adj-list graph) (generate-adj-list graph))
        (when verbose
            (progn
                (terpri)
                (format t "Done!~%")
                (dotimes (n 50)
                    (princ "#"))
                (terpri)))
        graph))

;; Generates an regular undirected graph, that is, a graph where 
;; each node has the same degree of "degree"
(defmethod regular-graph (number-of-nodes degree type &optional &key (verbose nil))
    (let ((graph nil) (total-time 0) (degree-array nil))
        (setf graph (empty-graph type nil))
        (setf (gethash "number-of-nodes" (properties graph)) number-of-nodes)
        (when (= 2 type)
            (setf degree-array (make-array number-of-nodes :initial-element 0)))
        (when verbose
            (progn
                (terpri)
                (dotimes (n 50)
                    (princ "#"))
                (terpri)
                (format t "Generating regular graph...")
                (terpri)))
        (setf (nodes graph) (make-array number-of-nodes :initial-element nil))
        (dotimes (node-id number-of-nodes)
            (let ((new-node nil))
                (setf new-node (make-node :id node-id :index node-id))
                (setf (aref (nodes graph) node-id) new-node)))
        (dotimes (node1-index number-of-nodes)
            (let ((counter 0) (current-degree 0) (node1 nil) (node2-index nil) (adj-nodes nil) (start-time nil) (end-time nil))
                (setf start-time (get-internal-real-time))
                (setf node1 (aref (nodes graph) node1-index))
                (setf node2-index (- node1-index (floor (/ degree 2))))
                (when (< node2-index 0)
                    (setf node2-index (+ number-of-nodes node2-index)))
                (when (= 2 type)
                    (setf current-degree (aref degree-array (node-index node1))))
                (loop
                    (when (or (> counter number-of-nodes) (>= current-degree degree))
                        (return nil))
                    (when (>= node2-index number-of-nodes)
                        (setf node2-index (rem node2-index number-of-nodes)))
                    (let ((node2 nil))
                        (setf node2 (aref (nodes graph) node2-index))
                        (when (not (= node1-index node2-index))
                            (if (= 1 type)
                                (progn
                                    (incf current-degree)
                                    (add-edge graph (list node1 node2))
                                    (push node2 (node-adj-nodes node1)))
                                (progn
                                    (when (and  (< (aref degree-array (node-index node2)) degree))
                                                ;; (not (find node2 (node-adj-nodes node1))))
                                        (progn
                                            (incf (aref degree-array (node-index node1)))
                                            (incf (aref degree-array (node-index node2)))
                                            (add-edge graph (list node1 node2))
                                            (push node2 (node-adj-nodes node1))
                                            (push node1 (node-adj-nodes node2))
                                            (incf current-degree))))))
                        (incf node2-index)
                        (incf counter)))
                (setf end-time (get-internal-real-time))
                (decf end-time start-time)
                (incf total-time end-time)
                (when verbose
                    (print-progress (1+ node1-index) number-of-nodes total-time))))
        (when verbose
            (progn
                (terpri)
                (dotimes (n 50)
                    (princ "#"))
                (terpri)
                (format t "Done!")
                (terpri)))
        (setf (adj-list graph) (generate-adj-list graph))
        graph))

;; Creates a random graph using the model of Watts e Strogatz, named "small world", where initially
;; each node has a degree of "degree", and has an extra p (0-100) chance of "redefining" links to other nodes.
(defmethod small-world (number-of-nodes degree p type &optional &key (verbose nil))
    (let ((graph nil) (adj-list nil) (start-time nil) (end-time nil) (total-time 0) (progress 0) (max-progress nil) (r-state (make-random-state t)))
        (setf graph (regular-graph number-of-nodes degree type :verbose verbose))
        (setf adj-list (adj-list graph))
        (setf max-progress (* number-of-nodes (1- number-of-nodes)))
        (when (= 2 type)
            (setf max-progress (/ max-progress 2)))
        (when verbose
            (progn
                (terpri)
                (dotimes (n 50)
                    (princ "#"))
                (terpri)
                (format t "Generating small-world graph from regular graph...")
                (terpri)))
        (dotimes (x number-of-nodes)
            (dotimes (y (if (= 1 type) number-of-nodes (- number-of-nodes x)))
                (let ((rand nil) (node2 nil) (node1 nil))
                    (setf start-time (get-internal-real-time))
                    (setf node1 (aref (nodes graph) x))
                    (setf node2 (aref (nodes graph) (if (= 1 type) y (+ x y))))
                    (when (not (= (node-id node1) (node-id node2)))
                        (setf rand (random 100001 r-state))
                        (when (<= (/ rand 1000) p)
                            (let ((edge nil))
                                (if (find node2 (node-adj-nodes node1))
                                    (progn
                                        (decf (gethash "number-of-edges" (properties graph)))
                                        (setf (edges graph) (remove edge (edges graph) :test #'equal))
                                        (setf (node-adj-nodes node1) (remove node2 (node-adj-nodes node1)))
                                        (when (= 2 type)
                                            (progn
                                                (setf (node-adj-nodes node2) (remove node1 (node-adj-nodes node2)))
                                                (setf (edges graph) (remove (reverse edge) (edges graph) :test #'equal)))))
                                    (progn
                                        (add-edge graph edge)
                                        (push node2 (node-adj-nodes node1))
                                        (when (= 2 type)
                                            (push node1 (node-adj-nodes node2))))))))
                    (setf end-time (get-internal-real-time))
                    (decf end-time start-time)
                    (incf total-time end-time)
                    (when verbose
                        (print-progress progress max-progress total-time))
                    (incf progress))))
        (when verbose
            (progn
                (terpri)
                (dotimes (n 50)
                    (princ "#"))
                (terpri)
                (format t "Done!")
                (terpri)))
        (setf (adj-list graph) (adj-list graph))
        graph))

;; Returns an random scale-free graph, according to the Barabási and Albert (1999) model of scale-free graphs
(defmethod scale-free (number-of-nodes edges-per-insertion type &optional &key (verbose nil))
    (let  ((r-state nil) (graph nil) (degree nil) (adj-list nil) (start-time nil) (end-time nil) (total-time 0))
        (setf graph (empty-graph type nil))
        (setf (gethash "number-of-nodes" (properties graph)) number-of-nodes)
        (setf r-state (make-random-state t))
        (setf adj-list (make-array number-of-nodes :initial-element nil))
        (when verbose
            (progn
                (terpri)
                (dotimes (n 50)
                    (princ "#"))
                (terpri)
                (format t "Generating scale-free graph...")
                (terpri)))
        (setf (nodes graph) (make-array number-of-nodes :initial-element nil))
        (dotimes (node-id number-of-nodes)
            (let ((new-node nil))
                (setf new-node (make-node :id node-id :index node-id))
                (setf (aref (nodes graph) node-id) new-node)))
        (dotimes (node1-index number-of-nodes)
            (let ((node1 nil))
            (setf node1 (aref (nodes graph) node1-index))
            (setf start-time (get-internal-real-time))
            (let ((neighbors nil))
                (setf neighbors (choose-neighbors adj-list node1-index edges-per-insertion))
                (dolist (node2-index neighbors)
                    (let ((edge nil) (node2 nil))
                        (setf node2 (aref (nodes graph) node2-index))
                        (setf edge (list node1 node2))
                        (push node2 (node-adj-nodes node1))
                        (push node2-index (aref adj-list node1-index))
                        (add-edge graph edge)
                        (when (= 2 type)
                            (progn
                                (push node1-index (aref adj-list node2-index))
                                (push node1 (node-adj-nodes node2)))))))
                (setf end-time (get-internal-real-time))
                (decf end-time start-time)
                (incf total-time end-time)
                (when verbose
                    (print-progress (1+ node1-index) number-of-nodes total-time))))
        (when verbose
            (progn
                (terpri)
                (dotimes (n 50)
                    (princ "#"))
                (terpri)
                (format t "Done!")
                (terpri)))
        (setf (adj-list graph) adj-list)
        graph))
