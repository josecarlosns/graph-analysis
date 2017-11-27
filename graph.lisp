;; (load "metrics.lisp")
;; Class that represents a graph, it has 3 properites:
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
;;     Edges -> a list of all the edges of the graph
(defclass graph ()
    ((properties :accessor properties
        :initform (make-hash-table :test #'equal))
    (edge :accessor edges
        :initform nil
        :initarg :edges)))

;; Returns an empty graph of the given type and weighted if weighted=t
(defmethod empty-graph (type weighted)
    (let ((graph nil))
        (setf graph (make-instance 'graph))
        (setf (gethash "type" (properties graph)) type)
        (setf (gethash "weighted" (properties graph)) weighted)
        (setf (gethash "number-of-nodes" (properties graph)) 0)
        (setf (gethash "number-of-edges" (properties graph)) 0)
        graph))

(Defmethod nodep ((g graph) node)
    (if (>= node (gethash "number-of-nodes" (properties g))) nil t))

;; Returns t if the graph 'g' contains the edge 'edge', nil if otherwise
(defmethod edgep ((g graph) edge)
    (if (find edge (edges g) :test #'equal) t nil))

;; Adds the given node to graph
;;     Parameters:
;;         g -> The graph for the node to be added.
(defmethod add-node ((g graph))
    (incf (gethash "number-of-nodes" (properties g))))

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
    (format stream "Number of nodes: ~a~%Number of edges: ~a~%" 
        (gethash "number-of-nodes" (properties g)) (gethash "number-of-edges" (properties g)))
    (when verbose
        (progn
            (format stream "Type: ~a~%" (if (= 1 (gethash "type" (properties g))) "directed" "undirected"))
            (format stream "Weighted: ~a~%" (if (gethash "weighted" (properties g)) "yes" "no"))
            (format stream "Average distance: ~1,4f~%" (gethash "average-distance" (properties g)))
            (format stream "Average efficiency: ~1,4f~%" (gethash "average-efficiency" (properties g)))
            (format stream "Density: ~1,2f%~%" (* 100 (gethash "density" (properties g))))
            (format stream "Expected degree ( OUT IN ): ~{~f ~}~%" (gethash "expected-degree" (properties g)))
            (format stream "Diameter: ~1,2f~%" (gethash "diameter" (properties g)))
            (format stream "Degree distribution: Degree=(outcoming incoming)~%")
            (loop for prob in (gethash "degree-dist" (properties g)) do
                (format stream "~t~t~t~d=(~3,4f% ~3,4f%)~%" (first prob) (float (* 100 (second prob))) (float (* 100 (third prob))))))))

;; Creates an random graph with the given number of nodes, type, probability of existing edge and random weight, if any.
;;     Parameters:
;;         number-of-nodes -> The number of nodes for the graph.
;;         type -> The type of the graph, which is 1 for directed and 2 for undirected.
;;         edge-prob -> The probability of an edge to exist, ranging from 0 to 100.
;;         Optional:
;;             weight-range -> If given an value, each existing edge will be given an random value from 0 to weight-range.
;;     Returns:
;;         An random graph with the given caracteristics.
(defmethod random-graph (number-of-nodes type edge-prob)
    (setf *random-state* (make-random-state t))
    (let ((graph nil))
        (setf graph (empty-graph type nil))
        (setf (gethash "number-of-nodes" (properties graph)) number-of-nodes)
        (dotimes (node1 number-of-nodes)
            (dotimes (j (if (= 1 type) number-of-nodes (- number-of-nodes node1)))
                (let ((node2 nil) (edge nil))
                    (setf node2 (if (= 1 type) j (+ j node1)))
                    (when (and (not (= 0 edge-prob)) (not (= node1 node2)) (<= (/ (random 100001) 1000) edge-prob))
                        (progn
                            (setf edge (list node1 node2))
                            (add-edge graph edge))))))
        (setf (gethash "adj-list" (properties graph)) (adj-list graph))
        graph))