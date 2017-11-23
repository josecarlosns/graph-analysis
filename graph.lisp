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
;;             "node-ids" -> a list of node IDs for the graph.
;;             "diameter" -> the diameter of the graph (longest of all shortest paths).
;;             "number-of-nodes" -> the number of nodes.
;;             "number-of-edges" -> the number of edges.
;;             "altered" -> true if the graph has been altered since its last analysis.
;;     Node -> a hash-table were in the pair (key value) the key is the ID of the node and the value is actually
;;     another hash table whose keys are labels to properties of the node.
;;         Keys:
;;             "label" -> the label of the node.
;;             "distances" -> an array holding the distances of this node to all other nodes.
;;             "bfs-tree" -> the BFS (breath-first search) search tree as an array, with this node as root.
;;             "edges" -> an list holding all the edges that comes from this node.
;;             "degree" -> a pair (OUT IN) of degrees for the node. 
;;             "vulnerability" -> the vulnerability of the node
;;     Edges -> a hash-table were in the pair (key value) the key is the pair (nodeout nodein) of the edge and the value is actually
;;     another hash table whose keys are labels to properties of the edge.
;;         Keys:
;;             "weight" -> the weight of the node.
(defclass graph ()
    (
        (properties :accessor properties
            :initform (make-hash-table :test #'equal))
        (node :accessor nodes
            :initform (make-hash-table :test #'equal)
            :initarg :nodes)
        (edge :accessor edges
            :initform (make-hash-table :test #'equal)
            :initarg :edges)))

;; Creates an random graph with the given number of nodes, type, probability of existing edge and random weight, if any.
;;     Parameters:
;;         number-of-nodes -> The number of nodes for the graph.
;;         type -> The type of the graph, which is 1 for directed and 2 for undirected.
;;         edge-prob -> The probability of an edge to exist, ranging from 0 to 100.
;;         Optional:
;;             weight-range -> If given an value, each existing edge will be given an random value from 0 to weight-range.
;;     Returns:
;;         An random graph with the given caracteristics.
(defmethod random-graph (number-of-nodes type edge-prob &optional &key (weight-range nil))
    (setf *random-state* (make-random-state t))
    (let ((graph nil))
        (setf graph (empty-graph type (if weight-range t nil)))
        (dotimes (node1 number-of-nodes)
            (when (null (gethash node1 (nodes graph)))
                (add-node graph :node-id node1))
            (dotimes (j (if (= 1 type) number-of-nodes (- number-of-nodes node1)))
                (let ((node2 nil) (weight nil) (edge nil))
                    (setf node2 (if (= 1 type) j (+ j node1)))
                    (when (null (gethash node2 (nodes graph)))
                        (add-node graph :node-id node2))
                    (when (and (not (= 0 edge-prob)) (not (= node1 node2)) (<= (/ (random 100001) 1000) edge-prob))
                            (progn
                                (setf edge (list node1 node2))
                                (when weight-range 
                                    (setf weight (random (+ 1 weight-range))))
                                (add-edge graph edge))))))
        graph))

;; Returns an empty graph of the given type and weighted if weighted=t
(defmethod empty-graph (type weighted)
    (let ((graph nil))
        (setf graph (make-instance 'graph))
        (setf (gethash "type" (properties graph)) type)
        (setf (gethash "weighted" (properties graph)) weighted)
        (setf (gethash "number-of-nodes" (properties graph)) 0)
        (setf (gethash "number-of-edges" (properties graph)) 0)
        graph))

;; Returns t if the graph 'g' contains the node 'node-id', nil if otherwise
(defmethod nodep ((g graph) node-id)
    (if (gethash node-id (nodes g)) t nil))

;; Returns t if the graph 'g' contains the edge 'edge', nil if otherwise
(defmethod edgep ((g graph) edge)
    (if (gethash edge (edges g)) t nil))

;; Adds the given node to graph
;;     Parameters:
;;         g -> The graph for the node to be added.
;;         Optional:
;;             node-id -> the ID of the node, if nil it will be the ID of the number-of-nodes +1.
;;             label -> the label of the node, if nil no label will be given.
;;     Returns:
;;         The ID of the new node
(defmethod add-node ((g graph) &optional &key (node-id nil) (label nil))
    (let ((new-node nil))
        (if node-id
            (setf new-node node-id)
            (setf new-node (1+ (gethash "number-of-nodes" (properties g)))))
        (push new-node (gethash "node-ids" (properties g)))
        (incf (gethash "number-of-nodes" (properties g)))
        (setf (gethash new-node (nodes g)) (make-hash-table :test #'equal))
        (setf (gethash "altered" (properties g)) t)
        (when label
            (setf (gethash "label" (gethash new-node (nodes g))) label))
        new-node))

;; Adds the given edge to the graph
;;     Parameters:
;;         g -> the graph to be added the edge.
;;         edge -> the edge to be added
;;             Optional:
;;                 weight -> the weight of the graph. If nil, no weight will be added.
;;     Returns:
;;         The new edge added.
(defmethod add-edge ((g graph) edge &optional &key (weight nil))
    (let ((new-edge nil))
        (setf new-edge edge)
        (push new-edge (gethash "edges" (gethash (first new-edge) (nodes g))))
        (when (= 2 (gethash "type" (properties g)))
            (push new-edge (gethash "edges" (gethash (second new-edge) (nodes g)))))
        (setf (gethash edge (edges g)) (make-hash-table :test #'equal))
        (when weight
            (setf (gethash "weight" (gethash edge (edges g))) weight))
        (incf (gethash "number-of-edges" (properties g)))
        (setf (gethash "altered" (properties g)) t)
        new-edge))

;; Removes the given edge from the graph
(defmethod remove-edge ((g graph) edge)
    (let ((node1-edges nil) (node2-edges nil))
        (setf node1-edges (gethash "edges" (gethash (first edge) (nodes g))))
        (setf node1-edges (remove edge node1-edges))
        (setf (gethash "edges" (gethash (first edge) (nodes g))) node1-edges)
        (when (= 2 (gethash "type" (properties g)))
            (progn
                (setf node2-edges (gethash "edges" (gethash (second edge) (nodes g))))
                (setf node2-edges (remove edge node2-edges))
                (setf (gethash "edges" (gethash (second edge) (nodes g))) node2-edges)))
        (setf (gethash edge (edges g)) nil)
        (decf (gethash "number-of-edges" (properties g)))
        (setf (gethash "altered" (properties g)) t)))

;; Removes the given node and all its associated edges from the graph
(defmethod remove-node ((g graph) node)
    (dolist (edge (gethash "edges" (gethash node (nodes g))))
        (remove-edge g edge))
    (setf (gethash node (nodes g)) nil)
    (let ((node-ids nil))
        (setf node-ids (gethash "node-ids" (properties g)))
        (setf node-ids (remove node node-ids))
        (setf (gethash "node-ids" (properties g)) node-ids))
    (decf (gethash "number-of-nodes" (properties g)))
    (setf (gethash "altered" (properties g)) t))

;; Prints information about the given graph, like number of nodes and edges, to the given stream. 
;; If verbose, then it will print adicional information like average distance, eficiency etc.
(defmethod print-graph-info ((g graph) &optional &key (stream t) (verbose nil))
    (format stream "Number of nodes: ~a~%Number of edges: ~a~%" 
        (gethash "number-of-nodes" (properties g)) (gethash "number-of-edges" (properties g)))
    (when verbose
        (progn
            (format stream "Type: ~a~%" (if (= 1 (gethash "type" (properties g))) "directed" "undirected"))
            (format stream "Weighted: ~a~%" (if (gethash "weighted" (properties g)) "yes" "no"))
            (format stream "Average distance: ~1,2f~%" (gethash "average-distance" (properties g)))
            (format stream "Average efficiency: ~1,2f~%" (gethash "average-efficiency" (properties g))))))

;; Prints information about the given node in the graph, like ID, label etc., to the given stream.
;; If verbose, then it will print adicional information like the bfs tree, distances array etc.
(defmethod print-node-info ((g graph) node-id &optional &key (stream t) (verbose nil))
    (let ((node-properties nil))
        (setf node-properties (gethash node-id (nodes g)))
        (format stream "ID: ~a~%" node-id)
        (format stream "Label: ~a~%" (gethash "label" node-properties))
        (format stream "Efficiency: ~1,2f~%" (gethash "efficiency" node-properties))
        (format stream "Vulnerability: ~1,2f~%" (gethash "vulnerability" node-properties))
        (when verbose
            (progn
                (format stream "Distances array: node-id:distance ~%")
                (let ((distances-array nil))
                    (setf distances-array (gethash "distances" node-properties))
                    (dotimes (n (length distances-array))
                        (format stream "~t~a:~a" n (aref distances-array n))))
                (format stream "BFS tree (as array): node-id:closest-parent~%")
                (let ((bfs-tree nil))
                    (setf bfs-tree (gethash "bfs-tree" node-properties))
                    (dotimes (n (length bfs-tree))
                        (format stream "~t~a:~a" n (aref bfs-tree n))))))))

;; Will print the edge info to the given stream
(defmethod print-edge-info ((g graph) edge &optional &key (stream t) (verbose nil))
    (let ((edge-properties nil))
        (setf edge-properties (gethash edge (edges g)))
        (format stream "Edge (NodeOut NodeIn): (~a ~a)~%" (first edge) (second edge))
        (if (gethash "weighted" (properties g))
            (format stream "Weight: ~a~%" (third edge)))))
