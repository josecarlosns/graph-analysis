(load "graph.lisp")
(load "utils.lisp")

;; Returns the adjacency list for the node
(defmethod adj-list ((g graph) node)
    (gethash "edges" (gethash node (nodes g))))
    
;; Sets and returns an pair (X Y) where:
;;     x -> the degree out of the node
;;     y -> the degree in of the node
(defmethod degree ((g graph) node)  
    (if (not (gethash "altered" (properties g)))
        (gethash "degree" (gethash node (nodes g)))
        (let ((ad-list nil) (degree nil))
            (setf degree (list 0 0))
            (setf ad-list (adj-list g node))
            (setf (first degree) (list-length ad-list))
            (if (= 2 (gethash "type" (properties g)))
                (setf (second degree) (first degree))
                (dolist (n (gethash "node-ids" (properties g)))
                    (when (not (equal node n))
                        (let ((count nil) (edge-list nil))
                            (setf count (second degree))
                            (setf edge-list (gethash "edges" (gethash n (nodes g))))
                            (dolist (edge edge-list)
                                (if (= (first edge) node)
                                    (incf count)))
                            (setf (second degree) count)))))
            (setf (gethash "degree" (gethash node (nodes g))) degree))))

;; Sets and returns the density of the graph
(defmethod density ((g graph))
    (if (not (gethash "altered" (properties g)))
        (gethash "density" (properties g))
            (let ((num-of-nodes nil) (num-of-edges nil))
                (setf num-of-nodes (gethash "number-of-nodes" (properties g)))
                (setf num-of-edges (gethash "number-of-edges" (properties g)))
                (if (= (gethash "type" (properties g)) 1)
                    (setf (gethash "density" (properties g)) (* 1.0 (/ num-of-edges (* (- num-of-nodes 1) num-of-nodes))))
                (setf (gethash "density" (properties g)) (* 1.0 (/ num-of-edges (/ (* (- num-of-nodes 1) num-of-nodes) 2))))))))

;; Returns the degree distribution (out, if inp) of 'degree' for the graph
(defmethod degree-dist ((g graph) degree inp)
    (let ((counter nil))
        (setf counter 0)
        (dolist (n (gethash "node-ids" (properties g)))
            (let ((node-degree nil))
                (setf node-degree (degree g n))
                (when (equal degree (if inp (second node-degree) (first node-degree)))
                    (setf counter (+ 1 counter)))))
        (* 1.0 (/ counter (list-length (nodes g))))))

;; Sets (if nil) and returns the expected degree of the graph as an pair (OUT IN) where:
;;     OUT -> The expected degree out of nodes.
;;     IN -> The expected degree in of nodes.
(defmethod expt-degree ((g graph) inp)
    (if (not (gethash "altered" (properties g)))
        (gethash "expected-degree" (properties g))
            (let ((degree-sum nil) (deg-index nil))
            (setf degree-sum '(0 0))
                (dolist (e (gethash "node-ids" (properties g)))
                    (let ((node-degree nil))
                        (setf node-degree (gethash "degree" (gethash e (nodes g))))
                    (incf (first degree-sum) (first node-degree))
                    (incf (second degree-sum) (second node-degree))))
            (setf (first degree-sum) (float (/ (first degree-sum) (gethash "number-of-nodes" (properties g)))))
            (setf (second degree-sum) (float (/ (second degree-sum) (gethash "number-of-nodes" (properties g)))))
            (setf (gethash "expected-degree" (properties g)) degree-sum))))

;; Sets and returns the distances array and the bfs-tree for the graph given the origin point as the node n1
(defmethod bfs-search ((g graph) n1)
    (let ((distances nil) (bfs-tree nil) (unvisited-nodes nil) (distance nil))
        (setf bfs-tree (make-array (gethash "number-of-nodes" (properties g)) :initial-element -1))
        (setf distances (make-array (gethash "number-of-nodes" (properties g)) :initial-element -1))
        (setf unvisited-nodes (make-queue :head nil :tail nil))
        (queue-put n1 unvisited-nodes)
        (setf (aref bfs-tree n1) n1)
        (setf (aref distances n1) 0)
        (loop
            (let ((current-node nil))
                (setf current-node (queue-pop unvisited-nodes))
                (dolist (e (gethash "edges" (gethash current-node (nodes g))))
                    (let ((neighbor-node nil))
                        (setf neighbor-node (if (equal current-node (first e)) (second e) (first e)))
                        (when (= -1 (aref bfs-tree neighbor-node))
                                (progn
                                    (setf (aref bfs-tree neighbor-node) current-node)
                                    (setf (aref distances neighbor-node) (1+ (aref distances current-node)))
                                    (queue-put neighbor-node unvisited-nodes))))))
                (when (queue-empty unvisited-nodes)
                    (progn
                        (setf (gethash "distances" (gethash n1 (nodes g))) distances)
                        (setf (gethash "bfs-tree" (gethash n1 (nodes g))) bfs-tree)
                        (return (list distances bfs-tree)))))))

;; Returns the distance from node1 to node2
(defmethod distance ((g graph) node1 node2)
    (aref (gethash "distances" (gethash node1 (nodes g))) node2))

;; Returns the efficiency of the connection from node1 to node2
(defmethod efficiency ((g graph) node1 node2)
    (float (/ 1 (distance g node1 node2))))

;; Returns the vulnerability of the node, will print progress if verbose.
(defmethod vulnerability ((g graph) node &optional &key (verbose nil))
    (let ((current-eff nil) (new-eff nil))
        (setf current-eff (run-analysis g :verbose verbose))
        (remove-node g node)
        (setf new-eff (run-analysis g :verbose verbose))
        (float (- new-eff current-eff))))

;; Retorna o caminho do nó origin até o nó dest dado o array de parentes bfs-tree
(defmethod path (origin dest bfs-tree)
    (let ((path nil) (current-node nil))
        (push dest path)
        (loop
            (setf current-node (first path))
            (when (equal current-node origin)
                (return path))
            (push (aref bfs-tree current-node) path))))

;; Run an analysis on the graph g, to get its metrics. If verbose it will print information about what metrics its
;; currently working on and the progress of the algorithm
(defmethod run-analysis ((g graph) &optional &key (verbose nil))
    (let ((avg-distance nil) (diameter nil) (total-distance nil) (progress nil) (max-num-paths nil) 
            (total-time nil) (unconnected nil) (x-nodes nil) (num-nodes nil) (node-ids nil) (type nil))
        (setf total-distance 0)
        (setf progress 0)
        (setf num-nodes (gethash "number-of-nodes" (properties g)))
        (setf max-num-paths (* num-nodes (1- num-nodes)))
        (when (= 2 (gethash "type" (properties g)))
            (setf max-num-paths (/ max-num-paths 2)))
        (setf total-time 0)
        (setf node-ids (gethash "node-ids" (properties g)))
        (setf x-nodes node-ids)
        (setf diameter 0)
        (setf type (gethash "type" (properties g)))
        (dotimes (n num-nodes)
            (let ((x nil) (y-nodes nil) (bfs-tree nil) (distance-array nil) (start-time nil) (end-time nil))
                (setf x (first x-nodes))
                (setf x-nodes (cdr x-nodes))
                (setf start-time (get-internal-real-time))
                (degree g x)
                (setf bfs-tree (bfs-search g x))
                (setf end-time (get-internal-real-time))
                (setf end-time (- end-time start-time))
                (incf total-time end-time)
                (incf progress)
                (setf distance-array (first bfs-tree))
                (setf bfs-tree (second bfs-tree))
                (when verbose
                    (print-progress progress num-nodes (/ total-time progress) :process-name "distância média e grau do nó"))
                (if (= 2 type)
                    (setf y-nodes (cdr x-nodes))
                    (setf y-nodes node-ids))
                (dolist (y y-nodes)
                    (let ((distance nil))
                        (when (not (equal x y))
                            (setf distance (distance g x y))
                            (when (= -1 distance)
                            (progn
                                    (when verbose
                                        (format t "Não foi possível chegar ao nó ~a a partir do nó ~a.~%" y x)
                                        (terpri))
                                    (return (setf unconnected t))))
                            (when (> distance diameter)
                                (setf diameter distance))
                            (incf total-distance distance))))
                (when unconnected
                    (return nil))))
        (if unconnected
            (progn
                (setf (gethash "altered" (properties g)) nil)
                nil)
            (progn 
                (setf (gethash "diameter" (properties g)) diameter) 
                (setf (gethash "average-distance" (properties g)) (float (/ total-distance max-num-paths)))
                (setf (gethash "average-efficiency" (properties g)) (float (/ 1 diameter)))
                (setf (gethash "altered" (properties g)) nil)
                t))))
