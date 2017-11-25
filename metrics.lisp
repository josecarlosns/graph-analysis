(load "graph.lisp")
(load "utils.lisp")

;; Returns the adjacency list for the node
(defmethod adj-list ((g graph))
    (let ((adj-list nil) (g-type nil))
        (setf adj-list (make-array (gethash "number-of-nodes" (properties g)) :initial-element nil))
        (setf g-type (gethash "type" (properties g)))
        (dolist (edge (edges g))
            (let ((node1) (node2 nil))
                (setf node1 (first edge))
                (setf node2 (second edge))
                (push node2 (aref adj-list node1))
                (when (= 2 g-type)
                    (push node1 (aref adj-list node2)))))
        adj-list))
    
;; Sets and returns an pair (node1 Y) where:
;;     node1 -> the degree out of the node
;;     y -> the degree in of the node
(defmethod degree ((g graph) node)  
    (let ((out nil) (in nil) (adj-list nil) (number-of-nodes nil)) 
        (setf adj-list (gethash "adj-list" (properties g)))
        (setf out (list-length (aref adj-list node)))
        (setf in 0)
        (setf number-of-nodes (gethash "number-of-nodes" (properties g)))
        (if (= 2 (gethash "type" (properties g)))
            (setf in out)
            (dotimes (n number-of-nodes)
                (when (not (equal node n))
                    (dolist (node2 (aref adj-list n))
                        (if (= node node2)
                            (incf in))))))
        (list out in)))

;; Sets and returns the density of the graph
(defmethod density ((g graph))
    (let ((num-of-nodes nil) (num-of-edges nil))
        (setf num-of-nodes (gethash "number-of-nodes" (properties g)))
        (setf num-of-edges (gethash "number-of-edges" (properties g)))
        (if (= (gethash "type" (properties g)) 1)
            (float (/ num-of-edges (* (1- num-of-nodes) num-of-nodes)))
            (float (/ num-of-edges (/ (* (1- num-of-nodes) num-of-nodes) 2))))))

;; Returns the degree distribution (out in) of 'degree' for the graph
(defmethod degree-dist ((g graph) degree)
    (let ((out 0) (in 0) (number-of-nodes nil))
        (setf number-of-nodes (gethash "number-of-nodes" (properties g)))
        (dotimes (n number-of-nodes)
            (let ((node-degree nil))
                (setf node-degree (degree g n))
                (when (equal degree (first node-degree))
                    (incf out))
                (when (equal degree (second node-degree))
                    (incf in))))
        (setf out (float (/ out number-of-nodes)))
        (setf in (float (/ in number-of-nodes)))
        (list out in)))

;; Returns the expected degree of the graph as an pair (OUT IN) where:
;;     OUT -> The expected degree out of nodes.
;;     IN -> The expected degree in of nodes.
(defmethod expt-degree ((g graph))
    (let ((out 0) (in 0) (number-of-nodes nil))
        (setf number-of-nodes (gethash "number-of-nodes" (properties g)))
        (dotimes (n number-of-nodes)
            (let ((node-degree nil))
                (setf node-degree (degree g n))
                (incf out (first node-degree))
                (incf in (second node-degree))))
        (setf out (float (/ out number-of-nodes)))
        (setf in (float (/ in number-of-nodes)))
        (list out in)))

;; Sets and returns the distances array and the bfs-tree for the graph given the origin point as the node origin
(defmethod bfs-search ((g graph) origin)
    (let ((distances nil) (bfs-tree nil) (unvisited-nodes nil) (adj-list nil))
        (setf bfs-tree (make-array (gethash "number-of-nodes" (properties g)) :initial-element -1))
        (setf distances (make-array (gethash "number-of-nodes" (properties g)) :initial-element -1))
        (setf unvisited-nodes (make-queue :head nil :tail nil))
        (queue-put origin unvisited-nodes)
        (setf (aref bfs-tree origin) origin)
        (setf (aref distances origin) 0)
        (setf adj-list (gethash "adj-list" (properties g)))
        (loop
            (let ((current-node nil))
                (setf current-node (queue-pop unvisited-nodes))
                (dolist (neighbor-node (aref adj-list current-node))
                    (when (= -1 (aref bfs-tree neighbor-node))
                        (progn
                            (setf (aref bfs-tree neighbor-node) current-node)
                            (setf (aref distances neighbor-node) (1+ (aref distances current-node)))
                            (queue-put neighbor-node unvisited-nodes)))))
                (when (queue-empty unvisited-nodes)
                    (return (list distances bfs-tree))))))

;; Vulnerability: TODO
;; (defmethod vulnerability ((g graph) node &optional &key (verbose nil))
;;     (let ((current-eff nil) (new-eff nil))
;;         (setf current-eff (run-analysis g :verbose verbose))
;;         (remove-node g node)
;;         (setf new-eff (run-analysis g :verbose verbose))
;;         (float (- new-eff current-eff))))

;; Run an analysis on the graph g, to get its metrics. If verbose it will print information about what metrics its
;; currently working on and the progress of the algorithm
(defmethod run-analysis ((g graph) &optional &key (verbose nil))
    (let ((diameter nil) (total-distance nil) (progress nil) (max-num-paths nil) (total-time nil)
           (density nil) (expt-degree nil) (type nil) (num-nodes nil) (unconnected nil)) 
        (setf progress 0)
        (setf total-distance 0)
        (setf num-nodes (gethash "number-of-nodes" (properties g)))
        (setf type (gethash "type" (properties g)))
        (setf max-num-paths (* num-nodes (1- num-nodes)))
        (when (= 2 type)
            (setf max-num-paths (/ max-num-paths 2)))
        (setf total-time 0)
        (setf diameter 0)

        (setf density (density g))
        (setf expt-degree (expt-degree g))

        (dotimes (node1 num-nodes)
            (let ((bfs-tree nil) (distance-array nil) (start-time nil) (end-time nil))
                (setf start-time (get-internal-real-time))
                (setf bfs-tree (bfs-search g node1))
                (incf progress)
                (setf distance-array (first bfs-tree))
                (setf bfs-tree (second bfs-tree))
                (dotimes (y (if (= 2 type) (- num-nodes node1) num-nodes))
                    (let ((distance nil) (node2 nil))
                        (setf node2 (if (= 2 type) (+ y node1) y))
                        (when (not (equal node1 node2))
                            (setf distance (aref distance-array node2))
                            (when (= -1 distance)
                            (progn
                                (when verbose
                                    (format t "Não foi possível chegar ao nó ~a a partir do nó ~a.~%" y node1)
                                    (terpri))
                                (return (setf unconnected t))))
                            (when (> distance diameter)
                                (setf diameter distance))
                            (incf total-distance distance))))
                (setf end-time (get-internal-real-time))
                (setf end-time (- end-time start-time))
                (incf total-time end-time)
                (when verbose
                    (print-progress progress num-nodes (/ total-time progress) :process-name "distância média e grau do nó"))
                (when unconnected
                    (return nil))))
        (if unconnected
            nil
            (progn
                (setf total-distance (float (/ total-distance max-num-paths)))
                (setf (gethash "diameter" (properties g)) diameter) 
                (setf (gethash "average-distance" (properties g)) total-distance)
                (setf (gethash "average-efficiency" (properties g)) (float (/ 1 total-distance)))
                (setf (gethash "density" (properties g)) density)
                (setf (gethash "expected-degree" (properties g)) expt-degree)
                t))))
