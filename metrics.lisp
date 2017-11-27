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

;; Sets and returns the density of the graph
(defmethod density ((g graph))
    (let ((num-of-nodes nil) (num-of-edges nil))
        (setf num-of-nodes (gethash "number-of-nodes" (properties g)))
        (setf num-of-edges (gethash "number-of-edges" (properties g)))
        (if (= (gethash "type" (properties g)) 1)
            (float (/ num-of-edges (* (1- num-of-nodes) num-of-nodes)))
            (float (/ num-of-edges (/ (* (1- num-of-nodes) num-of-nodes) 2))))))


;; Returns an array where each element e(x)= (a b) where:
;;     x -> An node
;;     a -> The outcoming degree of x
;;     b -> The incoming degree of x
(defmethod degrees ((g graph))
    (let ((degree-array nil) (adj-list nil) (number-of-nodes nil))
        (setf number-of-nodes (gethash "number-of-nodes" (properties g)))
        (setf degree-array (make-array number-of-nodes :initial-element nil))
        (setf adj-list (gethash "adj-list" (properties g)))
        (dotimes (node1 number-of-nodes)
            (let ((out 0))
                (dolist (edge (aref adj-list node1))
                    (incf out)
                    (let ((node2 nil))
                        (setf node2 (second edge))
                        (when (null (aref degree-array node2))
                            (setf (aref degree-array node2) (list 0 0)))
                        (incf (second (aref degree-array node2)))))
                (when (null (aref degree-array node1))
                    (setf (aref degree-array node1) list 0 0))
                (setf (first (aref degree-array node1)) out)))
        degree-array))

;; Returns an list (d(0) d(1) d(2) ... d(n)) where d is an trio (x y z) where:
;;     z -> an degree
;;     x -> probability of an random node to have the outcoming degree of z.
;;     y -> probability of an random node to have the incoming degree of z.
;;     Obs.: the list does not stores an trio where x and y are equal to 0.
(defmethod degree-dist ((g graph) &optional &key (verbose nil))
    (let ((degrees nil) (out-sum 0) (in-sum 0) (current-degree 0) (number-of-nodes nil) (total-time 0))
        (setf number-of-nodes (gethash "number-of-nodes" (properties g)))
        (when verbose
            (progn
                (format t "Calculating degree distribution:")
                (terpri))
        (setf degrees (degrees g))
        (loop
            (let ((degree nil) (out nil) (in nil) (start-time nil) (end-time nil))
                (setf start-time (get-internal-real-time))
                (setf degree (degree-prob g current-degree))
                (setf out (first degree))
                (setf in (second degree))
                (when (or (> out 0) (> in 0))
                    (progn
                        (incf out-sum out)
                        (incf in-sum in)
                        (push (list current-degree out in) degrees)))
                (incf current-degree)
                (setf end-time (get-internal-real-time))
                (setf end-time (decf end-time start-time))
                (incf total-time end-time)
                (when verbose
                    (print-progress current-degree number-of-nodes (/ total-time current-degree))))
            (when (> current-degree number-of-nodes)
                (progn
                    (when verbose
                        (progn
                            (format t "~%Done!")
                            (terpri)))
                    (return (reverse degrees))))))))

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
        (setf out (/ out number-of-nodes))
        (setf in (/ in number-of-nodes))
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
           (density nil) (expt-degree nil) (type nil) (num-nodes nil) (unconnected nil) (degree-dist nil)) 
        (setf progress 0)
        (setf total-distance 0)
        (setf num-nodes (gethash "number-of-nodes" (properties g)))
        (setf type (gethash "type" (properties g)))
        (setf max-num-paths (* num-nodes (1- num-nodes)))
        (when (= 2 type)
            (setf max-num-paths (/ max-num-paths 2)))
        (setf total-time 0)
        (setf diameter 0)

        (when verbose
            (progn
                (format t "Analysis starting...")
                (terpri)))

        (setf density (density g))
        (setf expt-degree (expt-degree g))
        (setf degree-dist (degree-dist g :verbose verbose))

        (when verbose
            (progn
                (format t "Calculating average distance...")
                (terpri)))
        
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
                                    (format t "~%The graph is unconnected, stopping analysis...")
                                    (terpri))
                                (return (setf unconnected t))))
                            (when (> distance diameter)
                                (setf diameter distance))
                            (incf total-distance distance))))
                (setf end-time (get-internal-real-time))
                (setf end-time (- end-time start-time))
                (incf total-time end-time)
                (when verbose
                    (progn
                        (print-progress progress num-nodes (/ total-time progress))))
                (when unconnected
                    (return nil))))
        (if unconnected
            nil
            (progn
                (when verbose
                    (progn
                        (format t "~%Analysis complete!")
                        (terpri)))
                (setf total-distance (/ total-distance max-num-paths))
                (setf (gethash "diameter" (properties g)) diameter) 
                (setf (gethash "average-distance" (properties g)) total-distance)
                (setf (gethash "average-efficiency" (properties g)) (/ 1 total-distance))
                (setf (gethash "density" (properties g)) density)
                (setf (gethash "expected-degree" (properties g)) expt-degree)
                (setf (gethash "degree-dist" (properties g)) degree-dist)
                t))))

(defmethod average-diameter ((g graph)))