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


;; Returns an array with the outcoming (and another with the incoming, if the graph is directed) degrees of
;; each node of the graph
(defmethod degrees ((g graph) &optional &key (verbose nil))
    (let ((out-array nil) (in-array nil) (adj-list nil) (number-of-nodes nil) (total-time 0) (type nil))
        (setf number-of-nodes (gethash "number-of-nodes" (properties g)))
        (setf adj-list (gethash "adj-list" (properties g)))
        (setf type (gethash "type" (properties g)))
        (setf out-array (make-array number-of-nodes :initial-element 0))
        (when (= 1 type)
            (setf in-array (make-array number-of-nodes :initial-element 0)))
        (when verbose
            (progn
                (terpri)
                (format t "Calculating degrees...")
                (terpri)))
        (dotimes (node1 number-of-nodes)
            (let ((start-time nil) (end-time nil) (adj-nodes nil))
                (setf start-time (get-internal-real-time))
                (setf adj-nodes (aref adj-list node1))
                (setf (aref out-array node1) (list-length adj-nodes))
                (when (= 1 type)
                    (dolist (node2 adj-nodes)
                        (incf (aref in-array node2))))
                (setf end-time (get-internal-real-time))
                (decf end-time start-time)
                (incf total-time end-time)
                (when verbose
                    (print-progress (1+ node1) number-of-nodes (/ total-time (1+ node1))))))
        (when verbose
            (progn
                (terpri)
                (format t "Done!")
                (terpri)))
        (if (= 1 type)
            (list out-array in-array)
            out-array)))


;; Returns an list (x y z) where:
;;     x -> the expected degree of the graph, (OUTCOMING INCOMING) if directed, OUTCOMING if otherwise.
;;     y -> an array with the distribution of all outcoming degrees.
;;     z -> like y, but with incoming degrees, only present if graph is directed.
(defmethod degree-dist ((g graph) &optional &key (verbose nil))
    (let ((degrees-out nil) (degrees-in nil) (degree-dist-out nil) (degree-dist-in nil) 
            (number-of-nodes nil) (type nil) (total-time 0) (expt-degree nil))
        (setf number-of-nodes (gethash "number-of-nodes" (properties g)))
        (setf degrees-out (degrees g :verbose verbose))
        (setf type (gethash "type" (properties g)))
        (setf degree-dist-out (make-array number-of-nodes :initial-element 0))
        (setf expt-degree 0)
        (when (= 1 type)
            (progn
                (setf expt-degree (list 0 0))
                (setf degrees-in (second degrees-out))
                (setf degrees-out (first degrees-out))
                (setf degree-dist-in (make-array number-of-nodes :initial-element 0))))
        (when verbose
            (progn
                (terpri)
                (format t "Calculating degree distribution:")
                (terpri)))
        (dotimes (i number-of-nodes)
            (let ((out nil) (in nil) (start-time nil) (end-time nil))
                (setf start-time (get-internal-real-time))
                (setf out (aref degrees-out i))
                (if (= 1 type)
                    (progn
                        (setf in (aref degrees-in i))
                        (incf (aref degree-dist-in in))
                        (incf (first expt-degree) out)
                        (incf (second expt-degree) in))
                    (incf expt-degree out))
                (incf (aref degree-dist-out out))
                (setf end-time (get-internal-real-time))
                (setf end-time (decf end-time start-time))
                (incf total-time end-time)
                (when verbose
                    (print-progress i (* 2 number-of-nodes) (/ total-time (1+ i))))))
        (dotimes (i number-of-nodes)
            (let ((start-time nil) (end-time nil))
                (setf start-time (get-internal-real-time))
                (setf (aref degree-dist-out i) (/ (aref degree-dist-out i) number-of-nodes))
                (when (= 1 type)
                    (setf (aref degree-dist-in i) (/ (aref degree-dist-in i) number-of-nodes)))
                (setf end-time (get-internal-real-time))
                (setf end-time (decf end-time start-time))
                (incf total-time end-time))
                (when verbose
                    (print-progress (+ number-of-nodes (1+ i)) (* 2 number-of-nodes) (/ total-time (+ number-of-nodes (1+ i))))))
        (when verbose
            (progn
                (terpri)
                (format t "Done!")
                (terpri)))
        (if (= 1 type)
            (progn
                (setf (first expt-degree) (/ (first expt-degree) number-of-nodes))
                (setf (second expt-degree) (/ (second expt-degree) number-of-nodes))
                (list expt-degree degree-dist-out degree-dist-in))
            (list (/ expt-degree number-of-nodes) degree-dist-out))))

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
                (terpri)
                (dotimes (n 50)
                    (princ "#"))
                (terpri)
                (format t "Analysis starting...~%")
                (terpri)))

        (setf density (density g))
        (setf degree-dist (degree-dist g :verbose verbose))
        (setf expt-degree (first degree-dist))
        (setf degree-dist (if (= 1 type) (cdr degree-dist) (second degree-dist)))

        (when verbose
            (progn
                (terpri)
                (format t "Calculating average distance and efficiency...")
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
                                        (terpri)
                                        (format t "~%The graph is unconnected, stopping analysis...")
                                        (terpri))
                                    (return (setf unconnected t))))
                            (when (> distance diameter)
                                (setf diameter distance))
                            (incf total-distance distance))))
                (when unconnected
                    (return nil))
                (setf end-time (get-internal-real-time))
                (setf end-time (- end-time start-time))
                (incf total-time end-time)
                (when verbose
                    (progn
                        (print-progress progress num-nodes (/ total-time progress))))))
        (if unconnected
            nil
            (progn
                (when verbose
                    (progn
                        (terpri)
                        (format t "Analysis complete!")
                        (terpri)
                        (dotimes (n 50)
                            (princ "#"))
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