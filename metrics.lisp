(load "graph.lisp")
(load "utils.lisp")

;; Returns the adjacency list for the node
(defmethod get-adj-list ((g graph) node)
    (gethash "edges" (gethash node (nodes g))))
    
;; Sets and returns an pair (X Y) where:
;;     x -> the degree out of the node
;;     y -> the degree in of the node
(defmethod get-degree ((g graph) node)
    (let ((degree nil))
        (setf degree (gethash "degree" (gethash node (nodes g))))
        (if degree
            degree
            (let ((ad-list nil))
                (setf degree (list 0 0))
                (setf ad-list (get-adj-list g node))
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
                (setf (gethash "degree" (gethash node (nodes g))) degree)))))

;; Sets and returns the density of the graph
(defmethod get-density ((g graph))
    (let ((density nil))
        (setf density (gethash "density" (properties g)))
        (if density
            density
            (let ((num-of-nodes nil) (num-of-edges nil))
                (setf num-of-nodes (gethash "number-of-nodes" (properties g)))
                (setf num-of-edges (gethash "number-of-edges" (properties g)))
                (if (= (gethash "type" (properties g)) 1)
                    (setf (gethash "density" (properties g)) (* 1.0 (/ num-of-edges (* (- num-of-nodes 1) num-of-nodes))))
                    (setf (gethash "density" (properties g)) (* 1.0 (/ num-of-edges (/ (* (- num-of-nodes 1) num-of-nodes) 2)))))))))

;; Returns the degree distribution (out, if inp) of 'degree' for the graph
(defmethod get-degree-dist ((g graph) degree inp)
    (let ((counter nil))
        (setf counter 0)
        (dolist (n (gethash "node-ids" (properties g)))
            (let ((node-degree nil))
                (setf node-degree (get-degree g n))
                (when (equal degree (if inp (second node-degree) (first node-degree)))
                    (setf counter (+ 1 counter)))))
        (* 1.0 (/ counter (list-length (nodes g))))))

;; Sets and returns the expected degree (out, if inp) for the graph
(defmethod get-expt-degree ((g graph) inp)
    (let ((exp-degree nil))
        (setf exp-degree (gethash "expected-degree" (properties g)))
        (if exp-degree
            exp-degree
            (let ((degree-sum nil) (deg-index nil))
                (setf degree-sum 0)
                (setf deg-index (if inp 0 1))
                (dolist (e (gethash "node-ids" (properties g)))
                    (let ((node-degree nil))
                        (setf node-degree (gethash "degree" (gethash e (nodes g))))
                        (setf degree-sum (+ degree-sum (nth deg-index node-degree)))))
                (setf (gethash "expected-degree" (properties g)) (* 1.0 (/ degree-sum (gethash "number-of-nodes" (properties g)))))))))

;; Sets and returns the distances array and the bfs-tree for the graph given the origin point as the node n1
(defmethod get-distances ((g graph) n1)
    (let ((distances nil) (bfs-tree nil))
        (setf distances (gethash "distances" (gethash n1 (nodes g))))
        (setf bfs-tree (gethash "bfs-tree" (gethash n1 (nodes g))))
        (if (and distances bfs-tree)
            (list distances bfs-tree)
            (let ((unvisited-nodes nil) (distance nil))
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
                                (return (list distances bfs-tree)))))))))

;; Retorna o caminho do nó origin até o nó dest dado o array de parentes bfs-tree
(defmethod get-path (origin dest bfs-tree)
    (let ((path nil) (current-node nil))
        (push dest path)
        (loop
            (setf current-node (first path))
            (when (equal current-node origin)
                (return path))
            (push (aref bfs-tree current-node) path))))

;; Retorna a distância média do grafo g. A distância média seria a média das menores distâncias entre os nós
;; do grafo sem considerar os pesos, ou seja, considerando apenas a distância em número de arestas ou nós.
;; Se quiser o custo médio do grafo, tente a função average-cost. Retorna nil se o grafo não for conexo
(defmethod average-distance ((g graph) &optional &key (verbose nil))
    (let ((avg-distance nil) (diameter nil))
        (setf avg-distance (gethash "average-distance" (properties g)))
        (setf diameter (gethash "diameter" (properties g)))
        (if (and avg-distance diameter)
            (list diameter avg-distance)
            (let ((total-distance nil) (progress nil) (max-num-paths nil) (total-time nil) (unconnected nil) (x-nodes nil) (num-nodes nil))
                (setf total-distance 0)
                (setf progress 0)
                (setf num-nodes (gethash "number-of-nodes" (properties g)))
                (setf max-num-paths (* num-nodes (1- num-nodes)))
                (when (= 2 (gethash "type" (properties g)))
                    (setf max-num-paths (/ max-num-paths 2)))
                (setf total-time 0)
                (setf x-nodes (gethash "node-ids" (properties g)))
                (setf diameter 0)
                (dotimes (n num-nodes)
                    (let ((x nil) (y-nodes nil) (bfs-tree nil) (distance-array nil) (start-time nil) (end-time nil))
                        (setf x (first x-nodes))
                        (setf x-nodes (cdr x-nodes))
                        (setf start-time (get-internal-real-time))
                        (setf bfs-tree (get-distances g x))
                        (setf end-time (get-internal-real-time))
                        (setf end-time (- end-time start-time))
                        (setf total-time (+ total-time end-time))
                        (incf progress)
                        (setf distance-array (first bfs-tree))
                        (setf bfs-tree (second bfs-tree))
                        (when verbose
                            (print-progress progress num-nodes (/ total-time progress) :process-name "cálculo do diâmetro+distância"))
                        (if (= 2 (gethash "type" (properties g)))
                            (setf y-nodes (cdr x-nodes))
                            (setf y-nodes (gethash "node-ids" (properties g))))
                        (dolist (y y-nodes)
                            (let ((distance nil))
                                (when (not (equal x y))
                                    (setf distance (aref distance-array y))
                                    (when (= -1 distance)
                                    (progn
                                            (when verbose
                                                (format t "Não foi possível chegar ao nó ~a a partir do nó ~a.~%" y x)
                                                (terpri))
                                            (return (setf unconnected t))))
                                    (when (> distance diameter)
                                        (setf diameter distance))
                                    (setf total-distance (+ total-distance distance)))))
                        (when unconnected
                            (return nil))))
                (if unconnected
                    nil
                    (list 
                        (setf (gethash "diameter" (properties g)) diameter) 
                        (setf (gethash "average-distance" (properties g)) (float (/ total-distance max-num-paths)))))))))
