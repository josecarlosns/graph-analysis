(load "graph.lisp")
(load "utils.lisp")

;; "Retorna a lista de adjacência do grafo"
(defmethod get-adj-list ((g graph))
    (if (adj-list g)
        (adj-list g)
        (let ((ad-list))
            (setf ad-list (make-list (list-length (nodes g))))
            (dolist (edge (edges g))
                (push (rest edge) (nth (first edge) ad-list))
                (if (= 2 (g-type g))
                    (let ((edge2 (list (first edge))))
                        (push edge2 (nth (second edge) ad-list)))))
            (setf (adj-list g) ad-list))))

;; "Retorna uma lista com o grau do nó node pertencente ao grafo g, no formato (gOUT, gin).
;; gOUT - Grau de saída
;; gIN - Grau de entrada"
(defmethod get-degree ((g graph) node)
    (let ((degree) (ad-list))
        (setf degree (list 0 0))
        (setf ad-list (get-adj-list g))
        (setf (first degree) (list-length (nth node ad-list)))
        (if (= 2 (g-type g))
            (progn
                (setf (second degree) (first degree))
                degree)
            (progn 
                (loop for edge-list in ad-list and i from 0 do
                    (if (= node i)
                        (setf (first degree) (list-length edge-list))
                        (let ((count))
                            (setf count (second degree))
                            (dolist (edge edge-list)
                                (if (= (first edge) node)
                                    (setf count (+ 1 count))))
                            (setf (second degree) count))))
                degree))))

;; "Retorna uma lista com os graus dos nós do grafo g"
(defmethod get-degree-list ((g graph))
    (if (degree-list g)
        (degree-list g)
        (let ((d-list))
            (setf d-list (make-list (list-length (nodes g))))
            (loop for n in (nodes g) and i from 0 do
                (setf (nth i d-list) (get-degree g n)))
            (setf (degree-list g) d-list))))

;; "Retorna a densidade do grafo g"
(defmethod get-density ((g graph))
    (if (density g)
        (density g)
        (let ((size))
            (setf size (list-length (nodes g)))
            (if (= (g-type g) 1)
                (setf (density g) (* 1.0 (/ (list-length (edges g)) (* (- size 1) size))))
                (setf (density g) (* 1.0 (/ (list-length (edges g)) (/ (* (- size 1) size) 2))))))))

;; Função que retorna a distribuição de um grau (de entrada, se inp = t) em um grafo
(defmethod get-degree-dist ((g graph) degree inp)
    (if (degree-dist g)
        (degree-dist g)
        (let ((counter))
            (setf counter 0)
            (dolist (e (get-degree-list g))
                (when (equal degree (if inp (second e) (first e)))
                    (setf counter (+ 1 counter))))
            (setf (degree-dist g) (* 1.0 (/ counter (list-length (nodes g))))))))

;; Retorna o grau (de entrada, caso inp = t) esperado do grafo
(defmethod get-expt-degree ((g graph) inp)
    (if (expt-degree g)
        (expt-degree g)
        (let ((degree-sum) (deg-index))
            (setf degree-sum 0)
            (setf deg-index (if inp 0 1))
            (dolist (e (get-degree-list g))
                (setf degree-sum (+ degree-sum (nth deg-index e))))
            (setf (expt-degree g) (* 1.0 (/ degree-sum (list-length (nodes g))))))))

;; Calcula as distâncias e nós-parentes dado o nó origem n1
;; Retorna um par (x y) onde:
;;     x = um array onde cara nó do grafo representa um índice de uma posição, e cada posição possui o nó parente
;;     com a menor distância da origem.
;;     y = um array como x, só que ao invés dos parentes possui a distância, em arestas, até a origem.
(defmethod get-distances ((g graph) n1)
    (let ((parent nil) (unvisited-nodes nil) (distance nil))
        (setf parent (make-array (list-length (nodes g)) :initial-element -1))
        (setf distance (make-array (list-length (nodes g)) :initial-element -1))
        (setf unvisited-nodes (make-queue :head nil :tail nil))
        (queue-put n1 unvisited-nodes)
        (setf (aref parent n1) n1)
        (setf (aref distance n1) 0)
        (loop
            (let ((current-node nil))
                (setf current-node (queue-pop unvisited-nodes))
                (dolist (e (nth current-node (get-adj-list g)))
                    (let ((neighbor-node nil))
                        (setf neighbor-node (first e))
                        (when (= -1 (aref parent neighbor-node))
                                (progn
                                    (setf (aref parent neighbor-node) current-node)
                                    (setf (aref distance neighbor-node) (1+ (aref distance current-node)))
                                    (queue-put neighbor-node unvisited-nodes))))))
                (when (queue-empty unvisited-nodes)
                    (return (list parent distance))))))

;; Retorna a distância média do grafo g. A distância média seria a média das menores distâncias entre os nós
;; do grafo sem considerar os pesos, ou seja, considerando apenas a distância em número de arestas ou nós.
;; Se quiser o custo médio do grafo, tente a função average-cost. Retorna nil se o grafo não for conexo
(defmethod average-distance ((g graph) &optional &key (verbose nil))
    (let ((total-distance nil) (progress nil) (max-num-paths nil) (total-time nil) (etl nil) (unconnected nil) (x-nodes nil) (node-num nil) (diameter nil))
        (setf total-distance 0)
        (setf progress 0)
        (setf max-num-paths (list-length (nodes g)))
        (setf max-num-paths (* max-num-paths (1- max-num-paths)))
        (when (= 2 (g-type g))
            (setf max-num-paths (/ max-num-paths 2)))
        (setf total-time 0)
        (setf x-nodes (nodes g))
        (setf node-num (list-length (nodes g)))
        (setf diameter 0)
        (dotimes (n node-num)
            (let ((x nil) (y-nodes nil) (parent-array nil) (distance-array nil) (start-time nil) (end-time nil))
                (setf x (first x-nodes))
                (setf x-nodes (cdr x-nodes))
                (setf start-time (get-internal-real-time))
                (setf parent-array (get-distances g x))
                (setf end-time (get-internal-real-time))
                (setf end-time (- end-time start-time))
                (setf total-time (+ total-time end-time))
                (incf progress)
                (setf distance-array (second parent-array))
                (setf parent-array (first parent-array))
                (when verbose
                    (format t "~%Progresso do cálculo da distância+eficiência média: ~,5f%~%" (* 100.0 (/ progress node-num)))
                    (format t "Tempo decorrido em HH:MM:SS: ~d:~2,'0d:~2,'0d~%" (floor (/ total-time 3600000)) (rem (floor (/ total-time 60000)) 60) (rem (floor (/ total-time 1000)) 60))
                    (setf etl (* (- node-num progress) (/ total-time progress)))
                    (format t "Tempo restante estimado em HH:MM:SS: ~d:~2,'0d:~2,'0d~%" (floor (/ etl 3600000)) (rem (floor (/ etl 60000)) 60) (rem (floor (/ etl 1000)) 60))
                    (terpri))
                (if (= 2 (g-type g))
                    (setf y-nodes (cdr x-nodes))
                    (setf y-nodes (nodes g)))
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
                            (if (> distance diameter)
                                (setf diameter distance))
                            (setf total-distance (+ total-distance distance)))))
                (when unconnected
                    (return nil))))
         (if unconnected
            nil
            (list diameter (float (/ total-distance max-num-paths))))))
