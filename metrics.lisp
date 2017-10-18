(load "graph.lisp")

;; "Retorna a lista de adjacência do grafo"
(defmethod adj-list ((g graph))
    (let ((adj-list (make-list (list-length (nodes g)))))
        (dolist (edge (edges g))
            (push (rest edge) (nth (first edge) adj-list))
            (if (= 2 (g-type g))
                (let ((edge2 (list (first edge))))
                    (push edge2 (nth (second edge) adj-list)))))
            adj-list))

;; "Retorna uma lista com o grau do nó node pertencente ao grafo g, no formato (gOUT, gin).
;; gOUT - Grau de saída
;; gIN - Grau de entrada"
(defmethod degree ((g graph) node)
    (let ((degree) (ad-list))
        (setf degree (list 0 0))
        (setf ad-list (adj-list g))
        (setf (first degree) (list-length (nth node ad-list)))
        (if (= 2 (g-type g))
            (progn
                (setf (second degree) (first degree))
                degree)
            (progn 
                (loop for edge-list in (ad-list) and i from 0 do
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
(defmethod degree-list ((g graph))
    (let ((degree-list))
        (setf degree-list (make-list (list-length (nodes g))))
        (loop for n in (nodes g) and i from 0 do
            (setf (nth i degree-list) (degree g n)))
        degree-list))

    ;; "Retorna a densidade do grafo g"
(defmethod graph-density ((g graph))
    (let ((size))
        (setf size (list-length (nodes g)))
        (if (= (g-type g) 1)
            (* 1.0 (/ (list-length (edges g)) (* (- size 1) size)))
            (* 1.0 (/ (list-length (edges g)) (/ (* (- size 1) size) 2))))))
