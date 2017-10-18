
(defclass graph ()
    (
        (gtype :accessor gtype
            :initform 1
            :initarg :gtype)

        (node :accessor nodes
            :initform '()
            :initarg :nodes)

        (edge :accessor edges
            :initform '((indexnodeout indexnodein weight))
            :initarg :edges)
    )
)

    ;; "Cria um grafo aleatório com n vértices. 
    ;; Os tipos são 1-Direcionado e 2-Não direcionado. 
    ;; Os pesos (opcional) são randômicos entre 0 e weight, incluso. 
    ;; P é a probabilidade (0 a 100%) de uma aresta existir ou não"
(defmethod random-graph (n type p &optional (weight nil))
    (setf *random-state* (make-random-state t))
    (let ((g (make-instance 'graph :gtype type :nodes (make-list n) :edges '())))
        (loop for i in (nodes g) and index from 0 do
            (setf (nth index (nodes g)) (+ index 1))
        )
        (dolist (i (nodes g))
            (dolist (j (nodes g))
                (if (and (not (eq i j)) (<= (random 101) p))
                    (let ((w) (edge))
                        (if (not (null weight))
                            (setf w (random (+ 1 weight)))
                            (setf w nil)
                        )
                        (if (= type 1)
                            (progn
                                (setf edge (list i j w))
                                (push edge (edges g))
                            )
                            (if (> j i)
                                (progn
                                    (setf edge (list i j w))
                                    (push edge (edges g))
                                )
                            )
                        )   
                    )
                )
            )
        )
        g
    )
)

    ;; "Retorna a lista de adjacência do grafo"
(defmethod get-adj-list ((g graph))
    (let ((adj-list (make-list (list-length (nodes g)))))
        (dolist (edge (edges g))
            (push (rest edge) (nth (- (first edge) 1) adj-list))
            (if (= 2 (gtype g))
                (let ((edge2 (list (first edge) (third edge))))
                    (push edge2 (nth (- (second edge) 1) adj-list))
                )
            )
        )
        adj-list
    )
)

    ;; "Retorna uma lista com o grau do nó node pertencente ao grafo g, no formato (gOUT, gin).
    ;; gOUT - Grau de saída
    ;; gIN - Grau de entrada"
(defmethod get-degree ((g graph) node)
    (let ((degree  (list 0 0)))
        (loop for edge-list in (get-adj-list g) and i from 1 do
            (if (= node i)
                (let ((count 0))
                    (dolist (edge edge-list)
                        (setf count (+ 1 count))
                    )
                    (setf (first degree) count)
                )
                (let ((count (second degree)))
                    (dolist (edge edge-list)
                        (if (= (first edge) node)
                            (setf count (+ 1 count))
                        )
                    )
                    (setf (second degree) count)
                )                    
            )
        )
    degree
    )
)

    ;; "Retorna uma lista com os graus dos nós do grafo g"
(defmethod get-degree-list ((g graph))
    (let ((degree-list (make-list (list-length (nodes g)))))
        (loop for n in (nodes g) and i from 0 do
            (setf (nth i degree-list) (get-degree g n))
        )
        degree-list
    )
)

    ;; "Retorna a densidade do grafo g"
(defmethod get-graph-density ((g graph))
    (let ((size (list-length (nodes g))))
        (if (= (gtype g) 1)
            (/ (list-length (edges g)) (* (- size 1) size))
            (/ (list-length (edges g)) (/ (* (- size 1) size) 2))
        )
    )
)
