(load "graph.lisp")

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
        (setf (first degree) (list-length (nth (first node) ad-list)))
        (if (= 2 (g-type g))
            (progn
                (setf (second degree) (first degree))
                degree)
            (progn 
                (loop for edge-list in ad-list and i from 0 do
                    (if (= (first node) i)
                        (setf (first degree) (list-length edge-list))
                        (let ((count))
                            (setf count (second degree))
                            (dolist (edge edge-list)
                                (if (= (first edge) (first node))
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
