
(defclass graph ()
    (
        (g-type :accessor g-type
            :initform 1
            :initarg :g-type)
        (node :accessor nodes
            :initform nil
            :initarg :nodes)
        (edge :accessor edges
            :initform nil
            :initarg :edges)
        (adj-list :accessor adj-list
            :initform nil
            :initarg :adj-list)
        (degree-list :accessor degree-list
            :initform nil
            :initarg :degree-list)
        (density :accessor density
            :initform nil
            :initarg :density)
        (expt-degree :accessor expt-degree
            :initform nil
            :initarg :expt-degree)
        (degree-dist :accessor degree-dist
            :initform nil
            :initarg :degree-dist)))

;; "Cria um grafo aleatório com n vértices. 
;; Os tipos são 1-Direcionado e 2-Não direcionado. 
;; Os pesos (opcional) são randômicos entre 0 e weight, incluso. 
;; P é a probabilidade (0 a 100%) de uma aresta existir ou não"
(defmethod random-graph (n type p &optional (weight nil))
    (setf *random-state* (make-random-state t))
    (let ((g))
        (setf g (make-instance 'graph :g-type type :nodes '() :edges '()))
        (dotimes (i n)
            (push (list i) (nodes g)))
        (dolist (i (mapcar #'first (nodes g)))
            (dolist (j (mapcar #'first (nodes g)))
                (if (and (not (eq i j)) (<= (random 101) p))
                    (let ((w) (edge))
                        (if (not (null weight))
                            (setf w (random (+ 1 weight)))
                            (setf w nil))
                        (if (= type 1)
                            (progn
                                (setf edge (list i j w))
                                (push edge (edges g)))
                            (if (> j i)
                                (progn
                                    (setf edge (list i j w))
                                    (push edge (edges g)))))))))
        g))

(defmethod print-graph-info ((g graph) &optional &key (full-print nil))
    (format t "Nº de nós do grafo: ~a~%Nº de arestas do grafo: ~a~%" 
        (list-length (nodes g)) (list-length (edges g)))
    (when full-print
        (format t "Nós do grafo:~%~a~%Arestas do grafo:~%~a" (nodes g) (edges g))))
        