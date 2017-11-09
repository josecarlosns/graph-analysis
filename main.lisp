(load "graph.lisp")
(load "tgf-io.lisp")
(load "metrics.lisp")

(defvar graph (load-tgf "test.tgf" :g-type 2))
;; (defvar graph (random-graph 5500 2 10))

(print-graph-info graph :full-print t)
(format t "Desidade do grafo: ~a" (get-density graph))
(terpri)
(format t "Grau esperado do grafo: ~a" (get-expt-degree graph t))
(terpri)
(format t "Menor distância entre ~a e ~a: ~a" 0 6 (shortest-path graph 6 0))
(terpri)
