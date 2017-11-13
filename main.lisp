(load "graph.lisp")
(load "tgf-io.lisp")
(load "metrics.lisp")

;; (defvar graph (load-tgf "ipv6-2008-12-5374-9426.tgf" :g-type 2))
(defvar graph (load-tgf "test.tgf" :g-type 2))
;; (defvar graph (random-graph 10 2 0.1))

(print-graph-info graph :full-print t)
(format t "Desidade do grafo: ~a" (get-density graph))
(terpri)
(format t "Lista de adj: ~a" (get-adj-list graph))
(terpri)
;; (format t "Grau esperado do grafo: ~a" (get-expt-degree graph t))
;; (terpri)
(format t "Menor distância entre ~a e ~a: ~a" 0 6 (shortest-path graph 0 10))
(terpri)
