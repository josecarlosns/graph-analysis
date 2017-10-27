(load "graph.lisp")
(load "tgf-io.lisp")
(load "metrics.lisp")

(defvar graph (load-tgf "ipv6-2008-12-5374-9426.tgf" :g-type 2))
;; (defvar graph (random-graph 5500 2 50))

(print-graph-info graph :full-print t)
(format t "Desidade do grafo: ~a" (get-density graph))
(terpri)
(format t "Grau esperado do grafo: ~a" (get-expt-degree graph t))
(terpri)
