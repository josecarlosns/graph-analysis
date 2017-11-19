(load "graph.lisp")
(load "tgf-io.lisp")
(load "metrics.lisp")

(defvar graph (load-tgf "ipv6-2008-12-5374-9426.tgf" :g-type 2))
;; (defvar graph (load-tgf "arquivo-salvo.tgf" :g-type 2))
;; (defvar graph (load-tgf "tgf.tgf" :g-type 2))
;; (defvar graph (load-tgf "email-Eu-core.txt" :g-type 1 :nodes-first nil))
;; (defvar graph (load-tgf "facebook_combined.txt" :g-type 2 :nodes-first nil))
;; (defvar graph (random-graph 10 2 50))

(print-graph-info graph)
;; (print-graph-info graph :full-print t)
;; (save-tgf "arquivo-salvo.tgf" graph :nodes-first t)
(format t "Desidade do grafo: ~a" (get-density graph))
(terpri)
(format t "Grau esperado do grafo: ~a" (get-expt-degree graph t))
(terpri)
(format t "Distância média do grafo: ~a" (average-distance graph :verbose t))
(terpri)
;; (format t "Menor distância dos nós: ~a" (get-distances graph 0))
;; (terpri)
