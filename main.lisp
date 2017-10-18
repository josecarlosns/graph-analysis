(load "graph.lisp")
(load "tgf-io.lisp")
(load "metrics.lisp")

(defvar graph (load-tgf "ipv6-2008-12-5374-9426.tgf" :g-type 2))

(format t "~a" (get-graph-density graph))
(terpri)
