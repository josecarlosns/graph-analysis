(load "graph.lisp")
(load "tgf-io.lisp")
(load "metrics.lisp")

(let ((option nil))
    (loop
        (format t "What do you want to do?
            1- Load and analyse an graph from disk.
            2- Generate an random graph and analyse it.
            0- Exit~%")
        (finish-output)
        (setf option (read))
        (if (numberp option)
            (if (= 0 option)
                (return nil)
                (if (= 1 option)
                    (progn
                        (let ((graph nil) (archive nil) (graph-type nil) (nodes-first nil) (connected nil))
                            (format t "Type the name of the archive (must be in current directory):~%")
                            (finish-output)
                            (setf archive (read-line))
                            (loop
                                (format t "Type of graph:~t1- Directed~t2- Undirected~%")
                                (setf graph-type (read))
                                (if (and (numberp graph-type) (or (= 1 graph-type) (= 2 graph-type)))
                                    (return nil)
                                    (format t "Invalid input!~%")))
                            (format t "Node ids first?~%")
                            (finish-output)
                            (setf nodes-first (read))
                            (setf archive (merge-pathnames archive))
                            (setf graph (load-tgf archive :g-type graph-type :nodes-first nodes-first))
                            (setf connected (run-analysis graph :verbose t))
                            (print-graph-info graph :verbose connected)))                    
                    (let ((graph nil) (number-of-nodes nil) (graph-type nil) (edge-prob nil) (connected nil))
                        (loop 
                            (format t "Number of nodes:~%")
                            (finish-output)
                            (setf number-of-nodes (read))
                            (if (and (numberp number-of-nodes) (> number-of-nodes 0))
                                (return nil)
                                (format t "Invalid input!~%")))
                        (loop 
                            (format t "Type of graph: ~t1- Directed ~t2- Undirected~%")
                            (finish-output)
                            (setf graph-type (read))
                            (if (and (numberp number-of-nodes) (or (= 1 graph-type) (= 2 graph-type)))
                                (return nil)
                                (format t "Invalid input!~%")))
                        (loop 
                            (format t "Probability of link (float between 0 and 100):~%")
                            (finish-output)
                            (setf edge-prob (read))
                            (if (and (numberp number-of-nodes) (or (>= edge-prob 0) (<= edge-prob 100)) )
                                (return nil)
                                (format t "Invalid input!~%")))
                        (setf graph (random-graph number-of-nodes graph-type edge-prob :verbose t))
                            (setf connected (run-analysis graph :verbose t))
                            (print-graph-info graph :verbose connected))))
            (format t "Invalid input!~%"))))
