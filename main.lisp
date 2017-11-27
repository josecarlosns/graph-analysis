(load "graph.lisp")
(load "tgf-io.lisp")
(load "metrics.lisp")

(let ((option nil))
    (loop
        (format t "Digite o número da operação: 
            1- Carregar e analisar um grafo a partir de um arquivo.
            2- Gerar um grafo aleatório e analisá-lo.
            0- Sair~%")
        (finish-output)
        (setf option (read))
        (if (numberp option)
            (if (= 0 option)
                (return nil)
                (if (= 1 option)
                    (progn
                        (let ((graph nil) (archive nil) (graph-type nil) (nodes-first nil) (connected nil))
                            (format t "Digite o nome do arquivo a ser carregado:~%")
                            (finish-output)
                            (setf archive (read-line))
                            (loop
                                (format t "Digite o tipo de grafo:
                                    1- Direcionado
                                    2- Não-direcionado~%")
                                (setf graph-type (read))
                                (if (and (numberp graph-type) (or (= 1 graph-type) (= 2 graph-type)))
                                    (return nil)
                                    (format t "Operação inválida!~%")))
                            (format t "O arquivo identifica os nós?~%")
                            (finish-output)
                            (setf nodes-first (read))
                            (setf archive (merge-pathnames archive))
                            (setf graph (load-tgf archive :g-type graph-type :nodes-first nodes-first))
                            (setf connected (run-analysis graph :verbose t))
                            (print-graph-info graph :verbose connected)))                    
                    (let ((graph nil) (number-of-nodes nil) (graph-type nil) (edge-prob nil) (connected nil))
                        (loop 
                            (format t "Digite o número de nós do grafo:~%")
                            (finish-output)
                            (setf number-of-nodes (read))
                            (if (and (numberp number-of-nodes) (> number-of-nodes 0))
                                (return nil)
                                (format t "Digite um número válido!~%")))
                        (loop 
                            (format t "Digite o tipo de grafo:
                                1- Direcionado
                                2- Não direcionado~%")
                            (finish-output)
                            (setf graph-type (read))
                            (if (and (numberp number-of-nodes) (or (= 1 graph-type) (= 2 graph-type)))
                                (return nil)
                                (format t "Digite uma opção válida!~%")))
                        (loop 
                            (format t "Digite a probabilidade de existência de um nó (ponto-flutuante de 0 a 100):")
                            (finish-output)
                            (setf edge-prob (read))
                            (if (and (numberp number-of-nodes) (or (>= edge-prob 0) (<= edge-prob 100)) )
                                (return nil)
                                (format t "Digite uma opção válida!~%")))
                        (setf graph (random-graph number-of-nodes graph-type edge-prob :verbose t))
                            (setf connected (run-analysis graph :verbose t))
                            (print-graph-info graph :verbose connected))))
            (format t "Operação inválida!~%"))))