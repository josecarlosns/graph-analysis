(load "graph.lisp")
(load "utils.lisp")
(load "metrics.lisp")

(defun load-tgf (file-name &optional &key (g-type 1) (nodes-first t) (weighted nil) (separator " "))
    (let ((in nil) (graph nil) (current-list nil))
        (setf in (open file-name :if-does-not-exist nil))
        (setf graph (empty-graph g-type weighted))
        (setf current-list 1)
        (when in
            (loop for line = (read-line in nil)
                while line do 
                    (progn
                        (let ((e nil))  
                            (when (not (string= (char line 0) "#"))
                                (setf e (split-str line separator))
                                (if nodes-first      
                                    (progn
                                        (when (> (list-length e) 1)
                                            (setf current-list 2))
                                        (if (= current-list 1)
                                            (incf (gethash "number-of-nodes" (properties graph)))
                                            (let ((node1 nil) (node2 nil) (edge nil))
                                                (setf edge (mapcar #'parse-integer e))
                                                (loop for element in edge and i from 0 do
                                                    (case i
                                                        (0 (setf node1 element))
                                                        (1 (setf node2 element))))
                                                (when (not (nodep graph node1))
                                                    (add-node graph))
                                                (when (not (nodep graph node2))
                                                    (add-node graph))
                                                (setf edge (list node1 node2))
                                                (add-edge graph edge))))
                                    (let ((node1 nil) (node2 nil) (edge nil))
                                        (setf edge (mapcar #'parse-integer e))
                                        (loop for element in edge and i from 0 do
                                            (case i
                                                (0 (setf node1 element))
                                                (1 (setf node2 element))))
                                        (setf edge (list node1 node2))
                                        (when (not (nodep graph node1))
                                            (add-node graph))
                                        (when (not (nodep graph node2))
                                            (add-node graph))
                                        (add-edge graph edge)))))))
            (close in))
        (setf (gethash "adj-list" (properties graph)) (generate-adj-list graph))
        graph))

;; TODO
;; (defun save-tgf (file-name g &optional &key (nodes-first t))
;;     )
