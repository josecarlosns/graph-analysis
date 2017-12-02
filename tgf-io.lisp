(load "graph.lisp")
(load "utils.lisp")
(load "metrics.lisp")

(defun load-tgf (file-name &optional &key (g-type 1) (weighted nil) (separator " "))
    (let ((in nil) (graph nil) (in-edges nil) (node-id-list nil) (node-list nil) (adj-list nil))
        (setf in (open file-name :if-does-not-exist nil))
        (setf graph (empty-graph g-type weighted))
        (when in
            (loop for line = (read-line in nil)
                while line do 
                    (progn
                        (let ((e nil))  
                            (when (not (string= (char line 0) "#"))
                                (setf e (split-str line separator))
                                (when (> (list-length e) 1)
                                    (setf in-edges t))
                                (when in-edges
                                    (let ((node1-id 0) (node2-id 0) (node1 nil) (node2 nil) (edge nil))
                                        (loop for element in e and i from 0 do
                                            (case i
                                                (0 (setf node1-id (parse-integer element)))
                                                (1 (setf node2-id (parse-integer element)))))
                                        (if (not (find node1-id node-id-list :test #'equal))
                                            (progn
                                                (setf node1 (make-node :id node1-id))
                                                (push node1 node-list)
                                                (push node1-id node-id-list))
                                            (dolist (node node-list)
                                                (when (= (node-id node) node1-id)
                                                    (return (setf node1 node)))))
                                        (if (not (find node2-id node-id-list :test #'equal))
                                            (progn
                                                (setf node2 (make-node :id node2-id))
                                                (push node2 node-list)
                                                (push node2-id node-id-list))
                                            (dolist (node node-list)
                                                (when (= (node-id node) node2-id)
                                                    (return (setf node2 node)))))
                                        (setf edge (list node1 node2))
                                        (add-edge graph edge)
                                        (push node2 (node-adj-nodes node1))
                                        (if (= 2 g-type)
                                            (push node1 (node-adj-nodes node2)))))))))
            (close in))
        (setf (nodes graph) (make-array (list-length node-id-list) :initial-element nil))
        (setf adj-list (make-array (list-length node-id-list) :initial-element nil))
        ;; (print adj-list)
        (let ((number-of-nodes 0))
            (loop for node in node-list and index from 0 do
                (setf (aref (nodes graph) index) node)
                (incf number-of-nodes))
            (setf (gethash "number-of-nodes" (properties graph)) number-of-nodes))
        (setf (adj-list graph) (generate-adj-list graph))
        graph))

;; TODO
;; (defun save-tgf (file-name g &optional &key (nodes-first t))
;;     )
