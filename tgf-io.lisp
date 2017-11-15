(load "graph.lisp")

(defun load-tgf (file-name &optional &key (g-type 1) (node-naming t))
    (let ((in) (graph) (nodes) (edges) (current-list))
        (setf in (open file-name :if-does-not-exist nil))
        (setf graph (make-instance 'graph :g-type g-type))
        (setf current-list 1)
        (when in
            (loop for line = (read-line in nil)
                while line do 
                    (progn
                        (let ((e))
                            (setf e (split-str line " "))
                            (if node-naming
                                (if (string= (first e) "#")
                                    (setf current-list 2)
                                    (if (= current-list 1)
                                        (push (parse-integer (first e)) nodes)
                                        (push (mapcar #'parse-integer e) edges)))
                                (let ((edge nil))
                                    (setf edge (mapcar #'parse-integer e))
                                    (when (not (find (first edge) nodes))
                                        (push (first edge) nodes))
                                    (when (not (find (second edge) nodes))
                                        (push (second edge) nodes))
                                    (push edge edges))))))
            (close in))
        (setf (nodes graph) nodes)
        (setf (edges graph) edges)
        (setf (g-type graph) g-type)
        graph))

;; Funções para separar uma string, dado uma string "separadora"
;; Retirado de: https://gist.github.com/siguremon/1174988
(defun split-str (string &optional (separator " "))
  (split-str-1 string separator))

(defun split-str-1 (string &optional (separator " ") (r nil))
  (let ((n (position separator string
		     :from-end t
		     :test #'(lambda (x y)
			       (find y x :test #'string=)))))
    (if n
	    (split-str-1 (subseq string 0 n) separator (cons (subseq string (1+ n)) r))
        (cons string r))))
