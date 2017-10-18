(load "graph.lisp")

(defun load-tgf (file-name &optional &key (g-type 1))
    (let ((in) (graph) (nodes) (edges))
        (setf graph (make-instance 'graph :g-type g-type))
        (setf in (open file-name :if-does-not-exist nil))
        (when in
            (loop for line = (read-line in nil)
                while line do 
                    (progn
                        (let ((e))
                            (setf e (split-str line " "))
                            (if (> (list-length e) 1)
                                (push e edges)
                                (push e nodes)))))
            (close in))
        (setf (nodes graph) (rest nodes))
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
