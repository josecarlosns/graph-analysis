(load "graph.lisp")

(defun load-tgf (file-name &optional &key (g-type 1) (nodes-first t) (weighted nil))
    (let ((in) (graph) (nodes) (edges) (current-list))
        (setf in (open file-name :if-does-not-exist nil))
        (setf graph (make-instance 'graph :g-type g-type :weighted nil))
        (setf current-list 1)
        (when in
            (loop for line = (read-line in nil)
                while line do 
                    (progn
                        (let ((e))
                            (setf e (split-str line " "))
                            (if nodes-first
                                (if (string= (first e) "#")
                                    (setf current-list 2)
                                    (if (= current-list 1)
                                        (push (parse-integer (first e)) nodes)
                                        (push (mapcar #'parse-integer e) edges)))
                                (let ((edge nil))
                                    (setf edge (mapcar #'parse-integer e))
                                    (when (not weighted)
                                        (setf edge (remove (first (last edge)) edge)))
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

(defun save-tgf (file-name g &optional &key (nodes-first t))
    (let ((data nil) (file nil))
        (dolist (e (edges g))
            (push e data))
        (when nodes-first
            (progn
                (push "#" data)
                (dolist (n (nodes g))
                    (push n data))))
        (setf file (open file-name :direction :output :if-exists :supersede))
        (dolist (d data)
            (if (listp d)
                (progn
                    (format file "~a ~a" (first d) (second d))
                    (when (weighted g)
                        (format file " ~a" (third d)))
                    (format file "~%"))
                (format file "~a~%" d)))
        (close file)))

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
