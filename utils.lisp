;; Implementação de uma fila simples em LISP e suas funções de suporte
(defstruct queue
    head
    tail)

(defstruct queue-block
    value
    next)

(defun queue-pop (q)
    (let ((value nil))
        (setf value (queue-block-value (queue-head q)))
        (when (eq (queue-head q) (queue-tail q))
            (setf (queue-tail q) nil))
        (setf (queue-head q) (queue-block-next (queue-head q)))
        value))

(defun queue-put (v q)
    (let ((new-block nil))
        (setf new-block (make-queue-block :value v :next nil))
        (if (null (queue-tail q))
            (setf (queue-tail q) new-block)
            (setf (queue-block-next (queue-tail q)) new-block))
        (when (null (queue-head q))
            (setf (queue-head q) (queue-tail q)))
        (setf (queue-tail q) new-block)))

(defun queue-empty (q)
    (and (null (queue-head q)) (null (queue-tail q))))

(defun queue-to-list (q)
    (let ((q-list nil) (current-block nil))
        (if (not (queue-empty q))
            (progn
                (setf current-block (queue-head q))
                (loop
                    (push (queue-block-value current-block) q-list)
                    (setf current-block (queue-block-next current-block))
                    (when (null current-block)
                        (return (reverse q-list)))))
            nil)))

;; Imprime um texto humanamente legível sobre o progresso de um algoritmo
(defun print-progress (steps steps-total time-per-step &optional &key (process-name nil))
    (let ((elapsed-time nil) (etl nil) (steps-left nil))
        (setf elapsed-time (* steps time-per-step))
        (setf steps-left (- steps-total steps))
        (setf etl (* time-per-step steps-left))
        (format t "~%Progress~a: ~,5f%~%" (if process-name (format nil " of ~a" process-name) "") (* 100.0 (/ steps steps-total)))
        (format t "Elapsed time in HH:MM:SS: ~d:~2,'0d:~2,'0d~%" (floor (/ elapsed-time 3600000)) (rem (floor (/ elapsed-time 60000)) 60) (rem (floor (/ elapsed-time 1000)) 60))
        (format t "ETL: ~d:~2,'0d:~2,'0d~%" (floor (/ etl 3600000)) (rem (floor (/ etl 60000)) 60) (rem (floor (/ etl 1000)) 60))
        (terpri)))

;; Funções para separar uma string, dado uma string "separadora"
;; Retirado de: https://gist.github.com/siguremon/1174988
(defun split-str-1 (string &optional (separator " ") (r nil))
  (let ((n (position separator string
		     :from-end t
		     :test #'(lambda (x y)
			       (find y x :test #'string=)))))
    (if n
	    (split-str-1 (subseq string 0 n) separator (cons (subseq string (1+ n)) r))
        (cons string r))))

(defun split-str (string &optional (separator " "))
  (split-str-1 string separator))

;; Return a new list of the N cars of 'list'
(defun nthcar (n list)
    (let ((new-list nil))
        (loop for i from 1 to n and e in list do
            (push e new-list))
        (reverse new-list)))
