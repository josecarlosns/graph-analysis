;; Simple queue implementation
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

;; Prints progress information about given operation
(defun print-progress (steps steps-total elapsed-time)
    (let ((time-per-step nil) (etl nil) (steps-left nil))
        (setf time-per-step (/ elapsed-time (if (= 0 steps) 1 steps)))
        (setf steps-left (- steps-total steps))
        (setf etl (* time-per-step steps-left))
        (format t "~aProgress: ~,5f%~tElapsed time: ~d:~2,'0d:~2,'0d~tETL: ~d:~2,'0d:~2,'0d" 
            #\return
            (* 100.0 (/ steps steps-total))
            (floor (/ elapsed-time 3600000)) (rem (floor (/ elapsed-time 60000)) 60) (rem (floor (/ elapsed-time 1000)) 60)
            (floor (/ etl 3600000)) (rem (floor (/ etl 60000)) 60) (rem (floor (/ etl 1000)) 60))
        (finish-output)))

;; Function to split string given an separator string
;; Taken fron: https://gist.github.com/siguremon/1174988
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

;; Prints to an stream the list in a way that can be plotted on a graph.
;; Each element of the list will be printed on a new line.
(defun plot-data (data &optional &key (stream t))
    (loop for e in data and index from 0 do
        (if (listp e)
            (format stream "~d ~{~f ~}~%" index e)
            (if (arrayp e)
                (progn
                    (format stream "~d " index)
                    (dotimes (n (length e))
                        (format stream "~f " (aref e n)))
                    (format stream "~%"))
                (format stream "~d ~f~%" index e)))))

;; Saves the data in an file in an "plottable" way
(defun save-data (data file-name)
    (let ((file nil))
        (setf file (open (merge-pathnames file-name) :direction :output :if-exists :supersede :if-does-not-exist :create))
        (plot-data data :stream file)
        (close file)))

;; Chooses an number of neighbors to an node, with highest chance to those with highest degree
(defun choose-neighbors (adj-list node num-neighbors)
    (let ((neighbors nil) (degree-sum 0) (r-state (make-random-state t)))
        (dotimes (n (length adj-list))
            (let ((degree nil))
                (setf degree (list-length (aref adj-list n)))
                (incf degree-sum degree)))
        (dotimes (n num-neighbors)
            (let ((rand-num nil) (neighbor 0) (prob nil) (degree nil) (prob-sum 0))
                (setf rand-num (random 1.0 r-state))
                (loop
                    (setf degree (list-length (aref adj-list neighbor)))
                    (if (= 0 degree)
                        (if (= 0 degree-sum)
                            (setf prob (/ 1 (length adj-list)))
                            (setf prob 0))
                        (if (= 0 degree-sum)
                            (setf prob 1)
                            (setf prob (/ degree degree-sum))))
                    (incf prob-sum prob)
                    (if (and (not (find neighbor neighbors)) (<= rand-num prob-sum) (not (= neighbor node)))
                        (progn
                            (push neighbor neighbors)
                            (decf degree-sum degree)
                            (return nil))
                        (incf neighbor)))))
        neighbors))
