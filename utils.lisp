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
