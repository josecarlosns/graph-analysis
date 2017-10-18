(defun load-tgf (file-name &optional &key (g-type 1) (node-number) (edge-number))
    (let ((in) (graph) (nodes) (edges))
        (setf graph (make-instance 'graph :g-type g-type))
        (setf in (open file-name :if-does-not-exist nil))
        (when in
            (loop for line = (read-line in nil)
                while line do (eval line)
            )
            (close in)
        )
    )
)

