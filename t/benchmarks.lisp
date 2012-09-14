(in-package :fast-io.test)

(declaim (inline now))
(defun now ()
  (coerce (/ (get-internal-real-time)
             internal-time-units-per-second)
          'double-float))

(defmacro bench ((&optional (times 1)) &body body)
  (with-gensyms (results t1 t2 i)
    (declare (ignorable results t2))
    (once-only (times)
      `(let (,t1
             #+-(,results (make-array ,times :element-type 'double-float)))
         (declare (ignorable ,t1))
         (time
          (dotimes (,i ,times)
            #+-
            (setf ,t1 (now))
            ,@body
            #+-
            (let ((,t2 (now)))
              (setf (aref ,results ,i) (- ,t2 ,t1)))))
         #+-
         (format t "Tot: ~F   |  Min: ~F Max: ~F~%Avg: ~F Med: ~F Var: ~F Std: ~F"
                 (reduce #'+ ,results)
                 (reduce #'min ,results)
                 (reduce #'max ,results)
                 (mean ,results)
                 (median ,results)
                 (variance ,results)
                 (standard-deviation ,results))))))

 ;; Naive

(bench (50000)
  (let ((vec (make-array 16 :element-type 'octet
                            :adjustable t
                            :fill-pointer 0)))
    (dotimes (i 50)
      (vector-push-extend 0 vec))))

 ;; Flexi-streams

#+flexi-streams
(bench (50000)
  (flexi-streams:with-output-to-sequence (stream)
    (dotimes (i 50)
      (write-byte 0 stream))))

#+flexi-streams
(bench (50000)
  (flexi-streams:with-output-to-sequence (stream)
    (let ((vec (make-octet-vector 50)))
      (write-sequence vec stream))))

 ;; Fast-io

(bench (50000)
  (with-fast-output (buffer)
    (dotimes (i 50)
      (fast-write-byte 0 buffer))))

(bench (1000000)
  (let ((vec (make-octet-vector 50)))
   (with-fast-output (buffer)
     (fast-write-sequence vec buffer))))

 ;; Fast-io streams

(bench (1000000)
  (let ((stream (make-instance 'fast-output-stream))
        (vec (make-octet-vector 50)))
    (write-sequence vec stream)))
