
(random (* pi 2))

(ql:quickload "cl-online-gnuplot")

(let* ((nn 3)
       (fnn (/ (sqrt nn))))
 (defun vec ()
   (declare (values (complex double-float) &optional))
   (let ((phi (random (* pi 2))))
     (* fnn (exp (complex 0 phi)))))

  (let* ((n 1000)
	 (hist (make-array n :element-type 'fixnum))
	 (ma 3d0))
    (loop for i below 1000000 do
	 (incf (aref hist (floor (* (/ (- n 1) ma)
				    (abs
				     (loop for i below nn sum (vec))))))))
    (defparameter *bla* hist)

    

    (cl-online-gnuplot:multi-plot (loop for e across *bla* and i from 0 collect
				       (list (* 2d0 (/ i (- n 1))) e)))))



(declaim (optimize (speed 3) (safety 0) (debug 0)))

(time (run))

(defun run ()
 (let* ((nn 3)
	(fnn (/ (sqrt nn)))
	(n-trials 10000000))
   (flet ((vec ()
	    (let ((phi (random (* pi 2))))
	      (* fnn (exp (complex 0 phi))))))
     (let* ((n 1000)
	    (hist (make-array n :element-type 'fixnum))
	    (ma 3d0))
       (loop for i below n-trials do
	    (incf (aref hist (floor (* (/ (- n 1) ma)
				       (abs
					(+ (vec) (vec) (vec))
					;(loop for i below nn sum (vec))
					))))))
       (defparameter *bla* hist)

    

       (cl-online-gnuplot:multi-plot (loop for e across *bla* and i from 0 collect
					  (list (* 2d0 (/ i (- n 1)))
						(/ e n-trials))))))))
