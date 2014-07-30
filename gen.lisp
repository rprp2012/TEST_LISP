(in-package :com.ckl.gen)

(defun print-coordinate (number name value out)
  (format out "$node_(~a) set ~a_ ~a~%" number name value))

(defun my-random (&key (max 1000) (rr 40))
  (loop for i = (random max) then (random max)
       until (or (< i (- (/ max 2) rr)) (> i (+ (/ max 2) rr)))
       finally (return i)))

(defun my-walk-list (number out values)
  (let ((u (car values)) (v (cdr values)))
    (when u
      (if (integerp (car v))
	  (progn
	    (print-coordinate number u (car v) out)
	    (setf values v))
	  (print-coordinate number u (my-random) out))
      (my-walk-list number out (cdr values)))))

(defun list-all-coordinate (number out &rest values)
  (my-walk-list number out values))

(defun gen-map (number string)
  (with-open-file (out string :direction :output :if-exists :supersede)
    (loop for i below number
	 do (list-all-coordinate i out :x :y :z 0))))

(defun gen-delay-awk (number &rest values)
  (with-open-file (in "delay.awk" :direction :input)
    (with-open-file (out "new-delay.awk" :direction :output :if-exists :supersede)
      (loop
	 for i = 1 then (+ i 1)
	 for line = (read-line in nil) then (read-line in nil)
	 while line
	 do (write-line line out)
	 do (when (eql i 4)
	      (format out "sum=~a;~%" number)
	      (walk-list out 1 values))))))

(defun walk-list (out no values)
  (when values
    (let ((u (car values)) (v (cdr values)))
      (format out "startlist[~a]=\"_~a_\";~%" no u)
      (format out "endlist[~a]=\"_~a_\";~%" no (car v))
      (walk-list out (+ no 1) (cdr v)))))

(defun gen-cbr-cfg (&rest values)
  (with-open-file (out "new-cbr.tcl" :direction :output :if-exists :supersede)
    (with-open-file (in "cbr.tcl" :direction :input)
      (loop for line = (read-line in nil) then (read-line in nil)
	 while line
	 do (write-line line out)))
    (format out "~%")
    (walk-list-by-two 1 values (cbr-print out))))

(defun walk-list-by-two (number values fn)
  (when values
    (let ((u (car values)) (v (cdr values)))
      (funcall fn number u (car v))
      (walk-list-by-two (+ number 1) (cdr v) fn))))

(defun cbr-print (out)
  #'(lambda (no u v)
      (format out "$ns_ at 10.5 \"$ragent_(~a) startSink 10.0\"~%" v)
      (format out "set udp_(~a) [new Agent/UDP]~%" no)
      (format out "$ns_ attach-agent $node_(~a) $udp_(~a)~%" u no)
      (format out "set null_(~a) [new Agent/Null]~%" no)
      (format out "$ns_ attach-agent $node_(~a) $null_(~a)~%" v no)
      (format out "set cbr_(~a) [new Application/Traffic/CBR]~%$cbr_(~a) set packetSize_ 32~%$cbr_(~a) set interval_ 2.0~%$cbr_(~a) set random_ 1~%~%" no no no no)
      (format out "$cbr_(~a) attach-agent $udp_(~a)~%$ns_ connect $udp_(~a) $null_(~a)~%$ns_ at 76.0 \"$cbr_(~a) start\"~%$ns_ at 500.0 \"$cbr_(~a) stop\"~%~%" no no no no no no)))

(defun list-gen-delay (number list)
  (apply #'gen-delay-awk number list))

(defun list-gen-cbr (list)
  (apply #'gen-cbr-cfg list))

;the following is only used for xyz

(defun gen-point (max &key (x 0 x-supplied-p) (y 0 y-supplied-p) (z 0 z-supplied-p))
  (when (eql x-supplied-p nil)
    (setf x (my-random :max max)))
  (when (eql y-supplied-p nil)
    (setf y (my-random :max max)))
  (when (eql z-supplied-p nil)
    (setf z (my-random :max max)))
  (list x y z))

(defun gen-pset (number max)
  (with-open-file (out "set-of-point" :direction :output :if-exists :supersede)
    (loop repeat number
       do (print (gen-point max :z 0) out))))

(defun gen-map-from-points ()
  (with-open-file (out "new-map.tcl" :direction :output :if-exists :supersede)
    (with-open-file (in "set-of-point" :direction :input :if-does-not-exist nil)
      (when in
	(loop
	   for i = 0 then (+ i 1)
	   for line = (read in nil) then (read in nil)
	   while line
	   do (print-coordinate i 'x (first line) out)
	   do (print-coordinate i 'y (second line) out)
	   do (print-coordinate i 'z (third line) out))))))

(defun gen-map-cbr-awk (number max)
 ; (gen-pset number max)
  (gen-map-from-points)
  (let ((values (get-suit-pair number)))
    (list-gen-delay 8 values)
    (list-gen-cbr values)))

(defun get-suit-pair (number)
  (let ((point-array (make-array number :fill-pointer 0)))
    (with-open-file (in "set-of-point" :direction :input :if-exists :supersede)
      (loop
	 repeat number
	 for line = (read in nil) then (read in nil) while line do
	   (vector-push line point-array)))
    (loop repeat 8 with values = nil
       do (do ((x 0 (random number)) (y 0 (random number)))
	      ((my-judge (aref point-array x) (aref point-array y)) (progn (push x values) (push y values))))
       finally (return values))))
	   

(defun my-judge (u v)
  (let ((x1 (car u)) (y1 (cadr u)) (x2 (car v)) (y2 (cadr v)))
    (or
     (and (< x1 460) (> x2 540) (< y1 460) (> y2 540))
     (and (< x1 460) (> y1 540) (> x2 540) (< y2 460)))))

(defun my-judge (u v)
  (not (eql u v)))

(defun gen-aver-map ()
  (with-open-file (out "set-of-point" :direction :output :if-exists :supersede)
    (loop
       for i from 45 to 1000
       do (loop
	     for j from 0 to 1000
	     do (if (and (eql (mod i 45) 0) (eql (mod j 45) 0))
		    (progn
		      (format out "(~a ~a ~a)~%" i j 0)))))))
