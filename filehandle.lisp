(in-package :com.ckl.filehandle)

(defun div (n m)
  (/ (- n (mod n m)) m))

(defun change-to-hex (input-name output-name)
    (let ((stream (open input-name :direction :input :element-type '(unsigned-byte 8)))
	  (hn "0123456789ABCDEF")
	  (out (open output-name :direction :output :if-exists :supersede)))
      (loop for line = (read-byte stream nil) then (read-byte stream nil)
	 while line
	 do (format out "~a~a~%" (aref hn (div line 16)) (aref hn (mod line 16))))
      (close stream)
      (close out)))

