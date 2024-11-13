#!/usr/bin/guile -s
!#

(use-modules (ice-9 format)
	     (rnrs io ports))

(define (bytes->hexlist bytes)
  "Convert a bytevec to a list of hex values."
  (map (lambda (byte)
	 (format #f "~2,'0x" byte))
       (u8vector->list bytes)))

(define (read-n-bytes port n)
  "Read n bytes from a port."
  (let ((bytes (make-u8vector n 0)))
    (get-bytevector-n! port bytes 0 n)
    (bytes->hexlist bytes)))

(define (trim-trailing-zeros hexlist)
  "Trim trailing zeros from a list of hex strings."
  (map (lambda (hex)
	 (if (and (= (string-length hex) 2)
		  (char=? (string-ref hex 0) #\0)
		  (char=? (string-ref hex 1) #\0))
	     (string-replace hex "  ")
	     hex))
       hexlist))

(define (format-offset offset)
  "Format a number as an 8-digit hexadecimal string with leading zeros."
  (let ((hex (number->string offset 16)))
    (string-append (make-string (- 8 (string-length hex)) #\0)
                  hex ": ")))

(define (format-hex hexlist)
  "Format the hexlist."
 (string-join
    (let ((len (length hexlist)))
      (map (lambda (i)
             (if (< (+ i 1) len)
                 (string-append (list-ref hexlist i) (list-ref hexlist (+ i 1)))
                 (list-ref hexlist i)))
	   ;; Grab only the even indicies for the map function.
           (filter even? (iota len))))
    " "))

(define (format-port port)
  "Print the first 16 chars of a port."
  (let loop ((i 0)
	     (result ""))
    (if (or (>= i 16)
	    (eof-object? (peek-char port)))
	result
	(loop (+ i 1)
	      (string-append result (string (read-char port)))))))

(define (xxd line offset)
  (let ((chars (format-port line)))
    (string-append
     (format-offset offset)
     (format-hex
      (trim-trailing-zeros (read-n-bytes (open-input-string chars) 16)))
     "  "
     chars
     "\n")))

(define (main)
  "Main program entrypoint."
  (let* ((args (command-line))
	 (port (if (> (length args) 1)
		   (open-input-file (list-ref args 1))
		   (current-input-port))))
    (let loop ((offset 0))
      (unless (eof-object? (peek-char port))
	(display (xxd port offset))
	(loop (+ offset 16))))))

(main)
