#!/usr/bin/guile -s
!#

(use-modules (ice-9 format)
	     (ice-9 getopt-long)
	     (ice-9 rdelim)
	     (ice-9 regex)
	     (srfi srfi-1)
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

(define option-spec
  '((version (single-char #\v) (value #f))
    (help (single-char #\h) (value #f))
    (reverse (single-char #\r) (value #f))))

(define (print-usage)
  "Print the help options."
     (format #t "~{~a~%~}"
             '("Usage:"
	       "       xxd [options] [infile [outfile]]"
               "or"
               "        xxd -r [infile [outfile]]"
               "Options:"
               "  -h, --help     print this help message and exit."
               "  -v, --version  print the version number and exit."
               "  -r, --reverse  reverse operation: convert (or patch) hexdump into binary.")))

(define (parse-args args)
  "Collect cli args."
  (let* ((options (getopt-long (command-line) option-spec))
	 (help-wanted (option-ref options 'help #f))
	 (version-wanted (option-ref options 'version #f))
	 (reverse-operation (option-ref options 'reverse #f))
	 (val (option-ref options '() '())))
    (cond
     (help-wanted
      (print-usage)
      (exit 0))
     (version-wanted
      (display "Version 1.0.0\n")
      (exit 0))
     (reverse-operation
      (display "REVERSE")
      (exit 0))
     (else val))))

(define (hex-char->integer c)
  (let ((n (char->integer c)))
    (cond
     ((and (>= n (char->integer #\0))
           (<= n (char->integer #\9)))
      (- n (char->integer #\0)))
     ((and (>= n (char->integer #\a))
           (<= n (char->integer #\f)))
      (+ 10 (- n (char->integer #\a))))
     ((and (>= n (char->integer #\A))
           (<= n (char->integer #\F)))
      (+ 10 (- n (char->integer #\A))))
     (else (error "Invalid hex character" c)))))

(define (hex-pair->char hex-str)
  (integer->char
   (+ (* 16 (hex-char->integer (string-ref hex-str 0)))
      (hex-char->integer (string-ref hex-str 1)))))

(define (split-into-pairs str)
  (let loop ((chars (string->list str))
             (pairs '()))
    (if (< (length chars) 2)
        (reverse pairs)
        (loop (cddr chars)
              (cons (list->string (take chars 2)) pairs)))))

(define (xxd-reverse port)
  (let* ((line (get-string-all port))
	 (match (string-match "^[0-9a-fA-F]+:\\s+([0-9a-fA-F ]+)" line))
         (hex-part (if match
                      (match:substring match 1)
                      (error "Invalid hex dump format" line))))
    (let* ((hex-str (string-filter (lambda (c) (not (char=? c #\space))) hex-part))
           (hex-pairs (split-into-pairs hex-str)))
      (string-concatenate
       (map (lambda (pair)
              (string (hex-pair->char pair)))
            hex-pairs)))))

(define (main)
  "Main program entrypoint."
  (let* ((args (parse-args (command-line)))
	 (port (if (> (length args) 1)
		   (open-input-file (car args))
		   (current-input-port))))
    (let loop ((offset 0))
      (unless (eof-object? (peek-char port))
	;; (display (xxd port offset))
	(display (xxd-reverse port))
	(loop (+ offset 16))))))

(main)
