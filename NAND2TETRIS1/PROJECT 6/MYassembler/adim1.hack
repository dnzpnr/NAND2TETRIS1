
(define counter -1)

(define (pass1 exp)
  (set! counter (+ 1 counter))
    (cond ((and (equal? (string-ref exp 0) '#\()
                (equal? (string-ref exp (- (string-length exp) 1)) '#\)))
           (l-command (substring exp 1 (- (string-length exp) 1)))
           (let ((exp "")) exp))
          (else exp)))

(define (l-command x)
  (add-list-l (string->symbol x)))

(define list1 ())
(define symbol-table ())

(define (add-list-l exp)
  (set! list1 (list exp counter))
  (set! symbol-table (cons list1 symbol-table))
  (set! list1 ()))




(require 2htdp/batch-io)

(write-file "deneme.asm" (foldr string-append "" (map (lambda (x) (string-append (pass1 x) "\n"))
                                                       (read-lines "/home/deniz/symboliccode.asm"))))
                                                       
                                                       (define (a-ins x)
  (cond ((number? (string->number x)) (change-binary (string->number x)))
        ((equal? x "SP") (change-binary 0))
        ((equal? x "LCL") (change-binary 1))
        ((equal? x "ARG") (change-binary 2))
        ((equal? x "THIS") (change-binary 3))
        ((equal? x "THAT") (change-binary 4))
        ((equal? x "TEMP") (change-binary 5))
        ((equal? x "STATIC") (change-binary 16))
        (else (check x symbol-table))))
