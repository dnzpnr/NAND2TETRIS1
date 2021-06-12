
(define symbol-table ())
(define list1 ())

(define (assembler exp)
  (cond ((equal? exp "") exp)
          ((equal? (string-ref exp 0) '#\@)
           (a-ins (substring exp 1)))
          (else exp)))

(define (a-ins exp)
  (if (number? (string->number exp)) (change-binary (string->number exp))
      (check exp symbol-table)))



(define (check exp symbol-table)
  (cond ((and (equal? (string-ref exp 0) #\R)
       (number? (string->number (substring exp 1))))
         (change-binary (string->number (substring exp 1))))
        ((equal? exp "SP") (change-binary 0))
        ((member? (string->symbol exp) symbol-table)
         (call (string->symbol exp) symbol-table))
        (else (add-list (string->symbol exp)))))

(define (call exp symbol-table)
  (cond ((equal? (caar symbol-table) exp)
      (change-binary (cadar symbol-table)))
      (else (call exp (cdr symbol-table)))))


(define (member? x lst)
  (if (empty? lst)
    #f
    (if (equal? x (car lst))
      #t
      (begin
        (if (list? (car lst))
          (let ((r (member? x (car lst))))
               (if r
                 r
                 (member? x (cdr lst))))
          (member? x (cdr lst)))))))

(define number 16)

(define (add-list exp)
  (set! list1 (list exp number))
  (set! symbol-table (cons list1 symbol-table))
  (set! list1 ())
  (set! number (+ 1 number))
  (change-binary (- number 1)))

(define (change-binary x)
  (cond ((< (string-length (number->string x 2)) 16)
         (string-append
          (make-string (- 16 (string-length (number->string x 2))) #\0)
                          (number->string x 2)))
        ((= (string-length (number->string x 2)) 16)
         (number->string x 2))
        (else 'very-large-number)))



(require 2htdp/batch-io)

(write-file "deneme.hack" (foldr string-append "" (map (lambda (x) (string-append (assembler x) "\n")) (read-lines "/home/deniz/deneme.asm"))))


