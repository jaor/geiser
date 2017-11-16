(define string-prefix?
  (lambda (x y)
    (let ((n (string-length x)))
      (and (<= n (string-length y))
           (let prefix? ((i 0))
             (or (= i n)
                 (and (char=? (string-ref x i) (string-ref y i))
                      (prefix? (+ i 1)))))))))

(define (split-by l p k)
  (let loop ((low '())
             (high '())
             (l l))
    (cond ((null? l)
           (k low high))
          ((p (car l))
           (loop low (cons (car l) high) (cdr l)))
          (else
           (loop (cons (car l) low) high (cdr l))))))
 
(define (sort f l)
  (if (null? l)
      '()
      (split-by (cdr l) 
                (lambda (x) (f (car l) x))
                (lambda (low high)
                  (append (sort f low)
                          (list (car l))
                          (sort f high))))))

(define (filter pred lst)
  (cond ((null? lst) '())
        ((pred (car lst))
         (cons (car lst) (filter pred (cdr lst))))
        (else (filter pred (cdr lst)))))

(define (call-with-output-string proc)
  (let ((out (open-output-string)))
    (proc out)
    (get-output-string out)))

(define (with-output-to-string thunk)
  (call-with-output-string
    (lambda (out)
      (let ((old-out (current-output-port)))
        (set-output-port out)
        (thunk)
        (set-output-port old-out)))))

(define (write-to-string x)
  (with-output-to-string
    (lambda ()
      (write x))))

(define (map f a)
  (let ((map1 #f))
    (let ((tmp-map1
            (lambda (a)
              (if (null? a)
                  '()
                   (cons (f (car a))
                         (map1 (cdr a)))))))
    (set! map1 tmp-map1)
    (map1 a))))

(define (geiser:eval module form . rest)
    rest
    (let ((output (open-output-string))
	  (result (if module
                      (eval form (environment module))
                      (eval form))))
      (write `((result ,(write-to-string result))
               (output ,(get-output-string output))))
      (newline)))

(define (geiser:completions prefix . rest)
  rest
  ;; WSG: Fix the string comparison problem
  ;; (sort string-ci<?
        (filter (lambda (el)
                  (string-prefix? prefix el))
                (map write-to-string (apply append (oblist)))))
;; )

(define (geiser:no-values)
  #f)

(define (geiser:newline)
  #f))
