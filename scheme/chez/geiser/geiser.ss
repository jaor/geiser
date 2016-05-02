(library (geiser)
  (export geiser:eval
          geiser:completions
          geiser:module-completions
          geiser:autodoc
          geiser:no-values
          geiser:newline)
  (import (chezscheme))

  (define string-prefix?
    (lambda (x y)
      (let ([n (string-length x)])
        (and (fx<= n (string-length y))
             (let prefix? ([i 0])
               (or (fx= i n)
                   (and (char=? (string-ref x i) (string-ref y i))
                        (prefix? (fx+ i 1)))))))))

  (define (geiser:completions prefix . rest)
    rest
    (sort string-ci<?
          (filter (lambda (el)
                    (string-prefix? prefix el))
                  (map write-to-string (environment-symbols (interaction-environment))))))

  (define (write-to-string x)
    (with-output-to-string
      (lambda ()
        (write x))))

  (define (geiser:eval module form . rest)
    rest
    (let ((result (if module
                      (eval form (environment module))
                      (eval form))))
      (write `((result ,(write-to-string result))
               (output . "")))
      (newline)))

  (define (geiser:module-completions prefix . rest)
    (define (substring? s1 s2)
      (let ([n1 (string-length s1)] [n2 (string-length s2)])
        (let loop2 ([i2 0])
          (let loop1 ([i1 0] [j i2])
            (if (fx= i1 n1)
                i2
                (and (not (fx= j n2))
                     (if (char=? (string-ref s1 i1) (string-ref s2 j))
                         (loop1 (fx+ i1 1) (fx+ j 1))
                         (loop2 (fx+ i2 1)))))))))
    (filter (lambda (el)
              (substring? prefix el))
            (map write-to-string (library-list))))

  (define (procedure-parameter-list p)
    ;; same as (inspect object), then hitting c
    (let ((s (((inspect/object p) 'code) 'source)))
      (if s
          (let ((form (s 'value)))
            (if (and (list? form)
                     (> (length form) 2)
                     (eq? (car form) 'lambda))
                (cadr form)
                #f))
          #f)))

  (define (operator-arglist operator)
    (let ((binding (eval operator)))
      (if binding
          (let ((arglist (procedure-parameter-list binding)))
            (let loop ((arglist arglist)
                       (optionals? #f)
                       (required '())
                       (optional '()))
              (cond ((null? arglist)
                     `(,operator ("args" (("required" ,@(reverse required))
                                          ("optional" ,@(reverse optional))
                                          ("key")
                                          ;; ("module" ,module)
                                          ))))
                    ((symbol? arglist)
                     (loop '()
                           #t
                           required
                           (cons "..." (cons arglist optional))))
                    (else
                     (loop
                      (cdr arglist)
                      optionals?
                      (if optionals? required (cons (car arglist) required))
                      (if optionals? (cons (car arglist) optional) optional))))))
          '())))

  (define (geiser:autodoc ids . rest)
    (cond ((null? ids) '())
          ((not (list? ids))
           (geiser:autodoc (list ids)))
          ((not (symbol? (car ids)))
           (geiser:autodoc (cdr ids)))
          (else
           (map (lambda (id)
                  (operator-arglist id))
                ids))))

  (define (geiser:no-values)
    #f)

  (define (geiser:newline)
    #f))
