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

  (define (geiser:autodoc ids . rest)
    '())

  (define (geiser:no-values)
    #f)

  (define (geiser:newline)
    #f))
