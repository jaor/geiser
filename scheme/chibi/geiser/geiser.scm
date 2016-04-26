(define (all-environment-exports environment prefix)
  (if environment
      (append (filter (lambda (identifier)
                        (if (string=? prefix "")
                            #t
                            (string-contains identifier prefix)))
                      (map symbol->string (env-exports environment)))
              (all-environment-exports (env-parent environment) prefix))
      '()))

(define (geiser:completions prefix . rest)
  rest
  (sort (all-environment-exports (current-environment) prefix)
        string-ci<?))

(define (write-to-string form)
  (let ((out (open-output-string)))
    (write form out)
    (get-output-string out)))

(define (geiser:eval module form . rest)
  rest
  (let ((output (open-output-string))
        (result (if module
                    (let ((mod (module-env (find-module module))))
                      (eval form mod))
                    (eval form))))
    (write `((result ,(write-to-string result))
             (output . ,(get-output-string output))))
    (values)))

(define (geiser:module-completions prefix . rest)
  (let ((modules (map car (available-modules))))
    (map write-to-string
         (delete-duplicates
          (filter (lambda (module)
                    (if (string=? "" prefix)
                        #t
                        (string-contains prefix (write-to-string module))))
                  modules)))))

(define (geiser:autodoc ids . rest)
  '())

(define (geiser:no-values)
  #f)

(define (geiser:newline)
  #f)
