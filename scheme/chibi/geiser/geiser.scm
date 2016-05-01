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
  ;; (available-modules) walks the directory tree and is too slow
  (let ((modules (map car *modules*)))
    (map write-to-string
         (delete-duplicates
          (filter (lambda (module)
                    (if (string=? "" prefix)
                        #t
                        (string-contains prefix (write-to-string module))))
                  modules)))))

(define (procedure-arglist id fun)
  (let ((arglist (lambda-params (procedure-analysis fun))))
    (if (pair? arglist)
        (let loop ((arglist arglist)
                   (optionals? #f)
                   (required '())
                   (optional '()))
          (cond ((null? arglist)
                 `(,id ("args" (("required" ,@(reverse required))
                                ("optional" ,@(reverse optional))
                                ("key")
                                ("module" ,(let ((mod (containing-module fun))) (if mod (car mod) #f)))))))
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
                  (if optionals? (cons (car arglist) optional) optional)))))
        '())))

(define (geiser:operator-arglist id)
  (let ((binding (eval id)))
    (cond ((procedure? binding)
           (if (opcode? binding)
               '()
               (procedure-arglist id binding)))
          (else
           '()))))

(define (geiser:autodoc ids . rest)
  rest
  (cond ((null? ids) '())
        ((not (list? ids))
         (geiser:autodoc (list ids)))
        ((not (symbol? (car ids)))
         (geiser:autodoc (cdr ids)))
        (else
         (map (lambda (id)
                (geiser:operator-arglist id))
              ids))))

(define (geiser:no-values)
  #f)

(define (geiser:newline)
  #f)
