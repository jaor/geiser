;; Copyright (C) 2015 Daniel J Leslie

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the Modified BSD License. You should
;; have received a copy of the license along with this program. If
;; not, see <http://www.xfree86.org/3.3.6/COPYRIGHT2.html#5>.

(module geiser
  ;; A bunch of these needn't be toplevel functions
  (geiser-eval
   geiser-no-values
   geiser-newline
   geiser-start-server
   geiser-completions
   geiser-autodoc
   geiser-object-signature
   geiser-symbol-location
   geiser-symbol-documentation
   geiser-find-file
   geiser-add-to-load-path
   geiser-load-file
   geiser-compile-file
   geiser-compile
   geiser-module-exports
   geiser-module-path
   geiser-module-location
   geiser-module-completions
   geiser-macroexpand
   make-geiser-toplevel-bindings)

  ;; Necessary built in units
  (import chicken
          scheme
          extras
          data-structures
          ports
          csi
          irregex
          srfi-1
          posix
          utils)

  (use apropos
       regex
       chicken-doc
       tcp
       srfi-18)

  (define use-debug-log #f)

  (if use-debug-log
   (use posix))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Symbol lists
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define geiser-r4rs-symbols
    (make-parameter
     '(not boolean? eq? eqv? equal? pair? cons car cdr caar cadr cdar cddr
           caaar caadr cadar caddr cdaar cdadr cddar cdddr caaaar caaadr caadar
           caaddr cadaar cadadr caddar cadddr cdaaar cdaadr cdadar cdaddr cddaar
           cddadr cdddar cddddr set-car! set-cdr! null? list? list length
           list-tail list-ref append reverse memq memv member assq assv assoc
           symbol? symbol->string string->symbol number? integer? exact? real?
           complex? inexact? rational? zero? odd? even? positive? negative?
           max min + - * / = > < >= <= quotient remainder modulo gcd lcm abs
           floor ceiling truncate round exact->inexact inexact->exact exp log
           expt sqrt sin cos tan asin acos atan number->string string->number
           char? char=? char>? char<? char>=? char<=? char-ci=? char-ci<?
           char-ci>? char-ci>=? char-ci<=? char-alphabetic? char-whitespace?
           char-numeric? char-upper-case? char-lower-case? char-upcase
           char-downcase char->integer integer->char string? string=? string>?
           string<? string>=? string<=? string-ci=? string-ci<? string-ci>?
           string-ci>=? string-ci<=?  make-string string-length string-ref
           string-set! string-append string-copy string->list list->string
           substring string-fill! vector? make-vector vector-ref vector-set!
           string vector vector-length vector->list list->vector vector-fill!
           procedure? map for-each apply force call-with-current-continuation
           input-port? output-port? current-input-port current-output-port
           call-with-input-file call-with-output-file open-input-file
           open-output-file close-input-port close-output-port load
           read eof-object? read-char peek-char write display write-char
           newline with-input-from-file with-output-to-file eval char-ready?
           imag-part real-part magnitude numerator denominator
           scheme-report-environment null-environment interaction-environment
           else)))

  (define geiser-r5rs-symbols
    (make-parameter
     '(abs acos and angle append apply asin assoc assq assv atan begin
           boolean? caar cadr call-with-current-continuation
           call-with-input-file call-with-output-file call-with-values
           car case cdddar cddddr cdr ceiling char->integer char-alphabetic?
           char-ci<=? char-ci<? char-ci=? char-ci>=? char-ci>? char-downcase
           char-lower-case? char-numeric? char-ready? char-upcase
           char-upper-case? char-whitespace? char<=? char<? char=? char>=?
           char>? char? close-input-port close-output-port complex? cond cons
           cos current-input-port current-output-port define define-syntax
           delay denominator display do dynamic-wind else eof-object? eq?
           equal? eqv? eval even? exact->inexact exact? exp expt floor
           for-each force gcd if imag-part inexact->exact inexact? input-port?
           integer->char integer? interaction-environment lambda lcm length
           let let* let-syntax letrec letrec-syntax list list->string
           list->vector list-ref list-tail list? load log magnitude make-polar
           make-rectangular make-string make-vector map max member memq memv
           min modulo negative? newline not null-environment null?
           number->string number? numerator odd? open-input-file
           open-output-file or output-port? pair? peek-char port? positive?
           procedure? quasiquote quote quotient rational? rationalize read
           read-char real-part real? remainder reverse round
           scheme-report-environment set! set-car! set-cdr! setcar sin sqrt
           string string->list string->number string->symbol string-append
           string-ci<=? string-ci<? string-ci=? string-ci>=? string-ci>?
           string-copy string-fill! string-length string-ref string-set!
           string<=? string<? string=? string>=? string>? string? substring
           symbol->string symbol? syntax-rules tan transcript-off transcript-on
           truncate values vector vector->list vector-fill! vector-length
           vector-ref vector-set! vector? with-input-from-file with-output-to-file
           write write-char zero?)))

  (define geiser-r7rs-small-symbols
    (make-parameter
     '(* + - ... / < <= = => > >= abs and append apply assoc assq
         assv begin binary-port? boolean=? boolean? bytevector
         bytevector-append bytevector-copy bytevector-copy! bytevector-length
         bytevector-u8-ref bytevector-u8-set! bytevector? caar cadr
         call-with-current-continuation call-with-port call-with-values call/cc
         car case cdar cddr cdr ceiling char->integer char-ready? char<=?
         char<? char=? char>=? char>? char? close-input-port
         close-output-port close-port complex? cond cond-expand cons
         current-error-port current-input-port current-output-port
         define define-record-type define-syntax define-values denominator do
         dynamic-wind else eof-object? equal? error error-object-message
         even? exact-integer-sqrt exact? features floor floor-remainder
         flush-output-port gcd get-output-string if include-ci inexact?
         input-port? integer? lcm let let*-values let-values letrec* list
         list->vector list-ref list-tail make-bytevector make-parameter
         make-vector max memq min negative? not number->string numerator
         open-input-bytevector open-output-bytevector or output-port?
         parameterize peek-u8 positive? quasiquote quotient raise-continuable
         rationalize read-bytevector! read-error? read-string real? reverse
         set! set-cdr! string string->number string->utf8 string-append
         eof-object eq? eqv? error-object-irritants error-object? exact
         exact-integer? expt file-error? floor-quotient floor/ for-each
         get-output-bytevector guard include inexact input-port-open?
         integer->char lambda length let* let-syntax letrec letrec-syntax
         list->string list-copy list-set! list? make-list make-string map
         member memv modulo newline null? number? odd? open-input-string
         open-output-string output-port-open? pair? peek-char port?
         procedure? quote raise rational? read-bytevector read-char read-line
         read-u8 remainder round set-car! square string->list string->symbol
         string->vector string-copy string-copy! string-for-each string-map
         string-set! string<? string>=? string? symbol->string symbol?
         syntax-rules truncate truncate-remainder u8-ready? unquote
         utf8->string vector vector->string vector-copy vector-fill!
         vector-length vector-ref vector? with-exception-handler write-char
         write-u8 string-fill! string-length string-ref string<=?
         string=? string>? substring symbol=? syntax-error textual-port?
         truncate-quotient truncate/ unless unquote-splicing values
         vector->list vector-append vector-copy! vector-for-each vector-map
         vector-set! when write-bytevector write-string zero?)))

  (define geiser-chicken-builtin-symbols
    (make-parameter
     '(and-let* assume compiler-typecase cond-expand condition-case cut cute declare define-constant
                define-inline define-interface define-record define-record-type define-specialization
                define-syntax-rule define-type define-values dotimes ecase fluid-let foreign-lambda
                foreign-lambda* foreign-primitive foreign-safe-lambda foreign-safe-lambda* functor
                handle-exceptions import let*-values let-location let-optionals let-optionals*
                let-values letrec* letrec-values match-letrec module parameterize regex-case
                require-extension select set! unless use when with-input-from-pipe match
                match-lambda match-lambda* match-let match-let* receive)))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Utilities
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define find-module ##sys#find-module)
  (define current-module ##sys#current-module)
  (define switch-module ##sys#switch-module)
  (define module-name ##sys#module-name)
  (define (list-modules) (map car ##sys#module-table))

  (define (write-to-log form) #f)
  (define debug-log (make-parameter #f))

  (if use-debug-log
   (begin
     (define (write-to-log form)
       (when (not (debug-log))
         (debug-log (file-open "~/geiser-log.txt" (+ open/wronly open/append open/text open/creat)))
         (set-file-position! (debug-log) 0 seek/end))
       (file-write (debug-log) (with-all-output-to-string (lambda () (write form) (newline))))
       (file-write (debug-log) "\n"))))

  ;; This really should be a chicken library function
  (define (write-exception exn)
    (define (write-call-entry call)
      (let ((type (vector-ref call 0))
            (line (vector-ref call 1)))
        (cond
         ((equal? type "<syntax>")
          (display (string-append type " ")) (write line) (newline))
         ((equal? type "<eval>")
          (display (string-append type "   ")) (write line) (newline)))))

    (display (format "Error: (~s) ~s: ~s"
                     ((condition-property-accessor 'exn 'location) exn)
                     ((condition-property-accessor 'exn 'message) exn)
                     ((condition-property-accessor 'exn 'arguments) exn)))
    (newline)
    (display "Call history: ") (newline)
    (map write-call-entry ((condition-property-accessor 'exn 'call-chain) exn))
    (newline))

  ;; And this should be a chicken library function as well
  (define (with-all-output-to-string thunk)
    (with-output-to-string
      (lambda ()
        (with-error-output-to-port
         (current-output-port)
         thunk))))

  (define (maybe-call func val)
    (if val (func val) #f))

  (define (make-apropos-regex prefix)
    (string-append "^([^#]+#)*" (regexp-escape prefix)))

  (define (describe-symbol sym #!key (exact? #f))
    (let* ((str (->string sym))
           (found (apropos-information-list (regexp (make-apropos-regex str)) #:macros? #t)))
      (delete-duplicates
       (if exact?
           (filter (lambda (v)
                     (equal? str (string-substitute ".*#([^#]+)" "\\1" (symbol->string (car v)))))
                   found)
           found))))

  ;; Wraps output from geiser functions
  (define (call-with-result module thunk)
    (let* ((result (if #f #f))
           (output (if #f #f))
           (module (maybe-call (lambda (v) (find-module module)) module))
           (original-module (current-module)))

      (set! output
            (handle-exceptions exn
             (with-all-output-to-string
              (lambda () (write-exception exn)))
             (with-all-output-to-string
              (lambda ()
                (switch-module module)
                (call-with-values thunk (lambda v (set! result v)))))))

      (switch-module original-module)

      (set! result
        (cond
         ((list? result)
          (map (lambda (v) (with-output-to-string (lambda () (pretty-print v)))) result))
         ((eq? result (if #f #t))
          (list output))
         (else
          (list (with-output-to-string (lambda () (pretty-print result)))))))

      (let ((out-form
             `((result ,@result)
               (output . ,output))))
        (write out-form)
        (write-to-log out-form))

      (newline)))

  (define geiser-toplevel-functions (make-parameter '()))

  ;; This macro aids in the creation of toplevel definitions for the interpreter which are also available to code
  ;; toplevel passes parameters via the current-input-port, and so in order to make the definition behave nicely
  ;; in both usage contexts I defined a (get-arg) function which iteratively pulls arguments either from the
  ;; input port or from the variable arguments, depending on context.
  (define-syntax define-toplevel-for-geiser
    (lambda (f r c)
      (let* ((name (cadr f))
             (body (cddr f)))
        `(begin
           (,(r 'define) (,name . !!args)
            (,(r 'define) !!read-arg (null? !!args))
            (,(r 'define) (get-arg)
             (if !!read-arg
                 (read)
                 (let ((arg (car !!args)))
                   (set! !!args (cdr !!args))
                   arg)))
            (begin ,@body))
           (,(r 'geiser-toplevel-functions) (cons (cons ',name ,name) (geiser-toplevel-functions)))))))

  (define (find-standards-with-symbol sym)
    (append
     (if (any (cut eq? <> sym) (geiser-r4rs-symbols))
         '(r4rs)
         '())
     (if (any (cut eq? <> sym) (geiser-r5rs-symbols))
         '(r5rs)
         '())
     (if (any (cut eq? <> sym) (geiser-r7rs-small-symbols))
         '(r7rs)
         '())
     (if (any (cut eq? <> sym) (geiser-chicken-builtin-symbols))
         '(chicken)
         '())))

  ;; Locates any paths at which a particular symbol might be located
  (define (find-library-paths sym types)
    ;; Removes the given sym from the node path
    (define (remove-self sym path)
      (cond
       ((not (list? path)) path)
       ((null? path) path)
       ((null? (cdr path))
        (if (eq? (car path) sym)
            '()
            path))
       (else
        (cons (car path) (remove-self sym (cdr path))))))

    (append
     (map
      (cut list <>)
      (find-standards-with-symbol sym))
     (map
      (lambda (node)
        (remove-self sym (node-path node)))
      (filter
       (lambda (n)
         (let ((type (node-type n)))
           (any (cut eq? type <>) types)))
       (match-nodes sym)))))

  ;; Builds a signature list from an identifier
  (define (find-signatures toplevel-module sym)
    (define str (->string sym))

    (define (make-module-list sym module-sym)
      (if (null? module-sym)
          (find-standards-with-symbol sym)
          (cons module-sym (find-standards-with-symbol sym))))

    (define (fmt node)
      (let* ((entry-str (car node))
             (module (cadr node))
             (rest (cddr node))
             (type (if (or (list? rest) (pair? rest)) (car rest) rest)))
        (cond
         ((equal? 'macro type)
          `(,entry-str ("args" (("required" <macro>)
                                ("optional" ...)
                                ("key")))
                       ("module" ,@(make-module-list sym module))))
         ((or (equal? 'variable type)
              (equal? 'constant type))
          (if (null? module)
              `(,entry-str ("value" . ,(eval sym)))
              (let* ((original-module (current-module))
                     (desired-module (find-module (string->symbol module)))
                     (value (begin (switch-module desired-module)
                                   (eval sym))))
                (switch-module original-module)
                `(,entry-str ("value" . ,value)
                             ("module" ,@(make-module-list sym module))))))
         (else
          (let ((reqs '())
                (opts '())
                (keys '())
                (args (if (or (list? rest) (pair? rest)) (cdr rest) '())))

            (define (clean-arg arg)
              (string->symbol (string-substitute "(.*[^0-9]+)[0-9]+" "\\1" (->string arg))))

            (define (collect-args args #!key (reqs? #t) (opts? #f) (keys? #f))
              (when (not (null? args))
                (cond
                 ((or (pair? args) (list? args))
                  (cond
                   ((eq? '#!key (car args))
                    (collect-args (cdr args) reqs?: #f opts?: #f keys?: #t))
                   ((eq? '#!optional (car args))
                    (collect-args (cdr args) reqs?: #f opts?: #t keys?: #f))
                   (else
                    (begin
                      (cond
                       (reqs?
                        (set! reqs (append reqs (list (clean-arg (car args))))))
                       (opts?
                        (set! opts (append opts (list (cons (clean-arg (caar args)) (cdar args))))))
                       (keys?
                        (set! keys (append keys (list (cons (clean-arg (caar args)) (cdar args)))))))
                      (collect-args (cdr args))))))
                 (else
                  (set! opts (list (clean-arg args) '...))))))

            (collect-args args)

            `(,entry-str ("args" (("required" ,@reqs)
                                  ("optional" ,@opts)
                                  ("key" ,@keys)))
                         ("module" ,@(make-module-list sym module))))))))

    (define (find sym)
      (map
       (lambda (s)
         ;; Remove egg name and add module
         (let* ((str (symbol->string (car s)))
                (name (string-substitute ".*#([^#]+)" "\\1" str))
                (module
                    (if (string-search "#" str)
                        (string-substitute "^([^#]+)#[^#]+$" "\\1" str)
                        '())))
           (cons name (cons module (cdr s)))))
       (describe-symbol sym exact?: #t)))

    (map fmt (find sym)))

  ;; Builds the documentation from Chicken Doc for a specific symbol
  (define (make-doc symbol #!optional (filter-for-type #f))
    (with-output-to-string
      (lambda ()
        (map (lambda (node)
               (display (string-append "= Node: " (->string (node-id node)) " " " =\n"))
               (describe node)
               (display "\n\n"))
             (filter
              (lambda (n)
                (or (not filter-for-type)
                    (eq? (node-type n) filter-for-type)))
              (match-nodes symbol))))))

  (define (make-geiser-toplevel-bindings)
    (map
     (lambda (pair)
       (toplevel-command (car pair) (cdr pair)))
     (geiser-toplevel-functions)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Geiser toplevel functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;; Basically all non-core functions pass through geiser-eval

  (define-toplevel-for-geiser geiser-eval
    ;; We can't allow nested module definitions in Chicken
    (define (form-has-module? form)
      (let ((reg "\\( *module +|\\( *define-library +"))
        (string-search reg form)))

    ;; Chicken doesn't support calling toplevel functions through eval,
    ;; So when we're in a module or calling into an environment we have
    ;; to first call from the toplevel environment and then switch
    ;; into the desired env.
    (define (form-has-geiser? form)
      (let ((reg "\\( *geiser-"))
        (string-search reg form)))

    ;; All calls start at toplevel
    (let* ((module (get-arg))
           (form (get-arg))
           (str-form (format "~s" form))
           (is-module? (form-has-module? str-form))
           (is-geiser? (form-has-geiser? str-form))
           (host-module (and (not is-module?)
                             (not is-geiser?)
                             (any (cut equal? module <>) (list-modules))
                             module)))

      (when (and module (not (symbol? module)))
        (error "Module should be a symbol"))

      ;; Inject the desired module as the first parameter
      (when is-geiser?
        (let ((module (maybe-call (lambda (v) (symbol->string module)) module)))
          (set! form (cons (car form) (cons module (cdr form))))))

      (define (thunk)
        (eval form))

      (write-to-log form)

      (call-with-result host-module thunk)))

  ;; Load a file

  (define-toplevel-for-geiser geiser-load-file
    (let* ((file (get-arg))
           (file (if (symbol? file) (symbol->string file) file))
           (found-file (geiser-find-file #f file)))
      (call-with-result #f
       (lambda ()
         (when found-file
           (load found-file))))))

  ;; The no-values identity

  (define-toplevel-for-geiser geiser-no-values
    (values))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Miscellaneous
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;; Invoke a newline

  (define (geiser-newline . rest)
    (newline))

  ;; Spawn a server for remote repl access

  (define (geiser-start-server . rest)
    (let* ((listener (tcp-listen 0))
           (port (tcp-listener-port listener)))
      (define (remote-repl)
        (receive (in out) (tcp-accept listener)
          (current-input-port in)
          (current-output-port out)
          (current-error-port out)

          (repl)))

      (thread-start! (make-thread remote-repl))

      (write-to-log `(geiser-start-server . ,rest))
      (write-to-log `(port ,port))

      (write `(port ,port))
      (newline)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Completions, Autodoc and Signature
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define (geiser-completions toplevel-module prefix . rest)
    ;; We search both toplevel definitions and module definitions
    (let* ((prefix (if (symbol? prefix) (symbol->string prefix) prefix))
           (re (regexp (make-apropos-regex prefix))))
      (sort! (map (lambda (sym)
                    ;; Strip out everything before the prefix
                    (string-substitute (string-append ".*(" (regexp-escape prefix) ".*)") "\\1" (symbol->string sym)))
                  (append (apropos-list re #:macros? #t)
                          (geiser-module-completions toplevel-module prefix)))
             string<?)))

  (define (geiser-module-completions toplevel-module prefix . rest)
    (let* ((match (string-append "^" (regexp-escape prefix))))
      (filter (lambda (v) (string-search match (symbol->string v)))
              (list-modules))))

  (define (geiser-autodoc toplevel-module ids . rest)
    (define (generate-details sym)
      (find-signatures toplevel-module sym))

    (if (list? ids)
        (foldr append '()
               (map generate-details ids))
        '()))

  (define (geiser-object-signature toplevel-module name object . rest)
    (let* ((sig (geiser-autodoc toplevel-module `(,name))))
      (if (null? sig) '() (car sig))))

    ;; TODO: Divine some way to support this functionality

  (define (geiser-symbol-location toplevel-module symbol . rest)
    '(("file") ("line")))

  (define (geiser-symbol-documentation toplevel-module symbol . rest)
    (let* ((sig (find-signatures toplevel-module symbol)))
      `(("signature" ,@(car sig))
        ("docstring" . ,(make-doc symbol)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; File and Buffer Operations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define geiser-load-paths (make-parameter '()))

  (define (geiser-find-file toplevel-module file . rest)
    (let ((paths (append '("" ".") (geiser-load-paths))))
      (define (try-find file paths)
        (cond
         ((null? paths) #f)
         ((file-exists? (string-append (car paths) file))
          (string-append (car paths) file))
         (else (try-find file (cdr paths)))))
      (try-find file paths)))

  (define (geiser-add-to-load-path toplevel-module directory . rest)
    (let* ((directory (if (symbol? directory)
                          (symbol->string directory)
                          directory))
           (directory (if (not (equal? #\/ (string-ref directory (- (string-length directory)))))
                          (string-append directory "/")
                          directory)))
      (call-with-result #f
       (lambda ()
         (when (directory-exists? directory)
           (geiser-load-paths (cons directory (geiser-load-paths))))))))

  (define (geiser-compile-file toplevel-module file . rest)
    (let* ((file (if (symbol? file) (symbol->string file) file))
           (found-file (geiser-find-file toplevel-module file)))
      (call-with-result #f
       (lambda ()
         (when found-file
           (compile-file found-file))))))

    ;; TODO: Support compiling regions

  (define (geiser-compile toplevel-module form module . rest)
    (error "Chicken does not support compiling regions"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Modules
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;; Should return:
  ;; '(("modules" . sub-modules) ("procs" . procedures) ("syntax" . macros) ("vars" . variables))
  (define (geiser-module-exports toplevel-module module-name . rest)
    (let* ((nodes (match-nodes module-name)))
      (if (null? nodes)
          '()
          (let ((mod '())
                (proc '())
                (syn '())
                (var '()))
            (map
             (lambda (node)
               (let ((type (node-type node))
                     (name (node-id node))
                     (path (node-path node)))
                 (cond
                  ((memq type '(unit egg))
                   (set! mod (cons name mod)))
                  ((memq type '(procedure record setter class method))
                   (set! proc (cons name proc)))
                  ((memq type '(read syntax))
                   (set! syn (cons name syn)))
                  ((memq type '(parameter constant))
                   (set! var (cons name var))))))
             nodes)
            `(("modules" . ,mod)
              ("proces" . ,proc)
              ("syntax" . ,syn)
              ("vars" . ,var))))))

  ;; Returns the path for the file in which an egg or module was defined

  (define (geiser-module-path toplevel-module module-name . rest)
    #f)

  ;; Returns:
  ;; `(("file" . ,(module-path name)) ("line"))

  (define (geiser-module-location toplevel-module name . rest)
    #f)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Misc
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define (geiser-macroexpand toplevel-module form . rest)
    (with-output-to-string
      (lambda ()
        (pretty-print (expand form)))))

;; End module
  )

(import geiser)
(make-geiser-toplevel-bindings)
