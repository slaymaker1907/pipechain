#lang racket/base
(provide (all-from-out (submod "." v1_0)))
(require syntax/location)

(require syntax/srcloc)
(define loc (quote-module-path))

(module v1_0 racket/base
  (require (only-in racket/stxparam
                    define-syntax-parameter
                    syntax-parameterize))
  (require (only-in racket/splicing
                    splicing-let))
  (require (only-in syntax/parse/define
                    define-syntax-parser))
  (require (for-syntax racket/base))
  (require (for-syntax (only-in syntax/parse
                                syntax-parse
                                id
                                ~and
                                ~not
                                ...+
                                define-syntax-class
                                pattern)))
  (provide ~> splicing~> ~>effect ~>define)
  (provide (rename-out [underscore _]))

  (define-syntax-parameter underscore (make-rename-transformer #'_))
  (define (~>effect val effect)
    (effect val)
    val)

  (begin-for-syntax
    (define-syntax-class notid
      (pattern (~and val (~not name:id)))))

  (define-syntax-parser ~>define
    [(_ header body ...+)
     #'(define header (~> body ...))])

  (define-syntax-parser ~> #:literals (define ~>effect)
    [(_ val) #'val]
    [(_ val:id (~>effect effect))
     #'(begin
         (~> val effect)
         val)]
    [(_ val func:id) #'(func val)]
    [(_ val:id new-expr:notid)
     #'(syntax-parameterize ([underscore (make-rename-transformer #'val)])
         new-expr)]
    [(_ val:notid new-expr:notid)
     #'(let ([val-var val])
         (~> val-var new-expr))]
    [(_ val (define name:id) rest ...+)
     #'(let ([ name val])
         (~> name rest ...))]
    [(_ val first rest ...+)
     #'(~> (~> val first) rest ...)]
    [(_ val:notid rest ...+) ;; Lazily introduce let bindings as necessary.
     #'(let ([val-var val])
         (~> val-var rest ...))])
  
  (define-syntax-parser splicing~> #:literals (define ~>effect)
    [(_ val) #'val]
    [(_ val (define name:id))
     #'(define name val)]
    [(_ val:id (~>effect effect))
     #'(begin
         (~> val effect)
         val)]
    [(_ val func:id) #'(func val)]
    [(_ val:id new-expr:notid)
     #'(syntax-parameterize ([underscore (make-rename-transformer #'val)])
         new-expr)]
    [(_ val:notid new-expr:notid)
     #'(let ([val-var val])
         (~> val-var new-expr))]
    [(_ val (define name:id) rest ...+)
     #'(begin
         (define name val)
         (splicing~> name rest ...))]
    [(_ val first rest ...+)
     #'(splicing~> (~> val first) rest ...)]
    [(_ val:notid rest ...+) ;; Lazily introduce let bindings as necessary.
     #'(splicing-let ([val-var val])
         (~> val-var rest ...))]))
(require (submod "." v1_0))

(module test racket/base
  (require (submod ".." v1_0))
  (require (only-in rackunit
                    check-equal?))
  (require (only-in data/gvector
                    gvector
                    gvector-add!))
  (require (only-in racket/splicing))
  (require (only-in racket/function
                    const
                    identity))
  (check-equal? (~> 45742655) 45742655)
  (check-equal? (~> "myval") "myval")
  (define ((make-args-inner func) kws kw-args . normal-args)
    (define kwhash
      (for/hash ([kwname (in-list kws)]
                 [kwval (in-list kw-args)])
        (values kwname kwval)))
    (hash 'keyword-args kwhash
          'normal-args normal-args
          'func func))
  (define (make-args func)
    (make-keyword-procedure (make-args-inner func)))
    
  (define (make-call-tracker #:tracker tracker-gvector
                             #:name name
                             #:retval retval)
    (define real-proc-mock (box #f))
    (define (proc-mock . args)
      (gvector-add! tracker-gvector (apply (make-args-inner (unbox real-proc-mock)) args))
      retval)
    (define result (procedure-rename (make-keyword-procedure proc-mock) name))
    (set-box! real-proc-mock result)
    result)
    
  (let* ([calls (gvector)]
         [x1 (gensym "x1")]
         [x2 (gensym "x2")]
         [func (make-call-tracker #:tracker calls #:name 'func #:retval x2)]
         [result (~> x1 func)])
    (check-equal? result x2)
    (check-equal? calls (gvector ((make-args func) x1))))
  
  (let* ([calls (gvector)]
         [x1 (gensym "x1")]
         [x2 (gensym "x2")]
         [x3 (gensym "x3")]
         [x4 (gensym "x4")]
         [func1 (make-call-tracker #:tracker calls #:name 'func1 #:retval x2)]
         [func2 (make-call-tracker #:tracker calls #:name 'func2 #:retval x3)]
         [result (~> (func1 x1) (func2 _ x4 #:arg _))])
    (check-equal? result x3)
    (check-equal? calls (gvector ((make-args func1) x1)
                                 ((make-args func2) x2 x4 #:arg x2))))
  (let* ([calls (gvector)]
         [x1 (gensym "x1")]
         [x2 (gensym "x2")]
         [func (make-call-tracker #:tracker calls #:name 'func #:retval x2)]
         [result (~> x1 (define temp) func (cons _ temp) func)])
    (check-equal? result x2)
    (check-equal? calls (gvector ((make-args func) x1)
                                 ((make-args func) (cons x2 x1)))))
  (let* ([x1 (gensym "x1")]
         [x2 (gensym "x2")]
         [func (const x2)])
    (splicing~> x1 func (define result1) (identity x1) (define result2))
    (check-equal? result1 x2)
    (check-equal? result2 x1))
  (void))
