#lang racket/base
#;(require (for-syntax (submod "." 'require-utils)))

(module require-utils racket/base
  (require (for-syntax racket/base))
  (require (only-in syntax/parse/define
                    define-syntax-parser))
  (require (for-syntax (only-in syntax/parse
                                pattern
                                ...+
                                define-syntax-class
                                id
                                ~datum)))
  (require (for-syntax (only-in racket/syntax format-id)))
  (provide require-safe require-safe*)
  
  (begin-for-syntax
    (define-syntax-class maybe-renamed
      (pattern [orig-name:id (~datum as) new-name:id]
               #:with for-only-in #'[orig-name new-name])
      (pattern orig-name:id
               #:with for-only-in #'orig-name)))
  (define-syntax-parser require-safe #:literals (*)
    [(_ prefix ... #:for-syntax suffix ...)
     #'(begin-for-syntax
         (require-safe prefix ... suffix ...))]
    [(_)
     #'(void)]
    [(_ req-spec:id)
     #'(void)]
    [(_ * (~datum as) prefix-id:id (~datum from) req-spec) ;; This form is slightly not safe
     (let ([plus-: (format-id #f "~a:" #'prefix-id #:source #'prefix-id)])
       #`(require (prefix-in #,plus-: req-spec)))]
    [(_ name:id (~datum as) new-name:id (~datum from) req-spec)
     #'(require (only-in req-spec [name new-name]))]
    [(_ prefix ... [* (~datum as) prefix-id:id] suffix ... (~datum from) req-spec)
     #'(begin
         (require-safe * as prefix-id from req-spec)
         (require-safe prefix ... suffix ... from req-spec))]
    [(_ name:maybe-renamed ...+ (~datum from) req-spec)
     #'(require (only-in req-spec name.for-only-in ...))])
  (define-syntax-parser require-safe*
    [(_ prefix ... #:for-syntax suffix ...)
     #'(begin-for-syntax
         (require-safe* prefix ... suffix ...))]
    [(_)
     #'(void)]
    [(_ prefix ... (spec) suffix ...)
     #'(require-safe* prefix ... suffix ...)]
    [(_ (require-safe-args ...) ...+)
     #'(begin
         (require-safe require-safe-args ...) ...)]))

(module define-proc racket/base
  (require (for-syntax racket/base))
  (require (submod ".." require-utils))
  (require-safe * as stxparam from racket/stxparam)
  (require-safe define-syntax-parser from syntax/parse/define)
  (begin-for-syntax
    (require (submod ".." require-utils))
    (require-safe function-header from syntax/parse/lib/function-header)
    (require-safe ...+ from syntax/parse))
  (provide return define-proc)
  
  (stxparam:define-syntax-parameter return (λ _ #'undefined))

  (define-syntax-parser define-proc
    [(_ head:function-header body ...+)
     #'(define head
         (let/ec return-func
           (stxparam:syntax-parameterize ([return (make-rename-transformer #'return-func)])
                                         body ...)))]))

(module ifx-utils racket/base
  (require (for-syntax racket/base))
  (require (submod ".." require-utils))
  (require-safe return define-proc from (submod ".." define-proc))
  (require-safe match-define match from racket/match)
  (require-safe define-syntax-parser from syntax/parse/define)
  (require-safe define-syntax-parameter from racket/stxparam)
  (require-safe split-at from racket/list)
  (begin-for-syntax
    (require (submod ".." require-utils))
    (require-safe ~not ~and ...+ id ~or* define-syntax-class pattern
                  from syntax/parse))
  (provide ifx ^ ifx:/ runrpn (struct-out runrpn-op))
  
  (define (force-exact num)
    (if (exact? num)
        num
        (inexact->exact num)))
  (define (force-inexact num)
    (if (inexact? num)
        num
        (exact->inexact num)))

  (define-proc (coerce-fl-/ arg0 . rest-args)
    (let ([result (apply / arg0 rest-args)])
      (unless (real? result)
        (return result))
      (if (integer? result)
          (force-exact result)
          (force-inexact result))))

  (define-syntax ^ (make-rename-transformer #'expt))
  (define-syntax-parameter ifx:/ (make-rename-transformer #'coerce-fl-/))

  (define-syntax-parser ifx-maybe-inner #:literals (+ - * / ^)
    [(_ head (~and item (~not +) (~not -) (~not *) (~not /) (~not ^)) ...+)
     #'(head item ...)]
    [(_ ifx-args ...+)
     #'(ifx ifx-args ...)])

  (begin-for-syntax
    (define-syntax-class is*/ #:literals (* /)
      (pattern * #:with op #'*)
      (pattern / #:with op #'ifx:/)))

  (struct runrpn-op (val))

  (define-proc (runrpn #:stack [stack '()] . ops)
    (when (null? ops)
      (if (null? stack)
          (return (void))
          (return (car stack))))
    (match-define (cons op ops-tail) ops)
    (when (equal? op /)
      (set! op ifx:/))
    (cond
      [(member op (list ^ * ifx:/ + -))
       (match stack
         [(list* arg2 arg1 stack-tail)
          (apply runrpn ops-tail #:stack (cons (op arg1 arg2) stack-tail))]
         [_ (apply runrpn ops-tail #:stack (list (apply op (reverse stack))))])]
      [(eq? op apply)
       (match-define (cons next-op next-ops-tail) ops-tail)
       (define new-ops (cons (λ (args) (apply next-op args)) next-ops-tail))
       (apply runrpn new-ops #:stack (list stack))]
      [(procedure? op)
       (define op-arity (procedure-arity op))
       (define first-arity (if (list? op-arity)
                               (car op-arity)
                               op-arity))
       (define min-arity
         (match first-arity
           [(arity-at-least x) x]
           [x x]))
       (define-values (op-args stack-tail) (split-at stack min-arity))
       (define op-result (call-with-values (λ _ (apply op (reverse op-args)))
                                           list))
       (define-values (new-stack new-ops)
         (for/fold ([new-stack stack-tail]
                    [new-ops ops-tail])
                   ([val-or-op (in-list op-result)])
           (match val-or-op
             [(runrpn-op new-op) (values new-stack (cons new-op new-ops))]
             [val (values (cons val new-stack) new-ops)])))
       (apply runrpn new-ops #:stack new-stack)]
      [else
       (apply runrpn ops-tail #:stack (cons op stack))]))

  (define-syntax-parser ifx #:literals (+ - * / ^)
    [(_ (item ...+))
     #'(ifx-maybe-inner item ...)]
    [(_ val)
     #'val]
    [(_ prefix ... left ^ right suffix ...)
     #'(ifx prefix ... (^ (ifx left) (ifx right)) suffix ...)]
    [(_ (~and prefix (~not (~or* * /))) ... left op:is*/ right suffix ...)
     #'(ifx prefix ... (op.op (ifx left) (ifx right)) suffix ...)]
    [(_ (~and prefix (~not (~or* + -))) ... left op:is*/ right suffix ...)
     #'(ifx prefix ... (op.op (ifx left) (ifx right)) suffix ...)]))

(require (submod "." require-utils))
(require (submod "." define-proc))
(require (submod "." ifx-utils))
(provide (all-from-out (submod "." require-utils)))
(provide (all-from-out (submod "." define-proc)))
(provide (all-from-out (submod "." ifx-utils)))
