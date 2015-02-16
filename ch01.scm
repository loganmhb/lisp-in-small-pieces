(define (evaluate e env)
  (if (atom? e)
      (cond ((symbol? e) (lookup e env))
            ((or (number? e) (string? e) (char? e) (boolean? e) (vector? e))
             e)
            (else (wrong "Cannot evaluate" e)))
      (case (car e)
        ((quote)  (cadr e))
        ((if)     (if (evaluate (cadr e) env)
                      (evaluate (caddr e) env)
                      (evaluate (cadddr e) env)))
        ((begin)  (eprogn (cdr e) env))
        ((set!)   (update! (cadr e) env (evaluate (caddr e) env)))
        ((lambda) (make-function (cadr e) (cddr e) env))
        (else     (invoke (evaluate (car e) env)
                          (evlis (cdr e) env))))))


(define (eprogn exprs env)
  (if (pair? exprs)
      (if (pair? (cdr exprs))
          (begin (evaluate (car exprs) env)
                 (eprogn (cdr exprs) env))
          (evaluate (car exprs) env))
      empty-begin))

(define empty-begin 813)


(define (evlis exprs env)
  (if (pair? exprs)
      (cons (evaluate (car exprs) env)
            (evlis (cdr exprs) env))
      '()))


(define (lookup id env)
  (if (pair? env)
      (if (eq? (caar env) id)
          (cdar env)
          (lookup id (cdr env)))
      (wrong "No such binding" id)))


(define (update! id env value)
  (if (pair? env)
      (if (eq? (caar env) id)
          (begin (set! (cdr (car env)) value)
                 value)
          (update! id (cdr env) value))
      (wrong "No such binding" id)))


(define (extend env variables values)
  (cond ((pair? variables)
         (if (pair? values)
             (cons (cons (car variables) (car values))
                   (extend env (cdr variables) (cdr values)))
             (wrong "Too few values")))
        ((null? variables)
         (if (null? values)
             env
             (wrong "Too many values.")))
        ((symbol? variables) (cons (cons variables values) env))))


(define (invoke fn args)
  (if (procedure? fn)
      (fn args)
      (wrong "Not a function" fn)))


(define (make-function variables body env)
  (lambda (values)
    (eprogn body (extend env variables values))))


(define env.global env.init)


(define-syntax definitial
  (syntax-rules ()
    ((definitial name)
     (begin (set! env.global (cons (cons 'name 'void) env.global))
            'name)
     ((definitial name value)
      (begin (set! env.global (cons (cons 'name value) env.global))
             'name)))))


(define-syntax defprimitive
  (syntax-rules ()
    ((defprimitive name value arity)
     (definitial name
       (lambda (values)
         (if (= arity (length values))
             (apply value values)
             (wrong "Incorrect arity.")))))))


(define the-false-value (cons "false" "value"))


(definitial t #t)
(definitial f the-false-value)
(definitial foo)
(definitial bar)
(definitial fib)
(definitial fact)

(defprimitive cons cons 2)
(defprimitive car car 1)
(defprimitive cdr cdr 1)
(defprimitive + + 2)
(defprimitive eq? eq? 2)
(defprimitive < < 2)

(define (chapter1-scheme)
  (define (toplevel)
    (display (evaluate (read) env.global))
    (toplevel))
  (toplevel))

