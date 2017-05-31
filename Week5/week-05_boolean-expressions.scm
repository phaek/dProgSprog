;;; week-05_boolean-expressions.scm
;;; dProgSprog 2016-2017, Q4
;;; Olivier Danvy <danvy@cs.au.dk>
;;; Tue 09 May 2017

;;;;;;;;;;

(define week-05_boolean-expressions
  #t)

;;;;;;;;;;

(define make-none
  (lambda (x)
    (list 'none)))

(define is-none?
  (lambda (v)
    (and (pair? v)
         (equal? (car v) 'none)
         (null? (cdr v)))))

(define make-some
  (lambda (x)
    (list 'some x)))

(define is-some?
  (lambda (v)
    (and (pair? v)
         (equal? (car v) 'some)
         (let ([rest (cdr v)])
           (and (pair? rest)
                (null? (cdr rest)))))))

(define some-1
  (lambda (v)
    (list-ref v 1)))

(define empty-list
  '())

(define extend-list
  (lambda (x d env)
    (cons (cons x d) env)))(define empty-list
  (lambda ()
    '()))

(define lookup-list
  (lambda (x env_init)
    (letrec ([traverse (lambda (env)
                         (cond
                           [(null? env)
                            (make-none)]
                           [(pair? env)
                            (let ([p (car env)])
                              (let ([y (car p)]
                                    [d (cdr p)])
                                (if (equal? x y)
                                    (make-some d)
                                    (traverse (cdr env)))))]
                           [else
                            (errorf 'lookup-list
                                    "not a proper list: ~s"
                                    env)]))])
      (traverse env_init))))

;;;;;;;;;;

;;; <boolean-expression> ::= (identifier <symbol>)
;;;                        | (conjunction <boolean-expression> <boolean-expression>)
;;;                        | (disjunction <boolean-expression> <boolean-expression>)
;;;                        | (negation <boolean-expression>)

;;; constructors:

(define make-identifier
  (lambda (x)
    (list 'identifier x)))

(define make-conjunction
  (lambda (e1 e2)
    (list 'conjunction e1 e2)))

(define make-disjunction
  (lambda (e1 e2)
    (list 'disjunction e1 e2)))

(define make-negation
  (lambda (e)
    (list 'negation e)))

;;; predicates:

(define is-identifier?
  (lambda (v)
    (and (pair? v)
         (equal? (car v) 'identifier)
         (let ([rest (cdr v)])
           (and (pair? rest)
                (null? (cdr rest)))))))

(define is-conjunction?
  (lambda (v)
    (and (pair? v)
         (equal? (car v) 'conjunction)
         (let ([rest (cdr v)])
           (and (pair? rest)
                (let ([rest (cdr rest)])
                  (and (pair? rest)
                       (null? (cdr rest)))))))))

(define is-disjunction?
  (lambda (v)
    (and (pair? v)
         (equal? (car v) 'disjunction)
         (let ([rest (cdr v)])
           (and (pair? rest)
                (let ([rest (cdr rest)])
                  (and (pair? rest)
                       (null? (cdr rest)))))))))

(define is-negation?
  (lambda (v)
    (and (pair? v)
         (equal? (car v) 'negation)
         (let ([rest (cdr v)])
           (and (pair? rest)
                (null? (cdr rest)))))))

;;; accessors:

(define identifier_1
  (lambda (v)
    (list-ref v 1)))

(define conjunction_1
  (lambda (v)
    (list-ref v 1)))

(define conjunction_2
  (lambda (v)
    (list-ref v 2)))

(define disjunction_1
  (lambda (v)
    (list-ref v 1)))

(define disjunction_2
  (lambda (v)
    (list-ref v 2)))

(define negation_1
  (lambda (v)
    (list-ref v 1)))

;;;;;;;;;;

;;; syntax checker:

(define positive-test-check-boolean-expression
  (lambda (candidate)
    (and (equal? (candidate (make-identifier 'x)) #t)
         (equal? (candidate (make-conjunction (make-negation (make-identifier 'x)) (make-identifier 'y))) #t)
         (equal? (candidate (make-disjunction (make-negation (make-identifier 'x)) (make-identifier 'y))) #t) ;test added
         (equal? (candidate (make-negation (make-negation (make-identifier 'x)))) #t) ;test added
         ;;; etc.
         )))

;;; Exercise: add a few more tests in positive-test-check-boolean-expression.

(define negative-test-check-boolean-expression
  (lambda (candidate)
    (and (equal? (candidate '(identifier)) #f)
         (equal? (candidate '(identifier . foo)) #f)
         (equal? (candidate '(identifier 10)) #f)
         (equal? (candidate '(identifier x . bar)) #f)
         (equal? (candidate '(identifier x y)) #f)
         (equal? (not (make-negation (candidate (make-negation (make-negation (make-identifier 'x)))))) #f) ;test added
         (equal? (not (candidate (make-negation (make-negation (make-identifier 'x))))) #f) ;test added
         ;;; etc.
         )))

;;; Exercise: add a few more tests in negative-test-check-boolean-expression.

(define check-boolean-expression
  (lambda (v)
    (cond
      [(is-identifier? v)
       (symbol? (identifier_1 v))]
      [(is-conjunction? v)
       (and (check-boolean-expression (conjunction_1 v))
            (check-boolean-expression (conjunction_2 v)))]
      [(is-disjunction? v)
       (and (check-boolean-expression (disjunction_1 v))
            (check-boolean-expression (disjunction_2 v)))]
      [(is-negation? v)
       (check-boolean-expression (negation_1 v))]
      [else
       #f])))

(define week-05_boolean-expressions
  (and (positive-test-check-boolean-expression check-boolean-expression) ;#t
       (negative-test-check-boolean-expression check-boolean-expression) ;#t
       week-05_boolean-expressions))

;;;;;;;;;;

(define lookup
  (lambda (x env)
    (let ([d (lookup-list x env)])
      (cond
        [(is-none? d)
         (errorf 'lookup
                 "unbound identifier: ~s"
                 x)]
        [(is-some? d)
         (some-1 d)]
        [else
         (errorf 'lookup
                 "weird lookup, man")]))))

(define interpret-boolean-expression
  (lambda (v env)
    (cond
      [(is-identifier? v)
       (lookup (identifier_1 v) env)]
      [(is-conjunction? v)
       (and (interpret-boolean-expression (conjunction_1 v) env)
            (interpret-boolean-expression (conjunction_2 v) env))]
      [(is-disjunction? v)
       (or (interpret-boolean-expression (conjunction_1 v) env)
           (interpret-boolean-expression (conjunction_2 v) env))]
      [(is-negation? v)
       (not (interpret-boolean-expression (negation_1 v) env))]
      [else
       (errorf 'interpret-boolean-expression
               "not a Boolean expression: ~s"
               v)])))

;;;;;;;;;;

;;; the art of the unit test:

(define enumerate
  (lambda (candidate e envs_init)
    (letrec ([traverse (lambda (envs)
                         (cond
                           [(null? envs)
                            #t]
                           [(pair? envs)
                            (and (candidate e (car envs))
                                 (traverse (cdr envs)))]
                           [else
                            (errorf 'enumerate
                                    "improper list of environments: ~s"
                                    envs)]))])
      (traverse envs_init))))

(define enumerate_neg
  (lambda (candidate e envs_init)
    (letrec ([traverse (lambda (envs)
                         (cond
                           [(null? envs)
                            #f]
                           [(pair? envs)
                            (and (candidate e (car envs))
                                 (traverse (cdr envs)))]
                           [else
                            (errorf 'enumerate
                                    "improper list of environments: ~s"
                                    envs)]))])
      (traverse envs_init))))

(define splice-in
  (lambda (x b envs_init)
    (letrec ([visit (lambda (envs)
                      (cond
                        [(null? envs)
                         '()]
                        [(pair? envs)
                         (cons (extend-list x b (car envs))
                               (visit (cdr envs)))]
                        [else
                         (errorf 'splice-in
                                 "not a proper list of environments: ~s"
                                 envs)]))])
      (visit envs_init))))

(define all-possible-environments
  (lambda (xs)
    (cond
      [(null? xs)
       (list empty-list)]
      [(pair? xs)
       (let ([your-induction-hypothesis-at-work (all-possible-environments (cdr xs))])
         (append (splice-in (car xs) #t your-induction-hypothesis-at-work)
                 (splice-in (car xs) #f your-induction-hypothesis-at-work)))]
      [else
       (errorf 'all-possible-environments
               "not a proper list of names: ~s"
               xs)])))

(define implies
  (lambda (e1 e2)
    (make-disjunction (make-negation e1) e2)))

(define test-interpret-boolean-expression-with-tautologies
  (lambda (candidate)
    (and (enumerate candidate
                    (make-disjunction (make-negation (make-identifier 'x))
                                      (make-identifier 'x))
                    (all-possible-environments '(x)))
         (enumerate candidate
                    (implies (make-identifier 'x) (make-identifier 'x))
                    (all-possible-environments '(x)))
         (enumerate candidate
                    (implies (make-conjunction (make-identifier 'x)
                                               (make-identifier 'y))
                             (make-disjunction (make-identifier 'x)
                                               (make-identifier 'y)))
                    (all-possible-environments '(x y)))
         (enumerate candidate ;test added
                    (make-disjunction (make-negation (make-negation (make-identifier 'x)))
                                      (make-negation (make-identifier 'x)))
                    (all-possible-environments '(x)))
         )))

;;; Exercise: add more tautologies in test-interpret-boolean-expression-with-tautologies.


(define week-05_boolean-expressions
  (and (test-interpret-boolean-expression-with-tautologies interpret-boolean-expression) ;#t
       week-05_boolean-expressions))

;;;;;;;;;;

;;; Exercise: devise expressions that evaluate to #f in all possible environments,
;;; and use them to implement another unit-test procedure for interpret-boolean-expression.
(define test-interpret-boolean-expression-with-tautologies_not
  (lambda (candidate)
    (and (enumerate_neg candidate
                    (make-disjunction (make-negation (make-identifier 'x))
                                      (make-identifier 'x))
                    (all-possible-environments '(x)))
         (enumerate_neg candidate
                    (implies (make-identifier 'x) (make-identifier 'x))
                    (all-possible-environments '(x)))
         (enumerate_neg candidate
                    (implies (make-conjunction (make-identifier 'x)
                                               (make-identifier 'y))
                             (make-disjunction (make-identifier 'x)
                                               (make-identifier 'y)))
                    (all-possible-environments '(x y)))
         (enumerate_neg candidate
                    (make-disjunction (make-negation (make-negation (make-identifier 'x)))
                                      (make-negation (make-identifier 'x)))
                    (all-possible-environments '(x)))
         )))
         ;;Successfully devised! 
;;;;;;;;;;

;;; negational normal forms:

;;; <boolean-expression_nnf> ::= (identifier_nnf <symbol>)
;;; | (conjunction_nnf <boolean-expression_nnf> <boolean-expression_nnf>)
;;; | (disjunction_nnf <boolean-expression_nnf> <boolean-expression_nnf>)
;;; | (negation_nnf <symbol>)

;;; Exercise: implement this BNF.

;;; constructors:

(define make-identifier_nnf
  (lambda (x)
    (list 'identifier_nnf x)))

(define make-conjunction_nnf
  (lambda (e1 e2)
    (list 'conjunction_nnf e1 e2)))

(define make-disjunction_nnf
  (lambda (e1 e2)
    (list 'disjunction_nnf e1 e2)))

(define make-negation_nnf
  (lambda (x)
    (list 'negation_nnf x)))

;;; predicates:

(define is-identifier_nnf?
  (lambda (v)
    (and (pair? v)
         (equal? (car v) 'identifier_nnf)
         (let ([rest (cdr v)])
           (and (pair? rest)
                (null? (cdr rest)))))))

(define is-conjunction_nnf?
  (lambda (v)
    (and (pair? v)
         (equal? (car v) 'conjunction_nnf)
         (let ([rest (cdr v)])
           (and (pair? rest)
                (let ([rest (cdr rest)])
                  (and (pair? rest)
                       (null? (cdr rest)))))))))

(define is-disjunction_nnf?
  (lambda (v)
    (and (pair? v)
         (equal? (car v) 'disjunction_nnf)
         (let ([rest (cdr v)])
           (and (pair? rest)
                (let ([rest (cdr rest)])
                  (and (pair? rest)
                       (null? (cdr rest)))))))))

(define is-negation_nnf?
  (lambda (v)
    (and (pair? v)
         (equal? (car v) 'negation_nnf)
         (let ([rest (cdr v)])
           (and (pair? rest)
                (null? (cdr rest)))))))
;;; accessors:

(define identifier_nnf_1
  (lambda (v)
    (list-ref v 1)))

(define conjunction_nnf_1
  (lambda (v)
    (list-ref v 1)))

(define conjunction_nnf_2
  (lambda (v)
    (list-ref v 2)))
    
(define disjunction_nnf_1
  (lambda (v)
    (list-ref v 1)))

(define disjunction_nnf_2
  (lambda (v)
    (list-ref v 2)))

(define negation_nnf_1
  (lambda (v)
    (list-ref v 1)))
;;;;;;;;;;

;;; syntax checker:

;;; Exercise: implement the following positive unit-test procedure:

(define positive-test-check-boolean-expression_nnf
  (lambda (candidate)
    (and (equal? (candidate (make-identifier_nnf 'x)) #t) ;test added
         (equal? (candidate (make-negation_nnf (make-identifier 'x))) #t) ;test added
         ;;;etc
         )))


(define negative-test-check-boolean-expression_nnf
  (lambda (candidate)
    (and (equal? (candidate '(identifier_nnf)) #f)
         (equal? (candidate '(identifier_nnf . foo)) #f)
         (equal? (candidate '(identifier_nnf 10)) #f)
         (equal? (candidate '(identifier_nnf x . bar)) #f)
         (equal? (candidate '(identifier_nnf x y)) #f)
         )))
;;;;;

(define check-boolean-expression_nnf
  (lambda (v)
    (cond
      [(is-identifier_nnf? v)
       (symbol? (identifier_nnf_1 v))]
      [(is-conjunction_nnf? v)
       (and (check-boolean-expression_nnf (conjunction_nnf_1 v))
            (check-boolean-expression_nnf (conjunction_nnf_2 v)))]
      [(is-disjunction_nnf? v)
       (and (check-boolean-expression_nnf (disjunction_nnf_1 v))
            (check-boolean-expression_nnf (disjunction_nnf_2 v)))]
      [(is-negation_nnf? v)
       (check-boolean-expression (negation_nnf_1 v))]
      [else
       #f])))
       
(define week-05_boolean-expressions
  (and (positive-test-check-boolean-expression_nnf check-boolean-expression_nnf) ;#t
       (negative-test-check-boolean-expression_nnf check-boolean-expression_nnf) ;#t
       week-05_boolean-expressions))

;;;;;;;;;;

;;; Exercise: implement an interpreter for negational normal forms.
(define interpret-boolean-expression_nnf
  (trace-lambda trace (v env)
    (cond
      [(is-identifier_nnf? v)
       (lookup (identifier_nnf_1 v) env)]
      [(is-conjunction_nnf? v)
       (and (interpret-boolean-expression_nnf (conjunction_nnf_1 v) env)
            (interpret-boolean-expression_nnf  (conjunction_nnf_2 v) env))]
      [(is-disjunction_nnf? v)
       (or (interpret-boolean-expression_nnf  (conjunction_nnf_1 v) env)
           (interpret-boolean-expression_nnf  (conjunction_nnf_2 v) env))]
      [(is-negation_nnf? v)
       (not (interpret-boolean-expression_nnf  (negation_nnf_1 v) env))]
      [else
       (errorf 'interpret-boolean-expression_nnf
               "not a Boolean expression: ~s"
               v)])))
;;;;;;;;;;


;;; Translation from Boolean expression to Boolean expression in negational normal form
;;; (a.k.a. negational normalization):

(define test-normalize-boolean-expression
  (lambda (candidate)
    (and (let ([e (make-negation (make-negation (make-identifier 'x)))]
               [e_nnf (make-identifier_nnf 'x)])
           (equal? (candidate e) e_nnf))
         (let ([e (make-negation (make-negation (make-negation (make-identifier 'x))))]
               [e_nnf (make-negation_nnf 'x)])
           (equal? (candidate e) e_nnf))
         (let ([e (make-negation (make-conjunction (make-identifier 'x) (make-identifier 'y)))]
               [e_nnf (make-disjunction_nnf (make-negation_nnf 'x) (make-negation_nnf 'y))])
           (equal? (candidate e) e_nnf))
         (let ([e (make-negation (make-disjunction (make-identifier 'x) (make-negation (make-identifier 'y))))]
               [e_nnf (make-conjunction_nnf (make-negation_nnf 'x) (make-identifier_nnf 'y))])
           (equal? (candidate e) e_nnf))
         (let ([e (make-negation (make-negation (make-negation (make-conjunction (make-identifier 'x) (make-negation (make-identifier 'y))))))] ;test added
               [e_nnf (make-disjunction_nnf (make-negation_nnf 'x) (make-identifier_nnf 'y))])
           (equal? (candidate e) e_nnf))
         (let ([e (make-negation (make-negation (make-identifier 'x)))] ;test added
               [e_nnf (make-identifier_nnf 'x)])
           (equal? (candidate e) e_nnf))
         (let ([e (make-conjunction (make-identifier 'x) (make-identifier 'y))] ;test added
               [e_nnf (make-conjunction_nnf (make-identifier_nnf 'x) (make-identifier_nnf 'y))])
           (equal? (candidate e) e_nnf))
         ;;; etc.
         )))

;;; Exercise: add a few more tests in test-normalize-boolean-expression.

(define normalize-boolean-expression
  (lambda (v)
    (letrec ([visit (lambda (v env)
            (cond

              [(and (is-identifier? v) env)             
               (make-negation_nnf (identifier_1 v))]
              [(is-identifier? v)
               (make-identifier_nnf (identifier_1 v))]


              [(and (is-conjunction? v) env)
                   (make-disjunction_nnf (visit (disjunction_nnf_1 v) env)
                                         (visit (disjunction_nnf_2 v) env))]              
              [(and (is-conjunction? v) (not env))
               (make-conjunction_nnf (visit (conjunction_nnf_1 v) env)
                                     (visit (conjunction_nnf_2 v) env))]

              [(and (is-disjunction? v) env)
               (make-conjunction_nnf (visit (conjunction_nnf_1 v) env)
                                     (visit (conjunction_nnf_2 v) env))]
              [(and (is-disjunction? v) (not env))
               (make-disjunction_nnf (visit (disjunction_nnf_1 v) env)
                                     (visit (disjunction_nnf_2 v) env))]


            [(is-negation? v)
             (visit (negation_1 v) (not env))]                     

            [else
             (errorf 'normalize-boolean-expression
                    "not a Boolean expression: ~s"
                    v)
             ]))])
      (visit v #f))))


;;; Exercise: complete this definition in a structurally recursive way.

(define week-05_boolean-expressions
  (and (test-normalize-boolean-expression normalize-boolean-expression) ;#t
       week-05_boolean-expressions))

;;;;;;;;;;

;;; Question: is normalization idempotent?
;;; (To answer this question, you will need to use the following procedure.)

(define embed_nnf
  (lambda (v)
    (cond
      [(is-identifier_nnf? v) ;if identifier_nnf
       (make-identifier (identifier_nnf_1 v))] ;then return new identifier_nnf from _1 of v → idempotent
       
      [(is-conjunction? v) ;if conjunction
       (make-conjunction (conjunction_nnf_1 v) ;then return new conjunction of conjunction_nnf_1|2 of v→ idempotent
                         (conjunction_nnf_2 v))]
                         
      [(is-disjunction_nnf? v) ;if disjunction_nnf
       (make-disjunction (disjunction_nnf_1 v) ;then return new non-nff disjunction of disjunction_nnf_1|2 of v → unidempotent
                         (disjunction_nnf_2 v))]
                         
      [(is-negation_nnf? v) ;if negation_nnf
       (make-negation (make-identifier (negation_nnf_1 v)))] ;then return ... (negation (identifier .._1 v)) v →  idempotent
      [else
       #f])))

;;;;;;;;;;

(if week-05_boolean-expressions
    "week-05_boolean-expressions.scm loaded and all unit tests succeeded"
    "week-05_boolean-expressions.scm loaded and some unit tests failed");;; week-05_boolean-expressions.scm
