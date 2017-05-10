;;; week-04_sets-as-lists.scm
;;; dProgSprog 2016-2017, Q4
;;; Olivier Danvy <danvy@cs.au.dk>
;;; Version of 02 May 2017

;;;;;;;;;;

(define week-04_sets-as-lists #t)

;;;;;;;;;;

(define positive-test-member?
  (lambda (candidate)
    (and (equal? (candidate 'x '(y x z)) #t)
         (equal? (candidate 'x '(x)) #t)
         ;;; etc.
         )))

(define negative-test-member?
  (lambda (candidate)
    (and (equal? (candidate 'x '()) #f)
         (equal? (candidate 'x '(y)) #f)
         (equal? (candidate 'x '(y z)) #f)
         ;;; etc.
         )))

;;;;;

(define member?
  (lambda (x xs)
    (if (null? xs)
        #f
        (if (pair? xs)
            (if (equal? x (car xs))
                #t
                (member? x (cdr xs)))
            (errorf 'member?
                    "not a proper list: ~s"
                    xs)))))

(define week-04_sets-as-lists
  (and (positive-test-member? member?)
       (negative-test-member? member?)
       week-04_sets-as-lists))

;;;;;

(define member?_alt
  (lambda (x xs_init)
    (letrec ([walk (lambda (xs)
                     (if (null? xs)
                         #f
                         (if (pair? xs)
                             (if (equal? x (car xs))
                                 #t
                                 (walk (cdr xs)))
                             (errorf 'member?_alt
                                     "not a proper list: ~s"
                                     xs_init))))])
      (walk xs_init))))

(define week-04_sets-as-lists
  (and (positive-test-member? member?_alt)
       (negative-test-member? member?_alt)
       week-04_sets-as-lists))

;;;;;;;;;;

(define positive-test-set?
  (lambda (candidate)
    (and (equal? (candidate '()) #t)
         (equal? (candidate '(1 2 3)) #t)
         ;;; etc.
         )))

(define negative-test-set?
  (lambda (candidate)
    (and (equal? (candidate '(1 2 1 3)) #f)
         ;;; etc.
         )))

;;;;;

(define set?
  (lambda (v)
    (if (null? v)
        #t
        (if (pair? v)
            (if (member? (car v) (cdr v))
                  #f
                  (set? (cdr v)))
            (errorf 'set?
                    "not a proper list: ~s"
                    s)))))

(define week-04_sets-as-lists
  (and (positive-test-set? set?)
       (negative-test-set? set?)
       week-04_sets-as-lists))

;;;;;;;;;;

;;; an element belongs to a set
;;; whenever
;;; its representation occurs in the representation of this set

;;; let e denote an element and es the representation of a set:
;;; evaluating
;;;   (belongs-to e es)
;;; yields #t whenever e belongs to es;
;;; otherwise, it yields #f

(define belongs-to?
  (lambda (e es)
    (member? e es)))

;;;;;

(define belongs-to?_alt
  member?)

;;;;;;;;;;

(define positive-test-included-in?
  (lambda (candidate)
    (and (equal? (candidate '() '()) #t)
         (equal? (candidate '() '(x)) #t)
         (equal? (candidate '() '(x y z)) #t)
         (equal? (candidate '(y) '(x y z)) #t)
         ;;; etc.
         )))

(define negative-test-included-in?
  (lambda (candidate)
    (and (equal? (candidate '(x) '()) #f)
         (equal? (candidate '(x y) '(x)) #f)
         ;;; etc.
         )))

;;;;;

(define included-in?
  (lambda (e1s_init e2s_init)
    (letrec ([traverse (lambda (e1s)
                         (if (null? e1s)
                             #t
                             (if (pair? e1s)
                                 (if (belongs-to? (car e1s) e2s_init)
                                     (traverse (cdr e1s))
                                     #f)
                                 (errorf 'included-in?
                                         "not a proper list: ~s"
                                         e1s))))])
      (traverse e1s_init))))

(define week-04_sets-as-lists
  (and (positive-test-included-in? included-in?)
       (negative-test-included-in? included-in?)
       week-04_sets-as-lists))

;;;;;;;;;;

(define includes?
  (lambda (e2s e1s)
    (included-in? e1s e2s)))

;;;;;;;;;;

(define set-equal?
  (lambda (e1s e2s)
    (and (included-in? e1s e2s)
         (included-in? e2s e1s))))

;;;;;;;;;;

(define positive-test-set-union?
  (lambda (candidate)
    (and (equal? (candidate '() '() '()) #t)
         (equal? (candidate '() '(0 1 2 3 4 5) '(0 1 2 3 4 5)) #t)
         (equal? (candidate '(0 1 2 3 4 5) '() '(0 1 2 3 4 5)) #t)
         (equal? (candidate '(1 2 3 4) '(0 4 5) '(0 1 2 3 4 5)) #t)
         (equal? (candidate '(0 1 2 3 4 5) '(0 1 2 3 4 5) '(0 1 2 3 4 5)) #t)
         (equal? (candidate '(0 1 2 3 4) '(1 2 3 4 5) '(0 1 2 3 4 5)) #t)
         ;;; etc.
         )))

(define negative-test-set-union?
  (lambda (candidate)
    (and (equal? (candidate '() '() '(0)) #f)
         (equal? (candidate '(0 1 2) '(4 5) '(0 1 2 3 4 5)) #f)
         (equal? (candidate '(0 1 2 3 4 5) '() '(0 1 2 3 4 5 6)) #f)
         (equal? (candidate '(0 1 2 6) '(3 4 5) '(0 1 2 3 4 5)) #f)
         (equal? (candidate '(0 1 2 3 4) '(1 2 3 4 5) '(0 1 2 3 4 5 6)) #f)
         ;;; etc.
         )))

(define set-union?
  (lambda (e1s e2s e1s_union_e2s)
    (let ([sound
           (lambda ()
             ;;; each element of each set belongs to the union
             (and (included-in? e1s e1s_union_e2s)
                  (included-in? e2s e1s_union_e2s)))]
          [complete
           (lambda ()
             ;;; each element of the union belongs to either set (or both)
             (letrec ([traverse
                       (lambda (es)
                         (if (null? es)
                             #t
                             (if (pair? es)
                                 (if (or (belongs-to? (car es) e1s)
                                         (belongs-to? (car es) e2s))
                                     (traverse (cdr es))
                                     #f)
                                 (errorf 'set-union?
                                         "not a proper list: ~s"
                                         es))))])
               (traverse e1s_union_e2s)))])
      (and (sound)
           (complete)))))

(define week-04_sets-as-lists
  (and (positive-test-set-union? set-union?)
       (negative-test-set-union? set-union?)
       week-04_sets-as-lists))

;;;;;

;;; Exercise: uncomment and complete the following definition:

;;; (define set-union
;;;   (lambda (e1s e2s)
;;;     ...))

;;; Exercise: uncomment the following definition:

;;; (define week-04_sets-as-lists
;;;   (and (positive-test-set-union? (lambda (e1s e2s e1s_union_e2s)
;;;                                    (set-equal? (set-union e1s e2s) e1s_union_e2s)))
;;;        (negative-test-set-union? (lambda (e1s e2s e1s_union_e2s)
;;;                                    (set-equal? (set-union e1s e2s) e1s_union_e2s)))
;;;        (positive-test-set-union? (lambda (e1s e2s e1s_union_e2s)
;;;                                    (set-union? e1s e2s (set-union e1s e2s))))
;;;        week-04_sets-as-lists))

;;;;;;;;;;

(define positive-test-set-intersection?
  (lambda (candidate)
    (and (equal? (candidate '() '() '()) #t)
         (equal? (candidate '() '(0 1 2 3 4 5) '()) #t)
         (equal? (candidate '(0 1 2 3 4 5) '() '()) #t)
         (equal? (candidate '(1 2 3) '(0 4 5) '()) #t)
         (equal? (candidate '(0 1 2 3 4 5) '(0 1 2 3 4 5) '(0 1 2 3 4 5)) #t)
         (equal? (candidate '(0 1 2 3) '(2 3 4 5) '(2 3)) #t)
         ;;; etc.
         )))

(define negative-test-set-intersection?
  (lambda (candidate)
    (and (equal? (candidate '(0 1 2 3) '(2 3 4 5) '(2 3 6)) #f)
         (equal? (candidate '(0 1 2 3) '(2 3 4 5) '(2)) #f)
         ;;; etc.
         )))

(define set-intersection?
  (lambda (e1s e2s e1s_inter_e2s)
    (let ([sound
           (lambda ()
             ;;; each element of the intersection belongs to both sets
             (letrec ([waltz-through
                       (lambda (es)
                         (if (null? es)
                             #t
                             (if (pair? es)
                                 (if (and (belongs-to? (car es) e1s)
                                          (belongs-to? (car es) e1s))
                                     (waltz-through (cdr es))
                                     #f)
                                 (errorf 'set-intersection?
                                         "not a proper list: ~s"
                                         es))))])
                     (waltz-through e1s_inter_e2s)))]
          [complete
           (lambda ()
             ;;; each element of one set that occurs in the other set
             ;;; also occurs in the intersection
             (let ([relative
                    (lambda (es other-es)
                      (letrec ([run-through
                                (lambda (es)
                                  (if (null? es)
                                      #t
                                      (if (pair? es)
                                          (if (belongs-to? (car es) other-es)
                                              (if (belongs-to? (car es) e1s_inter_e2s)
                                                  (run-through (cdr es))
                                                  #f)
                                              (run-through (cdr es)))
                                          (errorf 'set-intersection?
                                                  "not a proper list: ~s"
                                                  es))))])
                        (run-through es)))])
               (and (relative e1s e2s) (relative e2s e1s))))])
      (and (sound)
           (complete)))))

(define week-04_sets-as-lists
  (and (positive-test-set-intersection? set-intersection?)
       (negative-test-set-intersection? set-intersection?)
       week-04_sets-as-lists))

;;;;;

;;; Exercise: uncomment and complete the following definition:

;;; (define set-intersection
;;;   (lambda (e1s e2s)
;;;     ...))

;;; Exercise: uncomment the following definition:

;;; (define week-04_sets-as-lists
;;;   (and (positive-test-set-intersection? (lambda (e1s e2s e1s_inter_e2s)
;;;                                           (set-equal? (set-intersection e1s e2s) e1s_inter_e2s)))
;;;        (negative-test-set-intersection? (lambda (e1s e2s e1s_inter_e2s)
;;;                                           (set-equal? (set-intersection e1s e2s) e1s_inter_e2s)))
;;;        (positive-test-set-intersection? (lambda (e1s e2s e1s_inter_e2s)
;;;                                           (set-intersection? e1s e2s (set-intersection e1s e2s))))
;;;        week-04_sets-as-lists))

;;;;;;;;;;

(define positive-test-set-minus?
  (lambda (candidate)
    (and (equal? (candidate '() '() '()) #t)
         (equal? (candidate '() '(0 1 2 3 4 5) '()) #t)
         (equal? (candidate '(0 1 2 3 4 5) '() '(0 1 2 3 4 5)) #t)
         (equal? (candidate '(1 2 3) '(0 4 5) '(1 2 3)) #t)
         (equal? (candidate '(0 1 2 3 4 5) '(0 1 2 3 4 5) '()) #t)
         (equal? (candidate '(0 1 2 3) '(2 3 4 5) '(0 1)) #t)
         ;;; etc.
         )))

(define negative-test-set-minus?
  (lambda (candidate)
    (and (equal? (candidate '(0 1 2 3) '(2 3 4 5) '(0 1 2)) #f)
         (equal? (candidate '(0 1 2 3) '(2 3 4 5) '(0 1 4)) #f)
         (equal? (candidate '(0 1 2 3) '(2 3 4 5) '(0 1 6)) #f)
         ;;; etc.
         )))

(define set-minus?
  (lambda (e1s e2s e1s_minus_e2s)
    (let ([sound
           (lambda ()
             ;;; each element of the difference belongs to the first set but not the second
             (letrec ([cross
                       (lambda (es)
                         (if (null? es)
                             #t
                             (if (pair? es)
                                 (and (and (belongs-to? (car es) e1s)
                                           (not (belongs-to? (car es) e2s)))
                                      (cross (cdr es)))
                                 (errorf 'set-minus?
                                         "not a proper list: ~s"
                                         es))))])
               (cross e1s_minus_e2s)))]
          [complete
           (lambda ()
             ;;; each element of the second set does not belong to the difference,
             ;;; and
             ;;; for each element of the first set,
             ;;;   if it belongs to the second, it does not belong in the difference
             ;;;   and
             ;;;   if it does not belong to the second, it belongs to the difference
             (letrec ([about-the-second-set
                       (lambda (es)
                         (if (null? es)
                             #t
                             (if (pair? es)
                                 (if (belongs-to? (car es) e1s_minus_e2s)
                                     #f
                                     (about-the-second-set (cdr es)))
                                 (errorf 'set-minus?
                                         "not a proper list: ~s"
                                         es))))]
                      [about-the-first-set
                       (lambda (es)
                         (if (null? es)
                             #t
                             (if (pair? es)
                                 (and (if (belongs-to? (car es) e2s)
                                          (not (belongs-to? (car es) e1s_minus_e2s))
                                          (belongs-to? (car es) e1s_minus_e2s))
                                      (about-the-first-set (cdr es)))
                                 (errorf 'set-minus?
                                         "not a proper list: ~s"
                                         es))))])
               (and (about-the-second-set e2s)
                    (about-the-first-set e1s))))])
      (and (sound)
           (complete)))))

(define week-04_sets-as-lists
  (and (positive-test-set-minus? set-minus?)
       (negative-test-set-minus? set-minus?)
       week-04_sets-as-lists))

;;;;;;;;;;

;;; Exercise: uncomment and complete the following definition:

;;; (define set-minus
;;;   (lambda (e1s e2s)
;;;     ...))

;;; Exercise: uncomment the following definition:

;;; (define week-04_sets-as-lists
;;;   (and (positive-test-set-minus? (lambda (e1s e2s e1s_minus_e2s)
;;;                                    (set-equal? (set-minus e1s e2s) e1s_minus_e2s)))
;;;        (negative-test-set-minus? (lambda (e1s e2s e1s_minus_e2s)
;;;                                    (set-equal? (set-minus e1s e2s) e1s_minus_e2s)))
;;;        (positive-test-set-minus? (lambda (e1s e2s e1s_minus_e2s)
;;;                                    (set-minus? e1s e2s (set-minus e1s e2s))))
;;;        week-04_sets-as-lists))

;;;;;;;;;;

(define test-normalize-list-into-set
  (lambda (candidate)
    (and (set? (candidate '(1 2 3 4)))
         (set? (candidate '(1 1 1 1)))
         (set? (candidate '()))
         (set? (candidate '(1 2 3 4 3 2 1)))
         ;;; etc.
         )))

(define normalize-list-into-set
  (lambda (vs)
    (if (null? vs)
        '()
        (if (pair? vs)
            (let ([vsp (normalize-list-into-set (cdr vs))])
              (if (member? (car vs) vsp)
                  vsp
                  (cons (car vs) vsp)))
            (errorf 'normalize-list-into-set
                    "not a proper list: ~s"
                    vs)))))

(define week-04_sets-as-lists
  (and (test-normalize-list-into-set normalize-list-into-set)
       week-04_sets-as-lists))

;;;;;;;;;;

(define set-union_quick-and-dirty
  (lambda (e1s e2s)
    (normalize-list-into-set (append e1s e2s))))

;;;;;;;;;;

(define set-union_outside-the-box
  (lambda (e1s e2s)
    (append e1s e2s)))

;;;;;;;;;;

(define set-union
  (lambda (e1s e2s)
    (cond
     ((null? e2s) e1s)
        ((member? (car e2s) e1s)
         (set-union e1s (cdr e2s)))
        (else (set-union (cons (car e2s) e1s) (cdr e2s))))))

;;;;;;;;;

(define set-intersection
  (lambda (e1s e2s)
    (cond
      ((null? e1s) '())
      ((member? (car e1s) e2s)
       (cons (car e1s) (set-intersection (cdr e1s) e2s)))
      (else (set-intersection (cdr e1s) e2s)))))

;;;;;;;;;

(define set-minus
  (lambda (e1s e2s)
    (cond
     ((null? e1s) '())
     ((member? (car e1s) e2s)
      (set-minus (cdr e1s) e2s))
     (else (cons (car e1s) (set-minus (cdr e1s) e2s))))))

;;;;;;;;;


;;; end of week-04_sets-as-lists.scm

(if week-04_sets-as-lists
    "week-04_sets-as-lists.scm loaded and all unit tests succeeded"
    "week-04_sets-as-lists.scm loaded but some unit tests failed")
