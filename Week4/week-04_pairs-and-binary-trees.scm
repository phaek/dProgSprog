;;; week-04_pairs-and-binary-trees.scm
;;; dProgSprog 2016-2017, Q4
;;; Olivier Danvy <danvy@cs.au.dk>
;;; Version of 03 May 2017
;;; was:
;;; Version of 02 May 2017

;;;;;;;;;;

(define week-04_pairs-and-binary-trees
  #t)

;;;;;;;;;;

(define positive-test-binary-tree-of-non-negative-integers?
  (lambda (candidate)
    (and (equal? (candidate 0) #t)
         (equal? (candidate 1) #t)
         (equal? (candidate (cons 0 1)) #t)
         (equal? (candidate (cons (cons 0 1) (cons 2 3))) #t)
         ;;; etc.
         )))

(define negative-test-binary-tree-of-non-negative-integers?
  (lambda (candidate)
    (and (equal? (candidate -1) #f)
         (equal? (candidate #t) #f)
         (equal? (candidate (cons 1 #t)) #f)
         (equal? (candidate (cons 1 -1)) #f)
         ;;; etc.
         )))

(define binary-tree-of-non-negative-integers?
  (lambda (v)
    (if (integer? v)
        (if (negative? v)
            #f
            #t)
        (if (pair? v)
            (and (binary-tree-of-non-negative-integers? (car v))
                 (binary-tree-of-non-negative-integers? (cdr v)))
            #f))))

(define week-04_pairs-and-binary-trees
  (and (positive-test-binary-tree-of-non-negative-integers? binary-tree-of-non-negative-integers?)
       (negative-test-binary-tree-of-non-negative-integers? binary-tree-of-non-negative-integers?)
       week-04_pairs-and-binary-trees))

;;;;;

(define binary-tree-of-non-negative-integers?_alt
  (lambda (v)
    (if (integer? v)
        (not (negative? v))
        (if (pair? v)
            (and (binary-tree-of-non-negative-integers?_alt (car v))
                 (binary-tree-of-non-negative-integers?_alt (cdr v)))
            #f))))

(define week-04_pairs-and-binary-trees
  (and (positive-test-binary-tree-of-non-negative-integers? binary-tree-of-non-negative-integers?_alt)
       (negative-test-binary-tree-of-non-negative-integers? binary-tree-of-non-negative-integers?_alt)
       week-04_pairs-and-binary-trees))

;;;;;;;;;;

(define test-number-of-leaves
  (lambda (candidate)
    (and (equal? (candidate 32)
                 1)
         (equal? (candidate candidate)
                 1)
         (equal? (candidate (cons 32 33))
                 2)
         (equal? (candidate (cons 32 pair?))
                 2)
         (equal? (candidate (cons (cons 1 2) (cons 3 4)))
                 4)
         (equal? (candidate (cons 0 (cons (cons 1 2) (cons 3 4))))
                 5)
         (equal? (candidate (cons (cons 0 (cons (cons 1 2) (cons 3 4))) (cons 5 6)))
                 7)
         ;;; add more tests here
         )))

(define number-of-leaves
  (lambda (v)
    (if (pair? v)
        (+ (number-of-leaves (car v))
           (number-of-leaves (cdr v)))
        1)))

(define week-04_pairs-and-binary-trees
  (and (test-number-of-leaves number-of-leaves)
       week-04_pairs-and-binary-trees))

;;;;;;;;;;

(define test-number-of-nodes
  (lambda (candidate)
    (and (equal? (candidate 32)
                 0)
         (equal? (candidate candidate)
                 0)
         (equal? (candidate (cons 32 33))
                 1)
         (equal? (candidate (cons 32 pair?))
                 1)
         (equal? (candidate (cons (cons 1 2) (cons 3 4)))
                 3)
         (equal? (candidate (cons 0 (cons (cons 1 2) (cons 3 4))))
                 4)
         (equal? (candidate (cons (cons 0 (cons (cons 1 2) (cons 3 4))) (cons 5 6)))
                 6)
         ;;; add more tests here
         )))

(define number-of-nodes
  (lambda (v)
    (if (pair? v)
        (+ (number-of-nodes (car v))
           (number-of-nodes (cdr v))
           1)
        0)))

(define week-04_pairs-and-binary-trees
  (and (test-number-of-nodes number-of-nodes)
       week-04_pairs-and-binary-trees))

;;;;;;;;;;

(define test-swap
  (lambda (candidate)
    (and (equal? (candidate 0) 0)
         (equal? (candidate (cons 1 10)) (cons 10 1))
         (equal? (candidate (cons 1 (cons 10 100))) (cons (cons 100 10) 1))
         (equal? (candidate (cons (cons 100 10) 1)) (cons 1 (cons 10 100)))
         (equal? (candidate (cons (cons 1 10) (cons 100 1000))) (cons (cons 1000 100) (cons 10 1)))
         ;;; etc.
         )))

;;;;;;;;;;

(define swap
  (lambda (t)
    (if (not (pair? t))
        t
        (cons (swap (cdr t)) (swap (car t))))))


;;; end of week-04_pairs-and-binary-trees.scm

(if week-04_pairs-and-binary-trees
    "week-04_pairs-and-binary-trees.scm loaded and all unit tests succeeded"
    "week-04_pairs-and-binary-trees.scm loaded but some unit tests failed")
