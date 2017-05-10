;;; week-04_Fibonacci.scm
;;; dProgSprog 2016-2017, Q4
;;; Olivier Danvy <danvy@cs.au.dk>
;;; Version of 02 May 2017

;;;;;;;;;;

(define week-04_Fibonacci
  #t)

;;;;;;;;;;

(define test-fib
  (lambda (candidate)
    (and (equal? (candidate 0)
                 0)
         (equal? (candidate 1)
                 1)
         (equal? (candidate 2)
                 1)
         (equal? (candidate 3)
                 2)
         (equal? (candidate 4)
                 3)
         (equal? (candidate 5)
                 5)
         (equal? (candidate 6)
                 8)
         (equal? (candidate 7)
                 13)
         (equal? (candidate 8)
                 21)
         (equal? (candidate 9)
                 34)
         (equal? (candidate 10)
                 (+ (candidate 9) (candidate 8)))
         ;;; add more tests, based on the Fibonacci sequence
         )))

(define fib
  (lambda (n)
    (if (= n 0)
        0
        (if (= n 1)
            1
            (+ (fib (- n 2))
               (fib (- n 1)))))))

(define week-04_Fibonacci
  (and (test-fib fib)
       week-04_Fibonacci))

;;;;;;;;;;

(define fibfib
  (lambda (n)
    (if (= n 0)
        (cons 0 1)
        (let ([p (fibfib (- n 1))])
          (let ([fib_n-1 (car p)]
                [fib_n (cdr p)])
            (cons fib_n (+ fib_n-1 fib_n)))))))

(define fib_alt
  (lambda (n)
    (car (fibfib n))))

(define week-04_Fibonacci
  (and (test-fib fib_alt)
       week-04_Fibonacci))

;;;;;;;;;;

(define number-of-recursive-calls-to-fib
  (lambda (n)
    (if (<= n 1)
        0
        (+ 2
           (number-of-recursive-calls-to-fib (- n 2))
           (number-of-recursive-calls-to-fib (- n 1))))))

;;;;;;;;;;

(define test-fib3
  (lambda (candidate)
    (and (equal? (candidate 0)
                 0)
         (equal? (candidate 1)
                 1)
         (equal? (candidate 2)
                 2)
         (equal? (candidate 3)
                 (+ 0
                    1
                    2))
         (equal? (candidate 4)
                 (+ 1
                    2
                    (+ 0 1 2)))
         (equal? (candidate 5)
                 (+ 2
                    (+ 0 1 2)
                    (+ 1 2 (+ 0 1 2))))
         (equal? (candidate 6)
                 (+ (+ 0 1 2)
                    (+ 1 2 (+ 0 1 2))
                    (+ 2 (+ 0 1 2) (+ 1 2 (+ 0 1 2)))))
         (equal? (candidate 7)
                 (+ (+ 1 2 (+ 0 1 2))
                    (+ 2 (+ 0 1 2) (+ 1 2 (+ 0 1 2)))
                    (+ (+ 0 1 2) (+ 1 2 (+ 0 1 2)) (+ 2 (+ 0 1 2) (+ 1 2 (+ 0 1 2))))))
         ;;; add more tests
         )))

;;;;;;;;;;

(define fib3
  (lambda (n)
    (if (= n 0)
        0
        (if (= n 1)
            1
            (if (= n 2)
            2
            (+ (fib3 (- n 3))
            (+ (fib3 (- n 2))
               (fib3 (- n 1)))))))))

;;; end of week-04_Fibonacci.scm

(if week-04_Fibonacci
    "week-04_Fibonacci.scm loaded and all unit tests succeeded"
    "week-04_Fibonacci.scm loaded but some unit tests failed")
