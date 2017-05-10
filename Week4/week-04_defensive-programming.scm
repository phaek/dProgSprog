;;; week-04_defensive-programming.scm
;;; dProgSprog 2016-2017, Q4
;;; Olivier Danvy <danvy@cs.au.dk>
;;; Version of 02 May 2017

;;;;;;;;;;

(define week-04_defensive-programming
  #t)

;;;;;;;;;;

(define cautious-quotient
  (lambda (i j)
    (if (number? i)
        (if (number? j)
            (if (= j 0)
                (errorf 'cautious-quotient
                        "cannot divide anything by zero")
                (quotient i j))
            (errorf 'cautious-quotient
                    "~s is not a number"
                    j))
        (errorf 'cautious-quotient
                "~s is not a number"
                i))))

;;;;;;;;;;

(define test-fac
  (lambda (candidate)
    (and (equal? (candidate 0)
                 (*))
         (equal? (candidate 1)
                 (* 1))
         (equal? (candidate 2)
                 (* 2 1))
         (equal? (candidate 3)
                 (* 3 2 1))
         (equal? (candidate 4)
                 (* 4 3 2 1))
         (equal? (candidate 5)
                 (* 5 4 3 2 1))
         (equal? (candidate 6)
                 (* 6 5 4 3 2 1))
         (equal? (candidate 7)
                 (* 7 6 5 4 3 2 1))
         ;;; add more tests here
         )))

(define safe-fac
  (lambda (n)
    (letrec ([visit (lambda (i)
                      (if (= i 0)
                          1
                          (* i (visit (- i 1)))))])
      (if (and (integer? n)
               (>= n 0))
          (visit n)
          (errorf 'safe-fac "not a non-negative integer: ~s" n)))))

;;;;;;;;;;;

(define safe-fib
  (lambda (n)
    (letrec ([visit (lambda (i)
		      (if (= i 0)
			  0
			  (if (= i 1)
			      1
			      (+ (safe-fib (- i 2))
				 (safe-fib (- i 1))))))])
      (if (and (integer? n)
	       (>= n 0))
	  (visit n)
	  (errorf 'safe-fib "not a non-negative integer: ~s" n)))))

;;;;;;;;;;;

(define safe-fib_alt
  (lambda (i)
    (letrec ([fibfib (lambda (n)
		       (if (= n 0)
			   (cons 0 1)
			   (let ([p (fibfib (- n 1))])
			     (let ([fib_n-1 (car p)]
				   [fib_n (cdr p)])
			       (cons fib_n (+ fib_n-1 fib_n))))))])
      (if (and (integer? i)
	       (>= i 0))
	  (car (fibfib i))
	  (errorf 'safe-fib_alt "not a non-negative integer: ~s" i)))))
;;;;

(define week-04_defensive-programming
  (and (test-fac safe-fac)
       week-04_defensive-programming))

;;;;;;;;;;


;;; end of week-04_defensive-programming.scm

(if week-04_defensive-programming
    "week-04_defensive-programming.scm loaded and all unit tests succeeded"
    "week-04_defensive-programming.scm loaded but some unit tests failed")
