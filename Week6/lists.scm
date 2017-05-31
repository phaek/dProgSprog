;;; lists.scm
;;; dProgSprog 2015-2016, Q4
;;; Olivier Danvy <danvy@cs.au.dk>
;;; Version of 16 May 2016

;;; Accompanying material for the lecture note at
;;;   http://users-cs.au.dk/danvy/dProgSprog16/Lecture-notes/week-6_parameter-passing-strategies.html

;;;;;;;;;;

(define list-a-ref
  (lambda (xs n)
    (if (= n 0)
        (car xs)
        (list-a-ref (cdr xs) (- n 1)))))

(define make-a-list
  (lambda (seed next)
    (letrec ([produce (lambda (current)
                        (begin
                          (display "producing ")
                          (display current)
                          (newline)
                          (cons current
                                (produce (next current)))))])
      (produce seed))))


(define make-a-list-of-nats
  (lambda (n)
    (make-a-list n
                 (lambda (n)
                   (+ n 1)))))
           
                   
;;;;;;;;;;  



                                                                                               
 _|_|_|_|                                          _|                            _|  _|  _|    
 _|        _|    _|    _|_|    _|  _|_|    _|_|_|        _|_|_|    _|_|        _|_|  _|  _|    
 _|_|_|      _|_|    _|_|_|_|  _|_|      _|        _|  _|_|      _|_|_|_|        _|  _|_|_|_|  
 _|        _|    _|  _|        _|        _|        _|      _|_|  _|              _|      _|    
 _|_|_|_|  _|    _|    _|_|_|  _|          _|_|_|  _|  _|_|_|      _|_|_|        _|      _|    
                                                                                               
                                                                                               

    
;;;CALL-BY-VALUE evaluates, in this case, the diverging list on load, resulting in an infinite loop of increments
;  without ever checking terminating arguments

(define my-list-of-nats_value
  make-a-list-of-nats 0)) 
  
;  However, if thunked, it will await a proper call before evaluating itself, but
;  still remain forever incrementing. Something like:

(define CbV_thunk
  (lambda (t)
    (let ([v (t)])
      (lambda ()
        v))))
        
(define my-list-of-nats_value
  (lambda (a)
    (CbV_thunk (make-a-list-of-nats a))))
;;;;;;;;;;



;;;CALL-BY-NAME evaluates when called and terminating properly when n=n+1 is sufficing computed, 
;  ending in (display "producing ~s" input) and (pair?), thus circumventing
;  the infinite evaluation of n=n+1

(define my-list-of-nats_name
  (lambda (a)
    (make-a-list-of-nats a)))

;;;We can thunk this, although I can't think of why we would want to, by using:
(define CbName_thunk
  (lambda (t)
    t))
        
;  This procedure allows us to peer into the actual workings of the programming
;  behind the interpreter, getting the intended result instead of a loop:

;by-name> (+ (list-a-ref my-list-of-nats 5)
;            (list-a-ref my-list-of-nats 3))
;producing 0
;producing 1
;producing 2
;producing 3
;producing 4
;producing 5
;producing 0
;producing 1
;producing 2
;producing 3
;8
;by-name> (+ (list-a-ref my-list-of-nats 5)
;            (list-a-ref my-list-of-nats 3))
;producing 0
;producing 1
;producing 2
;producing 3
;producing 4
;producing 5
;producing 0
;producing 1
;producing 2
;producing 3
;8
;by-name> 
;;;;;;;;;;;;,


;;;CALL-BY-NEED evaluates when needed. Utilizing memoization, this calling technique stores
;  computed evaluations for later referencing, thus never computing an object twice

(define my-list-of-nats_need
  (lambda (a)
   (make-a-list-of-nats a)))

;Running the same computation a few times will show the memoization in action:

;by-need> (+ (list-a-ref my-list-of-nats 5) 
;            (list-a-ref my-list-of-nats 3))
;producing 0
;producing 1
;producing 2
;producing 3
;producing 4
;producing 5
;8
;by-need> (+ (list-a-ref my-list-of-nats 5) 
;            (list-a-ref my-list-of-nats 3))
;8
;by-need> 
;;;;;;;;;;;;;;;,
;;; end of lists.scm

