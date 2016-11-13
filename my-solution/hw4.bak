
#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; put your code below

(define (sequence low high stride)
  (letrec ([append
            (lambda (li low)
              (if (> low high)
                  null
                  (cons low (append li (+ low stride)))))])
    (append null low)))

(define (string-append-map xs suffix)
  (map (lambda (item)
         (string-append item suffix)) xs))

(define (list-nth-mod xs n)
  (if (< n 0)
      (error "list-nth-mod: negative number")
      (if (= (length xs) 0)
          (error "list-nth-mod: emply list")
          (car (list-tail xs (remainder n (length xs)))))))

;(define (my-delay fn)
;  (mcons #f fn))

;(define (my-force p)
;  (if (mcar p)
;      (mcdr p)
;      (begin
;        (set-mcar! p #t)
;        (set-mcdr! p ((mcdr p)))
;        (mcdr p)
;        )))

(define (stream-for-n-steps s n)
  (if (= n 0)
      null
      (cons (car (s)) (stream-for-n-steps (cdr (s)) (- n 1)))))

(define (funny-number-stream)
  (letrec ([f (lambda (x) (cons (if (= (remainder x 5) 0) (- x) x)
                                (lambda () (f (+ x 1)))))])
    ((lambda () (f 1)))))

(define (dan-then-dog)
  (letrec ([f (lambda (x)
                (cons x (lambda ()
                          (f (if (string=? x "dan.jpg")
                                 "dog.jpg"
                                 "dan.jpg"
                                 )))))])
  ((lambda () (f "dan.jpg")))))
                                                    
(define (stream-add-zero s)
  (letrec ([f (lambda (x) (cons (cons 0 (car (x)))
                                (lambda () (f (cdr (x))))))])
    (lambda () (f s))))

(define (cycle-lists xs ys)
  (letrec ([f (lambda (n) (cons (cons (list-nth-mod xs n)
                                     (list-nth-mod ys n))
                               (lambda () (f (+ n 1)))))])
    (lambda () (f 0))))

(define (vector-assoc v vec)
  (letrec ([len (vector-length vec)]
           [f (lambda (vec pos)
                (if (= len pos)
                    #f
                    (let ([item (vector-ref vec pos)]) 
                    (if (and (pair? item) (equal? (car item) v))
                        item
                        (f vec (+ pos 1))))))])
    (f vec 0)))

(define (cached-assoc xs n)
  (let ([cache (make-vector n #f)]
        [slot 0])
    (lambda (v)
      (let ([item (vector-assoc v cache)])
       (if (equal? item #f)
            (let ([res (assoc v xs)])
                 (begin (vector-set! cache slot res)
                  (+ slot 1)
                  (remainder slot n)
                  res))
          item)))))
          
          










  

                               