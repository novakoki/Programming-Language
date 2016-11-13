#lang racket
(define (cube x)
  (* x x x))
(define (pow x y)
  (if (= y 0)
      1
      (* x (pow x (- y 1)))))