
#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; put your code below

; Sequence
(define (sequence low high stride)
  (if (> low high)
      (list)
      (cons low (sequence (+ low stride) high stride))))

; String append map
(define (string-append-map xs suffix)
  (map (lambda (i) (string-append i suffix)) xs))

; List Nth Mod
(define (list-nth-mod xs n)
  (cond [(< n 0) (error "list-nth-mod: negative number")]
        [(empty? xs) (error "list-nth-mod: empty list")]
        [#t (car (list-tail xs (remainder n (length xs))))]))

;Stream for n Steps
(define (stream-for-n-steps s n)
  (cond [(= n 0) null]
        [#t (cons (car (s)) (stream-for-n-steps (cdr (s)) (- n 1)))]))

;Funny number stream
(define funny-number-stream
  (letrec ([f (lambda (x) (if [= (remainder x 5) 0]
                              [cons (- x) (lambda () (f (+ x 1)))]
                              [cons x (lambda () (f (+ x 1)))]
                              ))])
    (lambda () (f 1))))

;Dan then dog
(define dan-then-dog
  (letrec ([f (lambda (x) (if [string=? x "dan.jpg"]
                              [cons x (lambda () (f "dog.jpg"))]
                              [cons x (lambda () (f "dan.jpg"))]))])
    (lambda () (f "dan.jpg"))))

;Stream add zero
(define (stream-add-zero s)
  (letrec ([f (lambda (x) (cons (cons 0 (car (x))) (lambda () (f (cdr (x))))))])
    (lambda () (f s))))

;Cycle lists
(define (cycle-lists xs ys)
  (letrec ([f (lambda (n) (cons (cons (list-nth-mod xs n)
                                      (list-nth-mod ys n))
                                (lambda () (f (+ n 1)))))])
    (lambda () (f 0))))

;Vector assoc
(define (vector-assoc v vec)
  (letrec ([f (lambda (x)
                (cond [(= x (vector-length vec)) #f]
                      [(and (= v (vector-ref (vector-ref vec x) 0))
                            (= (vector-length (vector-ref vec x)) 2))
                       (vector-ref vec x)]
                      (#t (f (+ x 1)))))])
    (f 0)))

