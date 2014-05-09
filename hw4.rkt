
#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; put your code below

(define (sequence low high stride)
  (cond [(> low high) empty]
        [(> (+ low stride) high) (list low)]
        [#t (cons low (sequence (+ low stride) high stride))]))

(define (string-append-map xs suffix)
  (map (lambda (i) (string-append i suffix)) xs))

(define (list-nth-mod xs n)
  (cond [(< n 0) (error "list-nth-mod: negative number")]
        [(equal? (length xs) 0) (error "list-nth-mod: empty list")]
        [#t (car (list-tail xs (remainder n (length xs))))]))

(define (stream-for-n-steps s n)
  (cond [(equal? n 0) empty]
        [#t (cons (car (s)) (stream-for-n-steps (cdr (s)) (- n 1)))]))

(define funny-number-stream
  (letrec ([f (lambda (x) (cons (if (equal? x 5) (- x) x) (lambda () (f (+ x 1)))))])
    (lambda () (f 1))))

(define dan-then-dog 
  (letrec ([f (lambda (x) (cons x (lambda () (f (if (equal? x "dan.jpg") "dog.jpg" "dan.jpg")))))])
    (lambda () (f "dan.jpg"))))

(define (stream-add-zero s)
  (lambda () (cons (cons 0 (car (s))) (stream-add-zero (cdr (s))))))

(define (cycle-lists xs ys)
  (letrec ([f (lambda (n) (cons (cons (list-nth-mod xs n) (list-nth-mod ys n)) (lambda () (f (+ n 1)))))])
    (lambda () (f 0))))

(define (vector-assoc v vec)
  (letrec ([len (vector-length vec)]
           [f (lambda (n)
                (if (>= n len)
                    #f
                    (let ([tmp (vector-ref vec n)])
                      (if (and (pair? tmp) (equal? (car tmp) v))
                          tmp
                          (f (+ n 1))))))])
    (f 0)))

(define (cached-assoc xs n)
  (letrec ([cache (make-vector n #f)]
           [slot 0]
           [f (lambda (v) (let ([ans (vector-assoc v cache)])
                            (if ans
                                (cdr ans)
                                (let ([new-ans (assoc v xs)])
                                  (if new-ans
                                      (begin (vector-set! cache slot (cons v new-ans))
                                             (set! slot (remainder (+ slot 1) n))
                                             new-ans)
                                      #f)))))])
    f))

(define-syntax while-less
  (syntax-rules (do)
    [(while-less e1 do e2)
     (letrec ([n e1]
              [f (lambda()
                   (if (>= e2 n)
                       #t
                       (f)))])
       (f))]))

(define (cycle-lists-challenge xs ys)
  (letrec ([f (lambda (xs ys)
                (let ([x (car xs)]
                      [y (car ys)])
                  (cons (cons x y) (lambda () (f (append (cdr xs) (list x)) (append (cdr ys) (list y)))))))])
    (lambda () (f xs ys))))

(define (cached-assoc-lru xs n)
  (letrec ([findpos (lambda (vec v i) (if (and (pair? (vector-ref vec i)) (equal? v (car (vector-ref vec i)))) i (findpos vec v (+ i 1))))]
           [cache (make-vector n #f)]
           [f (lambda (v) (let ([ans (vector-assoc v cache)])
                            (if ans
                                (let ([i (findpos cache v 0)])
                                  (begin (set! cache (vector-append (vector ans) (vector-take cache i) (vector-drop cache (+ i 1)))) ans))
                                (let ([new-ans (assoc v xs)])
                                  (if new-ans
                                      (begin (set! cache (vector-append (vector (cons v new-ans)) (vector-take cache (- n 1))))
                                             new-ans)
                                      #f)))))])
    f))