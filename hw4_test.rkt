#lang racket

(require "1130379020_hw4.rkt") 

;; A simple library for displaying a 2x3 grid of pictures: used
;; for fun in the tests below (look for "Tests Start Here").

(require (lib "graphics.rkt" "graphics"))

(open-graphics)

(define window-name "Programming Languages, Homework 4")
(define window-width 700)
(define window-height 500)
(define border-size 100)

(define approx-pic-width 200)
(define approx-pic-height 200)
(define pic-grid-width 3)
(define pic-grid-height 2)

(define (open-window)
  (open-viewport window-name window-width window-height))

(define (grid-posn-to-posn grid-posn)
  (when (>= grid-posn (* pic-grid-height pic-grid-width))
    (error "picture grid does not have that many positions"))
  (let ([row (quotient grid-posn pic-grid-width)]
        [col (remainder grid-posn pic-grid-width)])
    (make-posn (+ border-size (* approx-pic-width col))
               (+ border-size (* approx-pic-height row)))))

(define (place-picture window filename grid-posn)
  (let ([posn (grid-posn-to-posn grid-posn)])
    ((clear-solid-rectangle window) posn approx-pic-width approx-pic-height)
    ((draw-pixmap window) filename posn)))

(define (place-repeatedly window pause stream n)
  (when (> n 0)
    (let* ([next (stream)]
           [filename (cdar next)]
           [grid-posn (caar next)]
           [stream (cdr next)])
      (place-picture window filename grid-posn)
      (sleep pause)
      (place-repeatedly window pause stream (- n 1)))))

;; Tests Start Here
(equal? (sequence 0 5 1) 
       '(0 1 2 3 4 5));P1

(equal? (string-append-map (list "dan" "dog" "curry" "dog2") ".jpg")
       '("dan.jpg" "dog.jpg" "curry.jpg" "dog2.jpg"));P2

(equal? (list-nth-mod '(1 2 3 4 5 6) 3) 4);P3

(equal? (stream-for-n-steps funny-number-stream 5) '(1 2 3 4 -5));P4 & P5

(equal? (stream-for-n-steps dan-then-dog 3) '("dan.jpg" "dog.jpg" "dan.jpg"));P6

(equal? (stream-for-n-steps (stream-add-zero funny-number-stream) 3) (list (cons 0 1) (cons 0 2) (cons 0 3)));P7

(equal? (stream-for-n-steps (cycle-lists '(1 2 3) '("a" "b")) 3) (list (cons 1 "a") (cons 2 "b") (cons 3 "a")));P8

(equal? (vector-assoc 2 (vector (cons 1 1) (cons 2 2) (cons 3 3))) (cons 2 2));P9

(equal? ((cached-assoc (list (cons 1 1) (cons 2 2) (cons 3 3)) 2) 2) (cons 2 2));P10

(define a 2)
(equal? (begin (while-less 7 do (begin (set! a (+ a 1)) a)) a) 7);P11

(equal? (stream-for-n-steps (cycle-lists-challenge '(1 2 3) '("a" "b")) 3) (list (cons 1 "a") (cons 2 "b") (cons 3 "a")));Ch1

(equal? ((cached-assoc-lru (list (cons 1 1) (cons 2 2) (cons 3 3)) 2) 2) (cons 2 2));P10
; These definitions will work only after you do some of the problems
; so you need to comment them out until you are ready.
; Add more tests as appropriate, of course.

(define nums (sequence 0 5 1))

(define files (string-append-map 
               (list "dan" "dog" "curry" "dog2") 
               ".jpg"))

(define funny-test (stream-for-n-steps funny-number-stream 16))

; a zero-argument function: call (one-visual-test) to open the graphics window, etc.
(define (one-visual-test)
  (place-repeatedly (open-window) 0.5 (cycle-lists nums files) 27))

; similar to previous but uses only two files and one position on the grid
(define (visual-zero-only)
  (place-repeatedly (open-window) 0.5 (stream-add-zero dan-then-dog) 27))