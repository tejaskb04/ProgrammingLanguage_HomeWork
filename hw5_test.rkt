#lang racket

(require "1130379020_hw5.rkt")

; a test case that uses problems 1, 2, and 4
; should produce (list (int 10) (int 11) (int 16))
(define test1
  (mupllist->racketlist
   (eval-exp (call (call mupl-mapAddN (int 7))
                   (racketlist->mupllist 
                    (list (int 3) (int 4) (int 9)))))))

(equal? (eval-exp (int 17))
        (int 17))
(equal? (eval-exp (aunit))
        (aunit))
(equal? (eval-exp (racketlist->mupllist (list (int 3) (int 4) (int 9))))
        (racketlist->mupllist (list (int 3) (int 4) (int 9))))
(equal? (eval-exp (closure null (fun #f "x" (var "x"))))
        (closure null (fun #f "x" (var "x"))))
(equal? (eval-exp (ifgreater (int 1) (int 2) (int 3) (int 4)))
        (int 4))
(equal? (eval-exp (fun #f "x" (var "x")))
        (closure null (fun #f "x" (var "x"))))
(equal? (eval-exp (call (fun #f "x" (var "x")) (int 4)))
        (int 4))
(equal? (eval-exp (mlet "x" (int 4) (var "x")))
        (int 4))
(equal? (eval-exp (fst (racketlist->mupllist (list (int 3) (int 4) (int 9)))))
        (int 3))
(equal? (eval-exp (snd (racketlist->mupllist (list (int 3) (int 4) (int 9)))))
        (racketlist->mupllist (list (int 4) (int 9))))
(equal? (eval-exp (isaunit (aunit)))
        (int 1))

(equal? (eval-exp (ifaunit (aunit) (int 1) (int 2)))
        (int 1))
(equal? (eval-exp (mlet* (list (cons "x" (int 2)) (cons "y" (var "x")) (cons "z" (var "y"))) (var "z")))
        (int 2))
(equal? (eval-exp (ifeq (int 1) (int 1) (int 3) (add (int 1) (int 1))))
        (int 3))

(equal? (eval-exp-c (call (fun #f "x" (var "x")) (int 4)))
        (int 4))

; a test case that uses eval-exp-c
; should produce (list (int 10) (int 11) (int 16))
(define test2
  (mupllist->racketlist
   (eval-exp-c (call (call mupl-mapAddN (int 7))
                   (racketlist->mupllist 
                    (list (int 3) (int 4) (int 9)))))))