#lang racket

(require "cek.rkt")

;;;;;;; test-cases
(define case-0 '(+ 2 (+ 1 (* 2 3) (- 10 6)) (+ 1 4) 5.5 2))
(define case-1 '(+ 2 3.5 (expt 2 1) 2.5 3))
(define case-2 '(- (- (/ (* (* (+ 1 2) 2) 6) 2) (expt 2 (expt 2 3))) 1))
(define case-3 '(let ((x 5) (y 7) (z 9))
                  (let ((y x) (x y))
                    (let ((z (- z y)))
                      (+ x y z)))))
(define case-4 '(let ((x #t))
                  (let ((x #f))
                    (let ((y #f))
                      (or x y)))))

(define case-5 '(let* ([a 3] [b (* 2 a)]) (cons a b)))

(define case-6 '(((call/cc (λ (x) ((x x) x))) (λ (y) y)) #t))
(define case-7 '(call/cc
                 (λ (top)
                   (let ((cc (call/cc (λ (cc) (cc cc)))))
                     (if (call/cc (λ (k) (if (cc (lambda (x) (top #f))) (k #f) (k #f))))
                         #t
                         #t)))))

(define case-8 '(if #t #f #t))
(define case-9 '(and (not (and #t #f)) (and #t #t)))
(define case-10 '(((λ (f) (λ (x) (f x))) (λ (y) (+ y y))) (+ 1 20)))
(define case-11 '((λ (x) x) (if #f 1 2)))
(define case-12 '((lambda (x) x) 1))
(define case-13 '((lambda () 5)))
(define case-14 '((λ (a b c) b) 1 2 3))
(define case-15 '((lambda x (cdr x)) 1 2 3))
(define case-16 '(list 1 2 3 (list 4 5) 6))
(define case-17 '((lambda a (cons 5 a)) 0 1 2 3 4))
(define case-18 '(apply + (list 1 3 4)))
(define case-19 '(apply (lambda x (cdr x)) (list 1 3 4)))
(define case-20 '(apply (lambda (a b c) b) (list 1 (list 5 6) 4)))



;;;;;;; Run examples
;#;
(begin
  (cek-interp case-0)
  (cek-interp case-1)
  (cek-interp case-2)
  (cek-interp case-3)
  (cek-interp case-4)
  (cek-interp case-5)
  (cek-interp case-6)
  (cek-interp case-7)
  (cek-interp case-8)
  (cek-interp case-9)
  (cek-interp case-10)
  (cek-interp case-11)
  (cek-interp case-12)
  (cek-interp case-13)
  (cek-interp case-14)
  (cek-interp case-15)
  (cek-interp case-17)
  (cek-interp case-18)
  (cek-interp case-19)
  (cek-interp case-20))