#lang racket

(provide cek-interp)

(require "parser.rkt")

(require (rename-in racket/base (eval racket-eval)))
         
(define default-prims '(* + - / expt = car cdr cons equal? null?))

(define prims
  (foldl (lambda (op env)
           (hash-set env op `(clo (λ args (apply-prim ,op args)) #hash())))
         '#hash((list . (clo (λ x x) #hash())))
         default-prims))

(define (racket-apply proc lst)
  (apply proc lst))

(define (cek-interp prog)
  ; Helper function to check if symbol λ or tag lambda
  (define (λ-or-lambda? str)
    (match str
      [(or (== 'lambda) (== 'λ)) #t]
      [else #f]))
  
  (define (eval exp env kont)
    ;(displayln (~a "\n>>>eval : " (list '--exp: exp '--env: env '--kont: kont)))
    (match exp  
      [(or (? number?) (? boolean?))
       (ret exp kont)]

      [`(,(? λ-or-lambda?) ,args ,body)
       (ret `(clo ,exp ,env) kont)]
       
      [(? symbol? x)
       (ret (hash-ref env x
                      (λ () (raise `(error ,(format "Undefined variable: ~a" x)))))
            kont)] 

      [`(if ,grd, texp, fexp)
       (eval grd env `(ifk ,texp ,fexp ,env ,kont))]

      [`(and ,e0, e1)
       (eval e0 env `(andk ,e1 ,env ,kont))]

      [`(or ,e0 ,e1)
       (eval e0 env `(ork ,e1 ,env ,kont))]
      
      [`(not ,exp)
       (eval exp env `(notk ,env ,kont))]

      [`(let ([,xs ,rhs] ...) ,body)
       (eval `((λ ,xs ,body) ,@rhs) env kont)]

      [`(let* () ,ebody) (eval ebody env kont)]
      [`(let* ([,lhs ,rhs] ,e-pairs ...) ,ebody)      
       (eval `(let ([,lhs ,rhs]) (let* ,e-pairs ,ebody)) env kont)]

      [`(call/cc ,e0)
       (eval e0 env `(callcc-k ,kont))]

      [`(apply ,e0 ,e1)
       (eval e0 env `(apply-ark ,e1 ,env ,kont))]

      [`(apply-prim ,opr ,x)
       (ret (racket-apply (racket-eval opr (make-base-namespace)) (hash-ref env x)) kont)]
      
      [`(,ef ,ea-list ...)
       (eval ef env `(app-k () ,ea-list ,env ,kont))]

      ;[else (raise  (format "error occured in eval function! exp: ~a env: ~a kont: ~a " exp env kont))]
      [else (raise `(Error occured in eval function!...State: ,exp ,env ,kont))]))
  
  (define (ret val kont)
    ;(displayln (~a ">>>ret  : " (list '--val: val '--kont: kont)))
    (match kont
      ['mt val]

      [`(ifk ,texp ,fexp ,this_env ,this_kont)
       (if val
           (eval texp this_env this_kont)
           (eval fexp this_env this_kont))]

      [`(andk ,ea ,this_env, this_kont)
       (if val
           (eval ea this_env this_kont)
           (ret val this_kont))]

      [`(ork ,ea ,this_env, this_kont)
       (if val
           (ret val this_kont)
           (eval ea this_env this_kont))]
      
      [`(notk ,this_env, this_kont)
       (if val
           (ret #f this_kont)
           (ret #t this_kont))]

      [`(callcc-k ,this_kont)
       (apply val `((kont ,this_kont)) this_kont)]

      [`(apply-ark ,ea ,env ,kont)
       (eval ea env `(apply-fnk ,val ,kont))]

      [`(apply-fnk ,fv ,kont)
       (apply fv val kont)]

      [`(app-k ,v-list () ,env ,sub_kont)
       (define vals (append v-list `(,val)))
       (apply (car vals) (cdr vals) sub_kont)]
       
      [`(app-k ,v-list (,e0 ,e-list ...) ,env ,sub_kont)
       (eval e0 env `(app-k ,(append v-list `(,val)) ,e-list ,env ,sub_kont))]
     
      [else (raise `(Error occured in ret function!...State: ,val ,kont))]))
  
  (define (apply vf va-list kont)
    ;(displayln (~a ">>>apply: " (list '--vf: vf '--va-list: va-list '--kont: kont)))
    ;(pretty-print vf)
    ;(pretty-print va-list)
    ;(pretty-print kont)
    (match vf
      [`(clo (,(? λ-or-lambda?) ,params ,eb) ,env)
       (if (symbol? params)
           (eval eb (hash-set env params va-list) kont)
           (if (= (length params) (length va-list))
               (eval eb
                     (foldl (lambda (x va env) (hash-set env x va)) env params va-list)
                     kont)
               (raise `(Error number of arguments do not match))))]

      [`(kont ,this_kont)
       (if (= 1 (length va-list))
           (ret (first va-list) this_kont)
           (raise `(Error occured applying continuation on ,(length va-list) arguments)))]

      [else (raise `(Error occured in apply function!...State: ,vf ,va-list ,kont))]))
  
  (eval prog prims 'mt))

; (cek-interp '(+ 420 (* 2 (/ 3 3))))
; (cek-interp '((λ (x) x) (if #f 1 2)))
; (cek-interp '(if #t #f #t))
; (cek-interp '((lambda (x) x) 1))
; (cek-interp '((lambda () 5)))
; (cek-interp '((λ (a b c) b) 1 2 3))
; ;(cek-interp '((λ (q) (λ (b) q)) (λ (z) z)))
; (cek-interp '((lambda x (cdr x)) 1 2 3))
; (cek-interp '(list 1 2 3 (list 4 5) 6))
; (cek-interp '((lambda a (cons 5 a)) 0 1 2 3 4))
; (cek-interp '(apply + (list 1 3 4)))
; (cek-interp '(apply (lambda x (cdr x)) (list 1 3 4)))
; (cek-interp '(apply (lambda (a b c) b) (list 1 (list 5 6) 4)))
;(cek-interp '(and #t #t))
;(cek-interp '(if #t #f #t))