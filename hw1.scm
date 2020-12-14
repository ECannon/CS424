;Question 1 -------------------------------------------------------------------------------------------------------------------


(define expand-where
  (lambda (x)
    (cons 'let (cons (extract-var-vals (get-after-where x)) (list (extract-expression x))))))



(define extract-expression
  (lambda (x)
    (cadr x)))


(define extract-var-vals
  (lambda (x)
    (cond
     [(null? x) '()]
     [else (cons (list (car x) (cadr x)) (extract-var-vals (cddr x)) )]
     )))


(define get-after-where
  (lambda (x)
    (cdddr x)))


(define test-expand-where
  (lambda ()
    (list
     (cond [(equal? (expand-where '(val (sqrt (+ x y)) where x 3 y 6)) '(let ((x 3) (y 6)) (sqrt (+ x y)))) 0]
	   [else 1])
     (cond [(equal? (expand-where '(val (e (* x y z i j)) where x 5 y 8 z 10 i 12 j 15)) '(let ((x 5) (y 8) (z 10) (i 12) (j 15)) (e (* x y z i j)))) 0]
	   [else 1])
     (cond [(equal? (expand-where '(val (lcm (x y)) where x 12 y 24)) '(let ((x 12) (y 24)) (lcm (x y)))) 0]
	   [else 1])
     (cond [(equal? (expand-where '(val (expt (x y)) where x 2 y 6)) '(let ((x 2) (y 6)) (expt (x y)))) 0]
	   [else 1])
     )))

;End Question 1 ---------------------------------------------------------------------------------------------------------------


;Question 2------------------------------------------------------------------------

(define subseq-alignments
  (lambda (xs ys)
    (define ll (get-list-length xs))
    (go-to-subseq-alignments xs ys 0)

    ))


(define go-to-subseq-alignments
  (lambda (xs ys i)
    (cond
     [(< (get-list-length ys) (get-list-length xs)) '()]
     [(equal? (get-start-to ys (get-list-length xs)) xs) (cons i (go-to-subseq-alignments xs (cdr ys) (+ i 1)))]
     [else (go-to-subseq-alignments xs (cdr ys) (+ i 1))])))



(define get-start-to
  (lambda (ys n)
    (cond
     [(= n 0) '()]
     [else (cons (car ys) (get-start-to (cdr ys) (- n 1)))])))




(define get-list-length
  (lambda (xs)
    (cond
     [(null? xs) 0]
     [else (+ 1 (get-list-length (cdr xs)))])))

;When I call this test function, scheme prints out #<procedure:>> and then it prints my list.. The test function that I wrote for Q1 seems to be the same but doesnt print #<procedure:>>.. Do you know why this is?
(define test-subseq-alignments
  (lambda ()
    (list
     (cond [(equal? (subseq-alignments '(a b) '(a b b c a b a a b)) '(0 4 7)) 0]
	   [else 1])
     (cond [(equal? (subseq-alignments '(a) '(a b b c a b a a b)) '(0 4 6 7)) 0]
	   [else 1])
     (cond [(equal? (subseq-alignments '(c) '(a b b c a b a a b)) '(3)) 0]
	   [else 1])
     (cond [(equal? (subseq-alignments '(a b c) '(a b b c a b a a b)) '()) 0]
	   [else 1])
     
     )))


;End question 2-------------------------------------------------------------------------------------------------------

;Question 3---------------------------------------------------------------------

;This part was what I sent to you for review ad you said it wasn't entirely right(I left it here just incase my second attempt below is entirely wrong)

;(define fibonacci (lambda (n) (fibonacci-cps n (lambda (x) x))))
;
;(define fibonacci-cps
;  (lambda (n k)
;    (cond
;      [(zero? n) (k 0)]
;      [(= n 1) (k 1)]
;      [else
;       (fibonacci-cps
;        (- n 1)
;        (lambda (a) (fibonacci-cps
;                (- n 2)
;                (lambda (b) (k (+ a b))))))])))

(define fibonacci (lambda (n) (fibonacci-cps (lambda (x) x) n)))
(define fibonacci-cps
  (λ (c n)
    (c= (λ (is-n-0)
          (if is-n-0
              (c 0)
              (c= (λ (is-n-1)
                    (if is-n-1
                        (c 1)
                        (c- (λ (nm1)
                              (fibonacci-cps (λ (fnm1)
                                         (c- (λ (nm2)
                                               (fibonacci-cps (λ (fnm2)
                                                          (c+ c fnm1 fnm2))
                                                        nm2))
                                             n 2))
                                       nm1))
                            n 1)))
                  n 1)))
        n 0)))

(define c= (λ (c x y) (c(= x y))))
(define c- (λ (c x y) (c(- x y))))
(define c+ (λ (c x y) (c(+ x y))))

(define test-fibonacci
  (lambda ()
    (list
     (cond [(= (fibonacci 7) 13) 0]
	   [else 1])
     (cond [(= (fibonacci 9) 34) 0]
	   [else 1])
     (cond [(= (fibonacci 13) 233) 0]
	   [else 1])
     (cond [(= (fibonacci 23) 28657) 0]
	   [else 1])
     )))

;End QUestion 3------------------------------------------------------
;Question 4 ------------------------------------------------------------------------------------

(define tex-eval
  (λ (e x)
    (cond ((equal? e 'x) x)
	  ((number? e) e)
	  (else
	   (let ((op (car e)) (args (cdr e)))
	     (apply (lookup-op op) args))))))


(define op-table
  (list(list '+ (lambda (u v) (simplify+ (ddx u) (ddx v)))) 
       (list '- (lambda (u v) (simplify- (ddx u) (ddx v)))) 
       (list '* (lambda (u v) (simplify+ (simplify* u (ddx v)) (simplify* v (ddx u))))) 
       (list '/ (lambda (u v) (simplify/ (simplify- (simplify* v (ddx u)) (simplify* u (ddx v))) (simplify* v v))))
       (list 'exp (lambda (u) (simplify* (ddx u) (simplify-exp u))))
       (list 'expt (lambda (u v) (simplify-expt (simplify* v  u) (simplify- v 1))))
       (list 'log (lambda (u) (simplify* (simplify/ 1  u) (ddx u))))       
       (list 'sin (lambda (u) (simplify* (ddx u) (simplify-cos u))))      
       (list 'cos (lambda (u) (simplify* (ddx u) (simplify* -1 (simplify-sin u)))))))



(define lookup-op
  (lambda (op)
    (cadr (assoc op op-table))))


(define lookup-deriv-calc
  (lambda (op)
    (caddr (assoc op op-table))))


(define ddx
  (lambda (e)
    (cond ((equal? e 'x) 1)    
	  ((number? e) 0)	      
	  (else
	   (let ((op (car e)) (args (cdr e)))
	     (apply (lookup-op op) args))))))


(define simplify+
  (lambda (x y)
    (cond
     [(and (number? x) (number? y)) (+ x y)]
     [(equal? x 0) y]
     [(equal? y 0) x]
     [(equal? y x) (simplify* 2 x)]
     [else (list '+ x y)])))

(define simplify-
  (lambda (x y)
    (cond
     [(and (number? x) (number? y)) (- x y)]
     [(equal? x 0) (list '- y)]
     [(equal? y 0) x]
     [(equal? y x) 0]
     [else (list '- x y)])))

(define simplify*
  (lambda (x y)
    (cond [(and (number? x) (number? y)) (* x y)]
	  [(equal? x 0) 0]
	  [(equal? y 0) 0]
	  [(equal? x 1) y]
	  [(equal? y 1) x]
	  [(equal? x -1) '- y]
	  [(equal? y -1) '- x]
	  (else [list '* x y]))))

(define simplify/
  (lambda (x y)
    (cond
     [(and (number? x) (number? y)) (/ x y)]
     [(equal? x 0) 0]
     [else (list '/ x y)])))

(define simplify-exp
  (lambda (x)
    (cond
     [(number? x) (exp x)]
     [else (list 'exp x)])))

(define simplify-expt
  (lambda (x y)
    (cond
     [(and (number? x) (number? y)) (expt x y)]
     [(equal? y 0) 1]
     [(equal? x 0) 0]
     [(equal? x 1) 1]
     [(equal? y 1) x]
     [else (list 'expt x y)])))

(define simplify-sin
  (lambda (x)
    (cond
     [(number? x) (sin x )]
     [else (list 'sin x)])))

(define simplify-cos
  (lambda (x)
    (cond
     [(number? x) (cos x )]
     [else (list 'cos x)])))

(define test-ddx
  (lambda ()
    (list
     (cond [(equal? (ddx '(* (+ x 5) (+ x 2))) ' (+ (+ x 5) (+ x 2))) 0]
	   [else 1])
     (cond [(equal? (ddx '(/ (+ x 8) (+ x -3))) '(/ (- (+ x -3) (+ x 8)) (* (+ x -3) (+ x -3)))) 0]
	   [else 1])
     (cond [(equal?  (ddx '(log (* x 3) )) '(* (/ 1 (* x 3)) 3)) 0]
	   [else 1])
     (cond [(equal?  (ddx '(expt (* x 5) 3 )) '(expt (* 3 (* x 5)) 2)) 0]
	   [else 1])
     (cond [(equal?  (ddx '(exp (* x 5))) '(* 5 (exp (* x 5)))) 0]
	   [else 1])
     (cond [(equal?  (ddx '(sin (* x 3))) '(* 3 (cos (* x 3)))) 0]
	   [else 1])
     (cond [(equal?  (ddx '(sin (* x 4))) '(* 4 (cos (* x 4)))) 0]
	   [else 1])
     )))


		  

;End Question 4 -----------------------------------------------------------------------


;Question 5----------------------------------------------------------------------


;iterate 
(define iterate
  (lambda (f x0 i)
    (cond
     [(= i 0) x0]
     [else (iterate f (f x0) (- i 1))])))

(define test-iterate
  (lambda ()
    (list
     (cond [(equal? (iterate sqrt 16 3) 1.4142135623730951) 0]
	   [else 1])
     (cond [(equal? (iterate cdr '(a b c d e f g) 3) '(d e f g)) 0]
	   [else 1])
     (cond [(equal? (iterate sin 2 4) 0.6516062636498291) 0]
	   [else 1])
     (cond [(equal? (iterate log 10 2) 0.834032445247956) 0]
	   [else 1])
     )))



;find-iter
(define find-iter
  (lambda (f x0 x m)
    (go-to-find-iter f x0 x m 0)))

(define go-to-find-iter
  (lambda (f x0 x m i)
    (cond
     [(= m 0) #f]
     [(equal? x0 x) i]
     [else (go-to-find-iter f (f x0) x (- m 1) (+ i 1))])))

(define test-find-iter
  (lambda ()
    (list
     (cond [(equal? (find-iter cdr '(a b c d e f g) '(f g) 100) 5) 0]
	   [else 1])
     (cond [(equal? (find-iter sqrt 16 3.2 100) #f) 0]
	   [else 1])
     )))



;find-iter2
(define find-iter2
  (lambda (f x0 mf g y0 mg)
    (go-to-find-iter2 f x0 (- mf 1) g y0 (- mg 1) (- mf 1) (- mg 1))))

(define go-to-find-iter2
  (lambda (f x0 mf g y0 mg i j)
    (cond
     [(equal? (iterate f x0 i) (iterate g y0 j)) (cons i (cons j '()))]
     [(and (= j 0) (= i 0)) #f]
     [(= i 0) (go-to-find-iter2 f x0 mf g y0 (- mg 1) mf (- mg 1))]

     [else (go-to-find-iter2 f x0 mf g y0 mg (- i 1) j)]

     )))

(define test-find-iter2
  (lambda()
    (list
     (cond [(equal? (find-iter2 cdr '(a b c d d d e f) 8 (λ (xs) (cons 'd xs)) '(e f) 20) '(3 3)) 0]
	   [else 1])
     (cond [(equal? (find-iter2 cdr '(a b c d d d e f) 8 (λ (xs) (cons 'd xs)) '(d d) 20) #f) 0]
	   [else 1])
     )))



;-----------------------------------------------------

