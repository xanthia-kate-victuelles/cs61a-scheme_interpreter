(define (caar x) (car (car x)))
(define (cadr x) (car (cdr x)))
(define (cdar x) (cdr (car x)))
(define (cddr x) (cdr (cdr x)))

; Some utility functions that you may find useful to implement.

(define (cons-all first rests)
      (map (lambda(val) (append(list first) val))rests)
)
(define (zip pairs)
  (cons (map car pairs) (cons (map cadr pairs) nil)))

(define (enumerate s)
    ; BEGIN PROBLEM 16
    (define (helper index s)
      (if (null? s)
        s
        (cons (cons index (cons (car s) nil)) (helper (+ index 1) (cdr s)))
      )
    )
    (helper 0 s)
  )


;; Problem 17
;; List all ways to make change for TOTAL with DENOMS

(define (list-change total denoms)
  ; BEGIN PROBLEM 17
  (cond ((null? denoms) nil)
        ((< total 0) nil)
        ((= total 0) '(()))
        ((< total (car denoms)) (list-change total (cdr denoms)))
        (else (append (cons-all (car denoms) (list-change (- total (car denoms)) denoms))
          (list-change total (cdr denoms))))
  )
)
  ; END PROBLEM 17

;; Problem 18
;; Returns a function that checks if an expression is the special form FORM
(define (check-special form)
  (lambda (expr) (equal? form (car expr))))

(define lambda? (check-special 'lambda))
(define define? (check-special 'define))
(define quoted? (check-special 'quote))
(define let?    (check-special 'let))

;; Converts all let special forms in EXPR into equivalent forms using lambda
(define (let-to-lambda expr)
  (cond ((atom? expr)
         ; BEGIN PROBLEM 18
         expr
         ; END PROBLEM 18
         )
        ((quoted? expr)
         ; BEGIN PROBLEM 18
         expr
         ; END PROBLEM 18
         )
        ((or (lambda? expr)
             (define? expr))
         (let ((form   (car expr))
               (params (cadr expr))
               (body   (cddr expr)))
           ; BEGIN PROBLEM 18
            (append (cons form nil)(cons params (map let-to-lambda body)))
           ; END PROBLEM 18
           ))
        ((let? expr)
         (let ((values (cadr expr))
               (body   (cddr expr)))
           ; BEGIN PROBLEM 18
            ;using zip to combine values with zip
           (cons
                (cons 'lambda (cons (car (zip (let-to-lambda values))) (map let-to-lambda body)))
                (car(cdr (zip (let-to-lambda values))))
           )
           ; END PROBLEM 18
           ))
        (else
         ; BEGIN PROBLEM 18
          (map let-to-lambda expr)
         ; END PROBLEM 18
         )))
