#lang racket

;Problem 1
(define countdown
  (λ (x)
  (if (< x 0)
  ;then
  `()
  ;else
  (cons x (countdown (- x 1))))))

;Problem 2
(define insertL
  (λ (x y list)
    (if (empty? list)
        ;then
        '()
        ;else
        (if (eqv? (car list) x)
            ;then
            (cons y (cons (car list)
                          (insertL x y (cdr list))))
            ;else
            (cons (car list) (insertL x y (cdr list)))))))

;Problem 3
(define remv-1st
  (λ (x list)
      (if (empty? list)
      ;then
      '()
      ;else
      (if (eqv? (car list) x)
          ;then
          (cdr list)
          ;else
          (cons (car list) (remv-1st x (cdr list)))))))

;Problem 4
(define remove-from
  (λ (predicate list)
    (if (empty? list)
        ;then
        '()
        ;else
        (if (predicate (car list))
                   ;then
                   (remove-from predicate (cdr list))
                   ;else
                   (cons (car list) (remove-from predicate (cdr list)))))))

;Problem 5
(define map
  (λ (p list)
    (if (empty? list)
        ;then
        '()
        ;else
        (cons (p (car list))
              (map p (cdr list))))))

;Problem 6
(define zip
  (λ (list1 list2)
    (if (empty? list1)
        ;then
        '()
        ;else
        (if (empty? list2)
            ;then
            '()
            ;else
            (cons (cons (car list1) (car list2)) (zip (cdr list1) (cdr list2)))))))

;Problem 7
(define list-index-ofv
  (λ (x lst) 
    (let loop ((lst lst) (index 0))
    (cond
      ;if
      ((empty? lst) 'bad-data)
      ;elif
      ((eqv? x (car lst)) index)
      (else
       (loop (cdr lst) (+ index 1)))))))

;Problem 8
(define append
  (λ (ls1 ls2)
    (if (empty? ls1)
        ;then
        ls2
        ;else
        (cons (car ls1) (append (cdr ls1) ls2)))))

;Problem 9
(define reverse
  (λ (ls)
    (if (empty? ls)
        ;then
        '()
        ;else
        (append (reverse (cdr ls)) (list (car ls))))))

;Problem 10
(define repeat
  (λ (ls num)
    (if (zero? num)
        ;then
        '()
        ;else
        (append ls (repeat ls (- num 1))))))

;Problem 11
(define same-lists*
  (λ (list1 list2)
    (cond
      ((and (empty? list1) (empty? list2)) #t)
      ((or (empty? list1) (empty? list2)) #f)

      ((and (list? (car list1)) (list? (car list2)))
       (and (same-lists* (car list1) (car list2))
            (same-lists* (cdr list1) (cdr list2))))
      ((eqv? (car list1) (car list2))
       (same-lists* (cdr list1) (cdr list2)))
      (else #f))))

;Problem 12
;'((w . (x . ())) y (z . ()))
;(equal? '((w x) y (z)) '((w . (x . ())) y (z . ())))

;Problem 13
(define binary->natural
  (λ (lst)
    (define binary-helper
      (λ (h-list power)
        (if (empty? h-list)
            ;then
            0
            ;else
            (+ (* (car h-list) (expt 2 power))
               (binary-helper (cdr h-list) (+ power 1))))))
  (binary-helper lst 0)))

;Problem 14
(define div
  (λ (x y)
    (cond
      ((zero? y) 'bad-data)
      ((< x y) 0)
      (else (+ 1 (div (- x y ) y))))))

;Problem 15
(define append-map
  (λ (p lst)
    (cond
      ((empty? lst) '())
      (else (append (p (car lst))
                    (append-map p (cdr lst)))))))

;Problem 16
(define set-difference
  (λ (lst1 lst2)
    (cond
      ((empty? lst1) '())
      ((member (car lst1) lst2)
       (set-difference (cdr lst1) lst2))
      (else (cons (car lst1) (set-difference (cdr lst1) lst2))))))

;Problem 17 (I'm not sure with this one)

;(define G-1
  ;(λ (i)
    ;(λ (n m)
      ;(cond
        ;((zero? m) n)  ; Used to be: ((zero? m) (λ (n m) (n)))
        ;(else (add1 ((G 0) n (sub1 m)))))))))

;(define G-2
  ;(λ (i)
    ;(λ (n m)
      ;(cond
        ;((zero? m) 0)                    ; Used to be ((zero? m) (λ (n m) 0))
        ;(else ((G 0) n ((G 1) n (sub1 m)))))))))

;(define G-3
  ;(λ (i)
    ;(λ (n m)
      ;(cond
        ;((zero? m) (cond
                     ;((zero? i) n)
                     ;((zero? (sub1 i)) 0)
                     ;(else 1)))
        ;((zero? i) (add1 ((G 0) n (sub1 m))))
        ;(else ((G (sub1 i)) n ((G i) n (sub1 m))))))))

;Problem 18
(define powerset
  (λ (lst)
    (if (empty? lst)
        ;then
        '(())
        ;else
        (let ((rest (powerset (cdr lst))))
          (append rest
                  (map
                   (λ (subset)
                     (cons (car lst) subset))
                   rest))))))

;Problem 19
(define (cartesian-product sets)
  (if (empty? sets)
      ;then
      '(())
      ;else
      (let ((rest (cartesian-product (cdr sets))))
        (append-map
         (λ (elem)
           (map
            (λ (subset)
              (cons elem subset))
            rest))
         (car sets)))))

;Problem 20
(define C
  (letrec ((f
            (λ (int)
              (cond
                ((zero? int) 1)
                ((even? int) (f (/ int 2)))
                (else (f (+ (* 3 int) 1)))))))
  (λ (int) 1)))

;Problem 21

(print-reader-abbreviations #f)
(print-as-expression #f) 

(define quine
  '((λ (input) (list input (list 'quote input)))
    '(λ (input) (list input (list 'quote input)))))