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
      ((empty? lst) "bad-data")
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
    ((if (zero? num)
         ;then
         '()
         ;else
         (cons (append ls ls) (repeat ls (- num 1)))))))