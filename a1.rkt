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
        