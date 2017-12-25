
#lang planet neil/sicp

(define (fringe li)
  (define (iter first tail)
    (cond ((and (null? tail)
                (null? first)) nil)
          ((null? first)
           (iter (car tail)
                 (cdr tail)))
          ((list? first)
           (iter (car first)
                 (cons (cdr first)
                       tail)))
          (else (cons first
                      (iter (car tail)
                            (cdr tail))))))
  (iter (car li) (cdr li)))

(define x (list (list(list 0) 1 2 (list 3 4)) (list (list 5 6 (list 7)) (list (list 8 9 ) (list 10))) (list 11 12)))

(display (fringe x))