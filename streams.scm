
#lang planet neil/sicp

(define the-empty-stream '())

(define (stream-ref s n)
  (if (= n 0)
      (car-stream s)
      (stream-ref (cdr-stream s) (- n 1))))


(define (stream-map proc s)
  (if (stream-null s)
      the-empty-stream
      (cons-stream (proc (car-stream s))
                   (stream-map proc (cdr-stream s)))))

(define (stream-for-each proc s)
  (if (stream-null s)
      'done
      (begin (proc (car-stream s))
             (stream-for-each proc (cdr-stream s)))))

(define (stream-filter pred s)
  (cond ((stream-null s) the-empty-stream)
        ((pred (car-stream s))
         (cons-stream (car-stream s)
                      (stream-filter pred
                                     (cdr-stream s))))
        (else (stream-filter pred (cdr-stream s)))))


(define (stream-enumerate-interval low hight)
  (if (> low hight)
      the-empty-stream
      (cons-stream low
                   (stream-enumerate-interval (+ 1 low) hight))))
;;____________________________________________________
;;
;;
;;____________________________________________________

(define (stream-null s)
  (null? s))

(define (cons-stream a b)
  (cons a (delay b)))

(define (car-stream stream)
  (car stream))

(define (cdr-stream stream)
  (force (cdr stream)))

(define (force delayed-object)
  (delayed-object))

(define (delay d)
  (lambda () d))