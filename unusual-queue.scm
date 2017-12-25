
#lang planet neil/sicp

(define (make-queue2)
  (let ((front '())
        (rear '()))
    (define (set-front! item)
      (set! front item))
    
    (define (set-rear! item)
      (set! rear item))

    (define (insert item)
      (let ((new-item (cons (cons rear item) '())))
        (cond ((null? front)
               (set-front! new-item)
               (set-rear! new-item)
               front)
              (else (set-cdr! rear new-item)
                    (set-rear! new-item)))))

    (define (insert-front item)
      (let ((new-item (cons '() item)))
        (set-car! (car (car front)) new-item)
        (set-front! (cons new-item front))))

    (define (delete)
      (set-front! (cdr front)))

    (define (delete-rear)
      (set-rear! (car (car rear))))

    (define (make-list queue)
      (if (null? queue)
          '()
          (cons (cdr (car queue))
                (make-list (cdr queue)))))

    (define (dispatch m)
      (cond ((eq? m "insert") insert)
            ((eq? m "insert-front") insert-front)
            ((eq? m "delete") delete)
            ((eq? m "delete-rear") delete-rear)
            ((eq? m "front") (make-list front))
            ((eq? m "rear") rear)
            (else "unknown message")))
                       
    dispatch))

      
(define queue (make-queue2))

((queue "insert") 1)

((queue "insert") 2)

((queue "insert") 3)

((queue "insert") 4)

((queue "insert") 5)

((queue "delete-rear"))

((queue "insert") 6)

((queue "delete"))

((queue "insert-front") 0)

(display (queue "front"))
