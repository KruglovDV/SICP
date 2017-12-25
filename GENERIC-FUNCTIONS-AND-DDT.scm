
#lang planet neil/sicp

(define (square x)
  (* x x))


;;----------------
;; SCHEME-NUMBER
;;----------------
(define (install-scheme-number-pscksge)
  (define (tag x)
    (attach-tag 'scheme-number x))
  
  (define (raise-number->rational n)
  (make-rational n 0))
  
  (put 'equ? '(scheme-number scheme-number)
       (lambda (x y)(= x y)))

  
  (put 'add '(scheme-number scheme-number)
       (lambda (x y) (tag(+ x y))))
  (put 'sub '(scheme-number scheme-number)
       (lambda (x y) (tag (- x y))))

  (put 'mul '(scheme-number scheme-number)
       (lambda (x y) (tag (* x y))))

  (put 'div '(scheme-number scheme-number)
       (lambda (x y) (tag (/ x y))))

  (put 'make 'scheme-number
       (lambda (x) (tag x)))

  (put 'raise '(scheme-number) raise-number->rational)
  'done)
;;----------------

;;----------------
;; RATIONAL
;;----------------

(define (install-rational-package)
  ;; внутренние процедуры
  (define (raise-rational->complex n)
    (make-complex-from-real-imag (/ (numer n) (denom n)) 0))
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (make-rat n d)
    (let ((g (gcd n d)))
      (cons (/ n g) (/ d g))))
  (define (add-rat x y)
    (make-rat (+ (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (sub-rat x y)
    (make-rat (- (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (mul-rat x y)
    (make-rat (* (numer x) (numer y))
              (* (denom x) (denom y))))
  (define (div-rat x y)
    (make-rat (* (numer x) (denom y))
              (* (denom x) (numer y))))
  ;; интерфейс к остальной системе
  (define (tag x) (attach-tag 'rational x))
  (put 'equ? '(rational rational)
       (lambda (x y)(and (= (numer x) (numer y))
                         (= (denom x) (denom y)))))
  (put 'add '(rational rational)
       (lambda (x y) (tag (add-rat x y))))
  (put 'sub '(rational rational)
       (lambda (x y) (tag (sub-rat x y))))
  (put 'mul '(rational rational)
       (lambda (x y) (tag (mul-rat x y))))
  (put 'div '(rational rational)
       (lambda (x y) (tag (div-rat x y))))
  (put 'make 'rational
       (lambda (n d) (tag (make-rat n d))))
  (put 'raise '(rational) raise-rational->complex)
  'done)

;;----------------

;; ---------------
;; FUNCTIONS STORE
;; ---------------

(define (generic-store)
  
  (define store (list))

  (define (get-name node)
    (car node))

  (define (get-type node)
    (cadr node))

  (define (get-fn node)
    (caddr node))

  (define (compare-lists list1 list2)
    
    (cond ((and (null? list1) (null? list2))
           true)
          ((eq? (car list1) (car list2))
           (compare-lists (cdr list1) (cdr list2)))
          (else false)))
  
  (define (compare name1 name2 type1 type2)
    (and (eq? name1 name2) (compare-lists type1 type2)))
  
  (define (isSuite store-node name type)
    (let ((node-name (get-name store-node))
          (node-type (get-type store-node)))
      (cond ((and (list? node-type) (list? type))
             (compare node-name name node-type type))
            ((list? node-type)
             (compare node-name name node-type (list type)))
            ((list? type)
             (compare node-name name (list node-type) type)))))
  
  (define (find name type)
    (define (finder store)
      (cond ((null? store) false)
            ((isSuite (car store) name type)
             (get-fn (car store)))
            (else (finder (cdr store)))))
    (finder store))
            
  (define (dispatch message name type fn)
    (cond ((eq? message 'put)
           (set! store (append (list(list name type fn)) store)))
          ((eq? message 'get)
           (find name type))
          (else (error "unkonwn message -- DISPATCH " message))))
  dispatch)

(define store (generic-store))

(define (put name type fn)
  (store 'put name type fn))

(define (get name type)
  (store 'get name type (lambda (x) x)))

;; -----------------------------


(define (attach-tag type-tag contents)
  (if (number? contents)
      contents
      (cons type-tag contents)))

(define (type-tag datum)
  (cond ((pair? datum) (car datum))
        ((number? datum) 'scheme-number)
        (else (error "Некорректные помеченные данные -- TYPE-TAG" datum))))

(define (contents datum)
  (cond ((pair? datum) (cdr datum))
        ((number? datum) datum)
        (else (error "Некорректные помеченные данные -- CONTENTS" datum))))

(define (toLevel a1 a2)
  (define value-of-types (list (list 'scheme-number 1)
                               (list 'rational 2) (list 'complex 3)))
  (define (get-weight t)
    (define (iter ls)
      (let ((elemet (car ls)))
        (cond ((null? ls)
               (error "type did found" t))
              ((eq? (car elemet) t)
               (cadr elemet))
              (else (iter (cdr ls))))))
    (iter value-of-types))
  
  (define (compare a1 a2)
    (let ((first-weight (get-weight (type-tag a1)))
          (second-weight (get-weight (type-tag a2))))
      (cond ((= first-weight second-weight)
             (list a1 a2))
            ((< first-weight second-weight)
             (compare (raise a1) a2))
            (else (compare a1 (raise a2))))))
  (compare a1 a2))
  
  

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((opp (get op type-tags)))
      (if opp
          (apply opp (map contents args))
          (let ((arg1 (car args))
                (arg2 (cadr args)))
            (let ((equal (toLevel arg1 arg2)))
              (apply apply-generic (cons op equal))))))))


;; -----------------------------------
;; DECART COMPLEX
;; -----------------------------------

(define (install-rectangular-package)
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (make-from-real-imag x y) (cons x y))
  (define (magnitude z)
    (sqrt (+ (square (real-part z)) (square (imag-part z)))))
  (define (angle z)
    (atan (imag-part z) (real-part z)))
 (define (make-from-mag-ang r a)
   (cons (* r (cos a)) (* r (sin a))))
;; интерфейс к остальной системе
  (define (tag x) (attach-tag 'rectangular x))
  (put 'real-part '(rectangular) real-part)
  (put 'imag-part '(rectangular) imag-part)
  (put 'magnitude '(rectangular) magnitude)
  (put 'angle '(rectangular) angle)
  (put 'make-from-real-imag 'rectangular
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'rectangular
       (lambda (r a) (tag (make-from-mag-ang r a))))
'done)
;-------------------------------------------


;;------------------------------------------
;; POLAR COMPLEX
;;------------------------------------------

(define (install-polar-package)
  (define (magnitude z) (car z))
  (define (angle z) (cdr z))
  (define (make-from-mag-ang r a) (cons r a))
  (define (real-part z)
    (* (magnitude z) (cos (angle z))))
  (define (imag-part z)
    (* (magnitude z) (sin (angle z))))
  (define (make-from-real-imag x y)
    (cons (sqrt (+ (square x) (square y))) (atan y x)))
  ;; интерфейс к остальной системе
  (define (tag x) (attach-tag 'polar x))
  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'magnitude '(polar) magnitude)
  (put 'angle '(polar) angle)
  (put 'make-from-real-imag 'polar
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'polar
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)
;;-------------------------------------------------

(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z) (apply-generic 'angle z))

;;------------------
;; COMPLEX
;;------------------

(define (install-complex-package)
  ;; процедуры, импортируемые из декартова
  ;; и полярного пакетов
  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag '(rectangular)) x y))
  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang '(polar)) r a))
 ;; внутренние процедуры
  (define (add-complex z1 z2)
    (make-from-real-imag (+ (real-part z1) (real-part z2))
                         (+ (imag-part z1) (imag-part z2))))
  (define (sub-complex z1 z2)
    (make-from-real-imag (- (real-part z1) (real-part z2))
                         (- (imag-part z1) (imag-part z2))))
  (define (mul-complex z1 z2)
    (make-from-mag-ang (* (magnitude z1) (magnitude z2))
                       (+ (angle z1) (angle z2))))
  (define (div-complex z1 z2)
    (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
                       (- (angle z1) (angle z2))))
  ;; интерфейс к остальной системе
  (define (tag z) (attach-tag 'complex z))
  (put 'equ? '(complex complex)
       (lambda (x y)(and (= (real-part x) (real-part y))
                         (= (imag-part x) (imag-part y)))))
  (put 'add '(complex complex)
       (lambda (z1 z2) (tag (add-complex z1 z2))))
  (put 'sub '(complex complex)
       (lambda (z1 z2) (tag (sub-complex z1 z2))))
  (put 'mul '(complex complex)
       (lambda (z1 z2) (tag (mul-complex z1 z2))))
  (put 'div '(complex complex)
       (lambda (z1 z2) (tag (div-complex z1 z2))))
  (put 'make-from-real-imag 'complex
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'complex
       (lambda (r a) (tag (make-from-mag-ang r a))))
  (put 'real-part '(complex) real-part)
  (put 'imag-part '(complex) imag-part)
  (put 'magnitude '(complex) magnitude)
  (put 'angle '(complex) angle)
  'done)
;;-------------------
(install-scheme-number-pscksge)
(install-rational-package)
(install-rectangular-package)
(install-polar-package)
(install-complex-package)
;;---------------------


(define (raise n) (apply-generic 'raise n))
(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))
(define (equ? x y) (apply-generic 'equ? x y))

(define (make-scheme-number n)
  ((get 'make '(scheme-number)) n))


(define (make-rational n d)
  ((get 'make '(rational)) n d))

(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag '(complex)) x y))

(define (make-complex-from-mag-ang r a)
  ((get 'make-from-mag-ang '(complex)) r a))

(define x (make-complex-from-real-imag 2 5))
(define y (make-complex-from-real-imag 2 5))
(define a (make-scheme-number 3))
(define b (make-scheme-number 3))
(define c (make-rational 4 2))
(define e (make-rational 1 2))


(add b c)




