
#lang planet neil/sicp

(define (make-leaf symbol weight)
  (list 'leaf symbol weight))

(define (leaf? obj)
  (eq? (car obj) 'leaf))

(define (symbol-leaf x)
  (cadr x))

(define (weight-leaf x)
  (caddr x))


(define (left-branch tree)
  (car tree))

(define (right-branch tree)
  (cadr tree))

(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))

(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

(define (choose-branch bit tree)
  (cond ((= bit 0) (left-branch tree))
        ((= bit 1) (right-branch tree))
        (else (error "bad bit -- chose-branch" bit))))

(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
        '()
        (let ((next-branch
               (choose-branch (car bits) current-branch)))
          (if (leaf? next-branch)
              (cons (symbol-leaf next-branch)
                    (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))

(define (adjoin-set x set)
  (cond ((null? set)(list x))
        ((< (weight x) (weight (car set)))
         (cons x set))
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pair) (cadr pair))
                    (make-leaf-set (cdr pairs))))))

(define (includes symbol seq)
  (cond ((null? seq) false)
        ((eq? (car seq) symbol) true)
        (else (includes symbol (cdr seq)))))

(define (encode-symbol symbol tree)
  (cond ((leaf? tree) '())
        ((includes symbol (symbols (left-branch tree)))
         (cons 0
               (encode-symbol symbol (left-branch tree))))
        ((includes symbol (symbols (right-branch tree)))
         (cons 1
               (encode-symbol symbol (right-branch tree))))
        (else (error "bad symbol encode-symbol" symbol))))

(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
               (encode (cdr message) tree))))

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))

(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))


(define (successive-merge seq)
  (if (null? (cdr seq))
      (car seq)
      (let ((left (car seq))
            (right (cadr seq)))
        (if (null? right)
            seq
              (successive-merge
               (adjoin-set (make-code-tree left right) (cdr (cdr seq))))))))



(define tree (generate-huffman-tree (list (list 'a 4) (list 'b 2) (list 'c 1) (list 'd 1))))


(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))

(define result '(a d a b b c a))

(display (encode result tree))





