#lang typed/racket

(module+ test
  (require typed/rackunit))

(define-type Suit (U 'Spade 'Club 'Heart 'Diamond))
(define-predicate suit? Suit)

(define-type Rank (U 'Ace 2 3 4 5 6 7 8 9 10 'Jack 'Queen 'King))
(define-predicate rank? Rank)
(define-predicate rank-number? (U 2 3 4 5 6 7 8 9 10))
(define-predicate rank-face? (U 'Jack 'Queen 'King))
(define-predicate rank-ace? 'Ace)

(struct card ([suit : Suit] [rank : Rank]))

(: suits (Listof Suit))
(define suits '(Spade Club Heart Diamond))

(: ranks (Listof Rank))
(define ranks '(Ace 2 3 4 5 6 7 8 9 10 Jack Queen King))

(module+ test
  (check-eqv? (count rank-number? ranks) 9)
  (check-eqv? (count rank-face? ranks) 3)
  (check-eqv? (count rank-ace? ranks) 1))

(: product (All (A B C) (-> (-> A B C) (Listof A) (Listof B) (Listof C))))
(define (product f xs ys)
  (if (null? xs)
      '()
      (append (map (lambda ([y : B]) (f (car xs) y)) ys)
              (product f (cdr xs) ys))))

(module+ test
  (check-equal? (product (inst cons Integer Symbol)
                         '(1 2 3)
                         '(a b c))
                (list '(1 . a) '(1 . b) '(1 . c)
                      '(2 . a) '(2 . b) '(2 . c)
                      '(3 . a) '(3 . b) '(3 . c))))

(: cards (Listof card))
(define cards
  (product card suits ranks))
