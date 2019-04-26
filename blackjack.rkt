#lang typed/racket

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

(: product (All (A B C) (-> (-> A B C) (Listof A) (Listof B) (Listof C))))
(define (product f xs ys)
  (if (null? xs)
      '()
      (append (map (lambda ([y : B]) (f (car xs) y)) ys)
              (product f (cdr xs) ys))))

(: cards (Listof card))
(define cards
  (product card suits ranks))
