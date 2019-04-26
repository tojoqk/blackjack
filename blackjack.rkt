#lang typed/racket

(define-type Suit (U 'Spade 'Club 'Heart 'Diamond))
(define-predicate suit? Suit)

(define-type Rank (U 'Ace 2 3 4 5 6 7 8 9 10 'Jack 'Queen 'King))
(define-predicate rank? Rank)
(define-predicate rank-number? (U 2 3 4 5 6 7 8 9 10))
(define-predicate rank-face? (U 'Jack 'Queen 'King))
(define-predicate rank-ace? 'Ace)
