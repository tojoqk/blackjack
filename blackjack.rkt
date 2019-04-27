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

(define-type Score (U Integer 'Natural-Blackjack 'Bust))
(define-predicate natural-blackjack? 'Natural-Blackjack)
(define-predicate bust? 'Bust)

(: cards->score (-> (Listof card) Score))
(define (cards->score cs)
  (: cards->score* (-> (Listof card) Integer))
  (define (cards->score* cs)
    (let ([rs (map card-rank cs)])
      (let ([score-without-aces
             (+ (foldl + 0 (filter rank-number? rs))
                (* 10 (count rank-face? rs)))])
        (let add-ace-scores ([score* score-without-aces]
                             [ace-count (count rank-ace? rs)])
          (cond
            [(zero? ace-count) score*]
            [(<= (+ score* (* ace-count 11)) 21)
             (+ score* (* ace-count 11))]
            [else
             (add-ace-scores (+ score* 1)
                             (- ace-count 1))])))))
  (let ([score* (cards->score* cs)])
    (cond
      [(and (= (length cs) 2)
            (= score* 21))
       'Natural-Blackjack]
      [(< 21 score*)
       'Bust]
      [else
       score*])))

(module+ test
  (check-eqv? (cards->score (list (card 'Spade 'Jack)
                                  (card 'Spade 'Ace)))
              'Natural-Blackjack)
  (check-eqv? (cards->score (list (card 'Spade 'Jack)
                                  (card 'Spade 10)
                                  (card 'Spade 2)))
              'Bust)
  (check-eqv? (cards->score (list (card 'Spade 'Jack)
                                  (card 'Spade 'Queen)
                                  (card 'Spade 'Ace)))
              21))

(define-type Judgement (U 'Natural-Blackjack 'Win 'Lose 'Push))
(define-predicate win? 'Win)
(define-predicate lose? 'Lose)
(define-predicate push? 'Push)

(: judge (-> Score Score Judgement))
(define (judge player-score dealer-score)
  (cond
    [(and (natural-blackjack? player-score)
          (natural-blackjack? dealer-score))
     'Push]
    [(natural-blackjack? player-score)
     'Natural-Blackjack]
    [(natural-blackjack? dealer-score)
     'Lose]
    [(bust? player-score) 'Lose]
    [(bust? dealer-score) 'Win]
    [(< player-score dealer-score) 'Lose]
    [(> player-score dealer-score) 'Win]
    [else 'Push]))

(module+ test
  (check-pred push?
              (judge 'Natural-Blackjack
                     'Natural-Blackjack))
  (check-pred natural-blackjack?
              (judge 'Natural-Blackjack
                     21))
  (check-pred lose?
             (judge 21
                    'Natural-Blackjack))
  (check-pred push?
              (judge 10 10))
  (check-pred win?
              (judge 15 10)))
