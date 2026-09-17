#lang 2d typed/racket
(require 2d/cond)
(provide Rank rank?
         all-ranks
         Score natural-blackjack? bust?
         ranks->score
         Judgement bj-win? win? lose? push?
         judge)

(module+ test
  (require typed/rackunit))

(define-type Rank (U 'Ace 2 3 4 5 6 7 8 9 10 'Jack 'Queen 'King))
(define-predicate rank? Rank)
(define-predicate rank-number? (U 2 3 4 5 6 7 8 9 10))
(define-predicate rank-face? (U 'Jack 'Queen 'King))
(define-predicate rank-ace? 'Ace)

(: all-ranks (Listof Rank))
(define all-ranks '(Ace 2 3 4 5 6 7 8 9 10 Jack Queen King))

(module+ test
  (check-eqv? (count rank-number? all-ranks) 9)
  (check-eqv? (count rank-face? all-ranks) 3)
  (check-eqv? (count rank-ace? all-ranks) 1))

(define-type Score (U Natural 'Natural-Blackjack 'Bust))
(define-predicate natural-blackjack? 'Natural-Blackjack)
(define-predicate bust? 'Bust)

(: ranks->score (-> (Listof Rank) [#:split? Boolean ] Score))
(define (ranks->score rs #:split? [split? #f])
  (: ranks->score* (-> (Listof Rank) Natural))
  (define (ranks->score* rs)
    (let ([score-without-aces (+ (foldl + 0 (filter rank-number? rs))
                                 (* 10 (count rank-face? rs)))])
      (let add-ace-scores : Natural ([score* : Natural score-without-aces]
                                     [ace-count : Natural (count rank-ace? rs)])
        (cond [(zero? ace-count) score*]
              [(<= (+ score* (* ace-count 11)) 21) (+ score* (* ace-count 11))]
              [else (add-ace-scores (+ score* 1) (- ace-count 1))]))))
  (let ([score* (ranks->score* rs)])
    (cond [(and (not split?) (= (length rs) 2) (= score* 21)) 'Natural-Blackjack]
          [(< 21 score*) 'Bust]
          [else score*])))

(module+ test
  (check-eqv? (ranks->score (list 'Jack 'Ace))
              'Natural-Blackjack)
  (check-eqv? (ranks->score (list 'Jack 'Ace) #:split? #t)
              21)
  (check-eqv? (ranks->score (list 'Jack 10 2))
              'Bust)
  (check-eqv? (ranks->score (list 'Jack 'Queen 'Ace))
              21))

(define-type Judgement (U 'BJ-Win 'Win 'Lose 'Push))
(define-predicate bj-win? 'BJ-Win)
(define-predicate win? 'Win)
(define-predicate lose? 'Lose)
(define-predicate push? 'Push)

(: judge (-> Score Score Judgement))
(define (judge p d)
  (define natural21? natural-blackjack?)
  #2dcond
  ╔═════════════════╦═════════════════╦═══════════╦═══════════════════╗
  ║                 ║ (natural21? d)  ║ (bust? d) ║       else        ║
  ╠═════════════════╬═════════════════╬═══════════╩═══════════════════╣
  ║ (natural21? p)  ║      'Push      ║           'BJ-Win             ║
  ╠═════════════════╬═════════════════╩═══════════════════════════════╣
  ║    (bust? p)    ║                      'Lose                      ║
  ╠═════════════════╣                 ╔═══════════╦═══════════════════╣
  ║                 ║                 ║           ║ (cond             ║
  ║     else        ║                 ║   'Win    ║   [(> p d) 'Win]  ║
  ║                 ║                 ║           ║   [(< p d) 'Lose] ║
  ║                 ║                 ║           ║   [else 'Push])   ║
  ╚═════════════════╩═════════════════╩═══════════╩═══════════════════╝)

(module+ test
  (check-pred push?
              (judge 'Natural-Blackjack
                     'Natural-Blackjack))
  (check-pred bj-win?
              (judge 'Natural-Blackjack
                     21))
  (check-pred lose?
              (judge 21
                     'Natural-Blackjack))
  (check-pred push?
              (judge 10 10))
  (check-pred win?
              (judge 15 10))
  (check-pred lose?
              (judge 10 15))
  (check-pred lose?
              (judge 'Bust 3))
  (check-pred win?
              (judge 2 'Bust)))
