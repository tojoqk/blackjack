#lang info
(define collection "blackjack")
(define deps '("base" "typed-racket-lib" "2d-lib" "https://github.com/tojoqk/graph-executor.git"))
(define build-deps '("scribble-lib" "racket-doc" "rackunit-lib" "rackunit-typed"))
(define scribblings '(("scribblings/blackjack.scrbl" ())))
(define pkg-desc "Typed BlackJack")
(define version "0.1")
(define pkg-authors '(tojoqk))
