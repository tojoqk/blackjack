#lang typed/racket

(require "../blackjack.rkt"
         graph-executor)

(struct player ([wallet : Natural])
  #:type-name Player
  #:transparent)

(: show-wallet (-> Player Player))
(define (show-wallet st)
  (message (format "Your wallet: ~a" (player-wallet st)))
  st)

(define-type Entry (U 'entry 'terminal))

(: entry-graph (-> String
                   (Values (-> (Listof (List String AnyNode (Code (-> Player Any))))
                               (OpenGraph Entry Player))
                           (Node Entry Player))))
(define (entry-graph g)
  (define e-node ((inst node Entry Player) g))
  (define e-edge (inst edge Entry Player))
  (define e-bridge (inst bridge Entry Player))
  (define e-graph (inst open-graph Entry Player))

  (define entry (e-node "Entry" #:type 'entry #:trans (code show-wallet)))
  (define terminal (e-node "Home" #:type 'entry))
  (values
   (lambda (outputs)
     (e-graph g
              #:edges (list (e-edge "Go to Home" #:dom entry #:cod terminal #:priority -1))
              #:bridges (map (lambda ([output : (List String AnyNode (Code (-> Player Any)))])
                               (e-bridge (first output)
                                         #:dom entry #:cod (second output)
                                         #:trans (third output)))
                             outputs)))
   entry))

(struct bj-state ([player : Player]
                  [shoe : (Listof (U 'cut-card Card))]
                  [discard : (Listof Card)]
                  [seen-cut-card? : Boolean]
                  [num-of-decks : Positive-Integer])
  #:type-name BJ-State
  #:transparent)

(: make-cards (-> Positive-Integer (Listof Card)))
(define (make-cards n)
  (let ([n-1 (sub1 n)])
    (if (zero? n-1)
        cards
        (append cards (make-cards n-1)))))

(: make-bj-state (-> Player Positive-Integer BJ-State))
(define (make-bj-state pl n)
  (bj-state pl '() (make-cards n) #f n))

(define-type BJ-Type (U 'player 'dealer))

(: reset-seen-cut-card (-> BJ-State BJ-State))
(define (reset-seen-cut-card st)
  (struct-copy bj-state st [seen-cut-card? #f]))

(: show (All (A) (-> String (-> A A))))
(define ((show str) st)
  (message str)
  st)

(: reset-shoe (-> BJ-State BJ-State))
(define (reset-shoe st)
  (let ([shoe : (Listof Card) (filter card? (bj-state-shoe st))]
        [discard (bj-state-discard st)])
    (let* ([new-shoe (prompt-shuffle "Shuffle Cards" (append shoe discard))]
           [block-margin (quotient (length new-shoe) 5)])
      (define-values (buffer head) (split-at new-shoe block-margin))
      (struct-copy bj-state st
                   [shoe (append head (cons 'cut-card buffer))]))))

(: initial-cut-messsage (-> BJ-State BJ-State))
(define (initial-cut-messsage st)
  (message
   (format "Dealer mixed and shuffled ~a ~a."
           (bj-state-num-of-decks st)
           (if (= (bj-state-num-of-decks st) 1)
               "deck"
               "decks")))
  st)

(: show-player-wallet (-> BJ-State BJ-State))
(define (show-player-wallet st)
  (show-wallet (bj-state-player st))
  st)

(: bj-graph (-> String
                (Values (-> (Node Entry Player)
                            (Code (-> BJ-State Player))
                            (Node BJ-Type BJ-Playing)
                            (Code (-> BJ-State BJ-Playing))
                            (OpenGraph BJ-Type BJ-State))
                        (Node BJ-Type BJ-State)
                        (Node BJ-Type BJ-State))))
(define (bj-graph g)
  (define bj-node ((inst node BJ-Type BJ-State) g))
  (define bj-edge (inst edge BJ-Type BJ-State))
  (define bj-bridge (inst bridge BJ-Type BJ-State))
  (define bj-graph (inst open-graph BJ-Type BJ-State))
  (define entry-any-node (any-node player?))
  (define bj-playing-any-node (any-node bj-playing?))
  (define bj-show (inst show BJ-State))

  (define hello (bj-node "Hello" #:type 'dealer))
  (define idle (bj-node "Idle" #:type 'dealer #:trans (code show-player-wallet)))
  (define betting (bj-node "Betting" #:type 'dealer))

  (values
   (lambda (entry-node to-entry playing-node to-playing)
     (bj-graph g
               #:edges
               (list (bj-edge "Initial Cut" #:dom hello #:cod idle #:mode 'auto
                              #:trans (code (compose initial-cut-messsage reset-shoe)))
                     (bj-edge "Cut"
                              #:dom idle #:cod idle #:mode 'auto
                              #:when (code bj-state-seen-cut-card?)
                              #:trans (code
                                       (compose (bj-show "Dealer collected discards and shuffled the shoe.")
                                                (compose reset-shoe reset-seen-cut-card))))
                     (bj-edge "Bet"
                              #:dom idle #:cod betting)
                     (bj-edge "Wallet is empty"
                              #:dom betting #:cod idle #:mode 'auto
                              #:priority +1
                              #:when (code (compose zero?
                                                    (compose player-wallet bj-state-player)))
                              #:trans (code (bj-show "Your wallet is empty."))))
               #:bridges
               (list (bj-bridge "Input"
                                #:mode 'auto
                                #:dom betting
                                #:cod (bj-playing-any-node playing-node)
                                #:trans to-playing)
                     (bj-bridge "Go to Lobby"
                                #:dom idle
                                #:cod (entry-any-node entry-node)
                                #:trans to-entry))))
   hello idle))

(struct bj-playing bj-state ([player-hand : (Listof Card)]
                             [dealer-hand : (Listof Card)]
                             [bet : Positive-Integer]
                             [seen-empty? : Boolean])
  #:type-name BJ-Playing
  #:transparent)

(: place-bet (-> BJ-State BJ-Playing))
(define (place-bet st)
  (let ([w (player-wallet (bj-state-player st))])
    (assert w positive-integer?)
    (let ([bet (prompt "Place your bet." `(range 1 ,w))])
      (bj-playing (struct-copy player (bj-state-player st)
                               [wallet (assert (- w bet) natural?)])
                  (bj-state-shoe st)
                  (bj-state-discard st)
                  (bj-state-seen-cut-card? st)
                  (bj-state-num-of-decks st)
                  '()
                  '()
                  bet
                  #f))))

(: pop-card (-> BJ-Playing (Values (Option Card) BJ-Playing)))
(define (pop-card st)
  (let ([cs (bj-state-shoe st)])
    (if (null? cs)
        (values #f
                (struct-copy bj-playing st [seen-empty? #t]))
        (let ([fst (first cs)]
              [rst (rest cs)])
          (cond [(eq? fst 'cut-card)
                 (message "Dealer has seen cut card.")
                 (pop-card (struct-copy bj-playing st
                                        [shoe #:parent bj-state rst]
                                        [seen-cut-card? #:parent bj-state #t]))]
                [else (values fst (struct-copy bj-playing st
                                               [shoe #:parent bj-state rst]))])))))

(: deal-to-player (-> BJ-Playing BJ-Playing))
(define (deal-to-player st)
  (define-values (c new-st) (pop-card st))
  (if c
      (struct-copy bj-playing new-st
                   [player-hand (cons c (bj-playing-player-hand new-st))])
      new-st))

(: deal-to-dealer (-> BJ-Playing BJ-Playing))
(define (deal-to-dealer st)
  (define-values (c new-st) (pop-card st))
  (if c
      (struct-copy bj-playing new-st
                   [dealer-hand (cons c (bj-playing-dealer-hand new-st))])
      new-st))

(: less-than? (-> Integer (-> Integer Boolean)))
(define ((less-than? x) y)
  (< y x))

(: player-hand-count (-> BJ-Playing Natural))
(define (player-hand-count st)
  (length (bj-playing-player-hand st)))

(: dealer-hand-count (-> BJ-Playing Natural))
(define (dealer-hand-count st)
  (length (bj-playing-dealer-hand st)))

(: bj-judge (-> BJ-Playing Judgement))
(define (bj-judge st)
  (judge (cards->score (bj-playing-player-hand st))
         (cards->score (bj-playing-dealer-hand st))))

(: card->display (-> (U '* Card) String))
(define (card->display c)
  (if (eq? c '*)
      "[_ _]"
      (format "[~a ~a]"
              (string-ref (symbol->string (card-suit c)) 0)
              (card-rank c))))

(: show-player-hand (-> BJ-Playing BJ-Playing))
(define (show-player-hand st)
  (message (format "Your Hand: ~a"
                   (map card->display (reverse (bj-playing-player-hand st)))))
  st)

(: show-dealer-hand (-> BJ-Playing BJ-Playing))
(define (show-dealer-hand st)
  (message (format "Dealer's Hand: ~a"
                   (map card->display (reverse (bj-playing-dealer-hand st)))))
  st)

(: wallet-inc (-> Positive-Integer (-> Player Player)))
(define ((wallet-inc n) pl)
  (struct-copy player pl
               [wallet (+ (player-wallet pl) n)]))

(: result-bj-win (-> BJ-Playing BJ-Playing))
(define (result-bj-win st)
  (let* ([bet (bj-playing-bet st)]
         [bonus (quotient bet 2)]
         [payout (+ bet bet bonus)])
    (message (format "Blackjack! [+~a]" payout))
    (struct-copy bj-playing st
                 [player #:parent bj-state ((wallet-inc payout) (bj-state-player st))])))

(: result-win (-> BJ-Playing BJ-Playing))
(define (result-win st)
  (let* ([bet (bj-playing-bet st)]
         [payout (+ bet bet)])
    (message (format "Player wins. [+~a]" payout))
    (struct-copy bj-playing st
                 [player #:parent bj-state ((wallet-inc payout) (bj-state-player st))])))

(: result-push (-> BJ-Playing BJ-Playing))
(define (result-push st)
  (let* ([bet (bj-playing-bet st)]
         [payout bet])
    (message (format "Push. [+~a]" payout))
    (struct-copy bj-playing st
                 [player #:parent bj-state ((wallet-inc payout) (bj-state-player st))])))

(: result-lose (-> BJ-Playing BJ-Playing))
(define (result-lose st)
  (message "Dealer wins.")
  st)

(: player-score (-> BJ-Playing Score))
(define (player-score st)
  (cards->score (bj-playing-player-hand st)))

(: dealer-score (-> BJ-Playing Score))
(define (dealer-score st)
  (cards->score (bj-playing-dealer-hand st)))

(: dealer-hit? (-> BJ-Playing Boolean))
(define (dealer-hit? st)
  (let ([s (dealer-score st)])
    (cond
      [(natural-blackjack? s) #f]
      [(bust? s) #f]
      [else (< s 17)])))

(: bj-playing-graph (-> String
                        (Values (-> (Node BJ-Type BJ-State)
                                    (Code (-> BJ-Playing BJ-State))
                                    (OpenGraph BJ-Type BJ-Playing))
                                (Node BJ-Type BJ-Playing))))
(define (bj-playing-graph g)
  (define bj-node ((inst node BJ-Type BJ-Playing) g))
  (define bj-edge (inst edge BJ-Type BJ-Playing))
  (define bj-bridge (inst bridge BJ-Type BJ-Playing))
  (define bj-graph (inst open-graph BJ-Type BJ-Playing))
  (define bj-any-node (any-node bj-state?))
  (define bj-show (inst show BJ-Playing))

  (define dealing-to-player (bj-node "Dealing a Card to Player" #:type 'dealer
                                     #:trans (code (compose show-player-hand deal-to-player))))
  (define dealing-to-dealer (bj-node "Dealing a Card to Dealer" #:type 'dealer
                                     #:trans (code (compose show-dealer-hand deal-to-dealer))))
  (define empty-shoe (bj-node "Dealer Misdeal" #:desc "(Empty Shoe)" #:type 'dealer))
  (define player-decision (bj-node "Hit or Stand (Player)" #:type 'player))
  (define dealer-decision (bj-node "Hit or Stand (Dealer)"  #:type 'dealer))
  (define judgement (bj-node "Judgement" #:type 'dealer))
  (define player-win (bj-node "Player Win" #:type 'dealer #:trans (code result-win)))
  (define blackjack-win (bj-node "Blackjack Win" #:type 'dealer #:trans (code result-bj-win)))
  (define push (bj-node "Push" #:type 'dealer #:trans (code result-push)))
  (define dealer-win (bj-node "Dealer Win" #:type 'dealer #:trans (code result-lose)))

  (values
   (lambda (return-node return)
     (bj-graph g
               #:parent-name (node-graph-name return-node)
               #:edges
               (list
                (bj-edge "Dealing Fail"
                         #:priority +2
                         #:when (code (compose empty? bj-state-shoe))
                         #:dom dealing-to-player #:cod empty-shoe #:mode 'auto)
                (bj-edge "Dealing Fail"
                         #:priority +2
                         #:dom dealing-to-dealer #:cod empty-shoe #:mode 'auto
                         #:when (code (compose empty? bj-state-shoe)))
                (bj-edge "Player win!"
                         #:dom empty-shoe #:cod player-win #:mode 'auto)
                (bj-edge "Deal a Card to dealer"
                         #:priority +1
                         #:dom dealing-to-player #:cod dealing-to-dealer #:mode 'auto
                         #:when (code (compose (less-than? 2) player-hand-count)))
                (bj-edge "Deal a Card to player"
                         #:priority +1
                         #:dom dealing-to-dealer #:cod dealing-to-player #:mode 'auto
                         #:when (code (compose (less-than? 2) dealer-hand-count)))
                (bj-edge "Return to dealer"
                         #:dom dealing-to-dealer #:cod dealer-decision #:mode 'auto)
                (bj-edge "Return to player"
                         #:dom dealing-to-player #:cod player-decision #:mode 'auto)
                (bj-edge "Hit"
                         #:dom player-decision #:cod dealing-to-player)
                (bj-edge "Stand"
                         #:dom player-decision #:cod dealer-decision)
                (bj-edge "Bust"
                         #:dom player-decision #:cod dealer-win #:mode 'auto
                         #:trans (code (bj-show "Bust!"))
                         #:when (code (compose bust? player-score)))
                (bj-edge "Blackjack"
                         #:dom player-decision #:cod dealer-decision #:mode 'auto
                         #:trans (code (bj-show "Blackjack!"))
                         #:when (code (compose natural-blackjack? player-score)))
                (bj-edge "Hit"
                         #:dom dealer-decision #:cod dealing-to-dealer #:mode 'auto
                         #:when (code dealer-hit?))
                (bj-edge "Stand"
                         #:dom dealer-decision #:cod judgement #:mode 'auto
                         #:when (code (negate dealer-hit?)))
                (bj-edge "Bust"
                         #:dom dealer-decision #:cod player-win #:mode 'auto
                         #:when (code (compose bust? dealer-score))
                         #:trans (code (bj-show "Bust!")))
                (bj-edge "Blackjack Win!"
                         #:dom judgement #:cod blackjack-win #:mode 'auto
                         #:when (code (compose bj-win? bj-judge)))
                (bj-edge "Player win!"
                         #:dom judgement #:cod player-win #:mode 'auto
                         #:when (code (compose win? bj-judge)))
                (bj-edge "Push!"
                         #:dom judgement #:cod push #:mode 'auto
                         #:when (code (compose push? bj-judge)))
                (bj-edge "Dealer Win!"
                         #:dom judgement #:cod dealer-win #:mode 'auto
                         #:when (code (compose lose? bj-judge))))
               #:bridges
               (list
                (bj-bridge "Return"
                           #:dom blackjack-win
                           #:cod (bj-any-node return-node)
                           #:mode 'auto
                           #:trans return)
                (bj-bridge "Return"
                           #:dom player-win
                           #:cod (bj-any-node return-node)
                           #:mode 'auto
                           #:trans return)
                (bj-bridge "Return"
                           #:dom push
                           #:cod (bj-any-node return-node)
                           #:mode 'auto
                           #:trans return)
                (bj-bridge "Return"
                           #:dom dealer-win
                           #:cod (bj-any-node return-node)
                           #:mode 'auto
                           #:trans return))))
   dealing-to-player))

(: bj-wire (-> Positive-Integer (Values (Listof AnyGraph) AnyNode)))
(define (bj-wire n)
  (define-values (gen-entry entry)
    (entry-graph "Lobby"))
  (define-values (gen-bj hello idle)
    (bj-graph "Blackjack"))
  (define-values (gen-playing start)
    (bj-playing-graph "Playing"))
  (define bj-any-graph (any-graph bj-state?))
  (define bj-playing-any-graph (any-graph bj-playing?))
  (define entry-any-graph (any-graph player?))
  (define bj-any-node (any-node bj-state?))
  (define bj-playing-any-node (any-node bj-playing?))
  (define entry-node (any-node player?))

  (values (list (bj-any-graph (gen-bj entry (code bj-state-player) start (code place-bet)))
                (bj-playing-any-graph (gen-playing idle (code identity)))
                (entry-any-graph (gen-entry (list (list "Entry Blackjack"
                                                        (bj-any-node hello)
                                                        (code
                                                         (lambda ([pl : Player])
                                                           (make-bj-state pl n))))))))
          (entry-node entry)))

(: prompt-shuffle (All (A) (-> String (Listof A) (Listof A))))
(define (prompt-shuffle title xs)
  (let ([v (list->vector xs)])
    (: pop-at! (-> Natural Positive-Integer A))
    (define (pop-at! i len)
      (let ([val (vector-ref v i)])
        (for ([k (in-range i (sub1 len))])
          (vector-set! v k (vector-ref v (add1 k))))
        val))
    (let shuf ([len : Natural (vector-length v)])
      (if (zero? len)
          '()
          (let ([val (pop-at! (prompt title `(random ,len)) len)])
            (cons val (shuf (sub1 len))))))))

(module+ console
  (require typed/racket/gui typed/pict)
  (provide make-system)
  (define state-init (player 100))
  (: make-system (->* () (Positive-Integer)
                      (Values (->* () (Journal) Journal)
                              (->* () (Journal) DotWriter))))
  (define (make-system [n 4])
    (define-values (graphs node-init) (bj-wire n))
    (: writer  (->* () (Journal) DotWriter))
    (define (writer [j '()])
      (let-values ([(_node _state h) (replay graphs node-init state-init j)])
        (dot-writer graphs node-init #:history h)))
    (: show (-> Journal Void))
    (define (show j)
      (let ([bmp (make-bitmap 1 1)])
        (render-dot (writer j) bmp)
        (show-pict (scale (bitmap bmp) 0.5)
                   #:frame-style '() #:frame-x 0 #:frame-y 0)))
    (: run (->* () (Journal) Journal))
    (define (run [j '()])
      (parameterize ([current-console-commands (list (list 'action 'r "Render Graph" show)
                                                     (list 'quit 'q "Quit"))]
                     [current-console-trace-display 'hide]
                     [current-eventspace (make-eventspace)])
        (let-values ([(_node _state j-result)
                      (console-run graphs node-init state-init #:journal j)])
          j-result)))
    (values run writer)))

(module+ main
  (require racket/cmdline (submod ".." console))
  (: mode (Boxof (U 'dot 'console)))
  (define mode (box 'dot))
  (: num-of-decks (Boxof Positive-Integer))
  (define num-of-decks (box 4))
  (: any->positive (-> Any Positive-Integer))
  (define (any->positive x)
    (cond [(string->number (assert x string?))
           => (lambda (x)
                (if (and (exact? x) (positive-integer? x))
                    x
                    (error '--num-of-decks "must be exact positive integer (~a)" x)))]
          [else (error '--num-of-decks "must be exact positive integer (~a)" x)]))
  (define program-name "vs-dealer")
  (command-line
   #:program program-name
   #:once-each
   [("--num-of-decks") n "Number of decks" (set-box! num-of-decks (any->positive n))]
   #:once-any
   [("--dot") "Generate dot" (set-box! mode 'dot)]
   [("--console") "Run console" (set-box! mode 'console)]
   #:args ()
   (define-values (run writer) (make-system (unbox num-of-decks)))
   (case (unbox mode)
     [(dot) (write-dot (writer))]
     [(console) (writeln (run))])))
