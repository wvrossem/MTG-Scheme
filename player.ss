#|
File : player
Author : Wouter Van Rossem 
|#

#!r6rs

(library
 (player)
 (export player%)
 (import (rnrs base (6))
         (rnrs io simple)
         (rnrs control (6))
         (scheme class)
         (magicthegathering permanents) ; for is-a?
         (magicthegathering non-permanents) ; for is-a?
         (magicthegathering hand)
         (magicthegathering mana-pool)
         (magicthegathering cardstack)
         (magicthegathering zone))
 
 ; Class name: card
 ; Superclass: observer%
 ; Attributes: - public: name, deck, start-life, drew-from-empty-lib?
 ;                 life, mana-pool, hand, library
 ;                 mana-pool, hand, library, graveyard, rfg
 ;                 lands, nonlands, played-land?, nr-mulligans?
 ; Methods:
 ;  - public: 
 ;     * start: 
 ;         arguments: none
 ;         output: bool
 ;         description: sets up the player
 ;     * mulligan: 
 ;         arguments: none
 ;         output: none
 ;         description: shuffle all cards in library and take a new hand
 ;     * set-not-played-land: 
 ;         arguments: none
 ;         output: none
 ;         description: set played-land? #f
 ;     * reset: 
 ;         arguments: none
 ;         output: none
 ;         description: reset the player
 ;     * clean-up: 
 ;         arguments: none
 ;         output: none
 ;         description: clean-up the player
 ;     * inc/dec-life: 
 ;         arguments: amount
 ;         output: none
 ;         description: increase or decrease the lfie of the player
 ;     * reset-mana: 
 ;         arguments: none
 ;         output: none
 ;         description: reset the mana-pool, can cause mana-burn
 ;     * remove-summoning-sickness: 
 ;         arguments: none
 ;         output: none
 ;         description: remove the summoning sickness from all creatures
 ;     * apply-static-abilities: 
 ;         arguments: mtg
 ;         output: none
 ;         description: apply the static abilities of the permanents from this player
 ;     * remove-damage: 
 ;         arguments: none
 ;         output: none
 ;         description: resmove damage from all creatures of this player
 ;     * reset: 
 ;         arguments: none
 ;         output: none
 ;         description: reset the permanents of this player
 ;     * draw-card: 
 ;         arguments: none
 ;         output: none
 ;         description: remove the top of the library and put it in the player's hand
 ;     * draw-cards: 
 ;         arguments: amount
 ;         output: none
 ;         description: draw an amount of cards
 ;     * drew-from-empty-lib: 
 ;         arguments: none
 ;         output: none
 ;         description: set drew-from-empty-lib? to true
 ;     * reveal-toplibrary: 
 ;         arguments: none
 ;         output: top of library
 ;         description: reveal the top of the player's library
 ;     * shuffle-library: 
 ;         arguments: none
 ;         output: none
 ;         description: shuffle the player's library
 ;     * untap-permanents: 
 ;         arguments: none
 ;         output: none
 ;         description: untap the permanents of this player
 ;     * discard-card: 
 ;         arguments: card
 ;         output: none
 ;         description: remove the card from the players' hand and put it in the graveyard
 ;     * discard-random: 
 ;         arguments: none
 ;         output: none
 ;         description: discard a random card from the player's hand
 ;     * reveal: 
 ;         arguments: card
 ;         output: none
 ;         description: resveal the card
 ;     * reveal-random: 
 ;         arguments: none
 ;         output: card
 ;         description: reveal a random card
 ;     * play-permanent: 
 ;         arguments: card
 ;         output: none
 ;         description: play a permanent, i.e. put it in play
 ;     * play-spell: 
 ;         arguments: spell
 ;         output: none
 ;         description: play-spell, i.e. put it into the graveyard from hand
 ;     * play-card-facedown: 
 ;         arguments: card
 ;         output: none
 ;         description: play a card with face-down? true
 ;     * inplay-to-gy: 
 ;         arguments: card
 ;         output: none
 ;         description: put a card from play to the graveyard
 ;     * inplay-to-rfg: 
 ;         arguments: card
 ;         output: none
 ;         description: rput a crad from play to rfg
 ;     * library-to-hand: 
 ;         arguments: card
 ;         output: none
 ;         description: put a card from the graveyard in the player's hand
 ;     * library-to-play: 
 ;         arguments: card
 ;         output: none
 ;         description: put a card from the graveyard into play
 ;  - private:
 ;     * deck->library: 
 ;         arguments: decklist
 ;         output: none
 ;         description: add the cards in the decklist to the library
 
 (define player%
   (class object%
     (init-field name)
     (init-field deck)
     (init-field (start-life 20))
     (super-new)
     
     (field (life start-life))
     (field (mana-pool (new mana-pool%)))
     (field (hand (new hand%)))
     (field (library (new cardstack%)))
     (field (graveyard (new cardstack% (searchable? #t))))
     (field (rfg (new cardstack% (searchable? #t))))
     (field (lands (new zone%)))
     (field (nonlands (new zone%)))
     (field (played-land? #f))
     (field (nr-of-mulligans 1))
     (field (drew-from-empty-lib? #f))
     
     ; ( pair -> )
     (define/private (deck->library deck)
       (send library add-cards deck))
     
     ; ( -> )
     (define/public (start)
       (deck->library deck)
       (shuffle-library)
       (draw-cards 7))
     
     ; ( -> )
     (define/public (mulligan)
       (begin 
         (set! nr-of-mulligans (+ nr-of-mulligans 1))
         (do ((i 1 (+ i 1))
              (hand-lst '() (cons (send hand remove-random) hand-lst)) ; problem here
              (hand-size (send hand size)))
           ((= i hand-size) (send library add-cards hand-lst)))
         (shuffle-library)
         (draw-cards (- 7 nr-of-mulligans))))
     
     ; ( -> )
     (define/public (set-not-played-land)
       (when played-land?
         (set! played-land? #f)))
     
     ; ( -> )
     (define/public (reset)
       (set! life start-life)
       (set! nr-of-mulligans 0)
       (set! played-land? #f)
       (send mana-pool reset)
       (send hand reset)
       (send library reset)
       (send graveyard reset)
       (send rfg reset)
       (send lands reset)
       (send nonlands reset))
     
     ; ( -> )
     (define/public (clean-up)
       (let ((cleaned-up '()))
         (begin
           (send nonlands for-each 
                 (lambda (c) (cond
                               ((and (is-a? c creature%) (send c lethal-damage?))
                                (inplay-to-gy c)
                                (set! cleaned-up (cons c cleaned-up))))))
           cleaned-up)))
     
     ; ( number -> )
     (define/public (inc-life amount)
       (set! life (+ life amount)))
     
     ; ( number -> )
     (define/public (dec-life amount)
       (set! life (- life amount)))
     
     ; Reset mana after phases, unspent mana causes mana burn
     ; ( -> )
     (define/public (reset-mana)
       (let ((mana-burn (send mana-pool reset)))
         (if (not (= mana-burn 0))
             (dec-life mana-burn))))
     
     ; ( -> )
     (define/public (remove-summoning-sickness)
       (send nonlands for-each (lambda (c) (if (is-a? c creature%) (send c remove-summoning-sickness)))))
     
     ; ( mtg% -> )
     (define/public (apply-static-abilities mtg)
       (send lands for-each (lambda (c)
                              (unless (null? (get-field static-abilities c))
                                (for-each (lambda (sa)
                                            ((get-field effect sa) mtg this c))))))
       (send nonlands for-each (lambda (c)
                                 (unless (null? (get-field static-abilities c))
                                   (for-each (lambda (sa)
                                               ((get-field effect sa) mtg this c)) (get-field static-abilities c))))))
     
     ; ( -> )
     (define/public (remove-damage)
       (send nonlands for-each (lambda (c) (if (is-a? c creature%)
                                               (send c remove-damage)))))
     
     ; ( -> )
     (define/public (reset-cards)
       (send nonlands for-each (lambda (c) (send c reset)))
       (send lands for-each (lambda (c) (send c reset))))
     
     ; The player draws a card from his/her library
     ; ( -> )
     (define/public (draw-card)
       (let ((card (send library remove-top)))
         (begin (send hand add-card card)
                card)))        
     
     ; ( number -> )
     (define/public (draw-cards amount)
       (if (not (= amount 0))
           (begin (draw-card)
                  (draw-cards (- amount 1)))))
     
     ; ( -> )
     (define/public (drew-from-empty-lib)
       (set! drew-from-empty-lib? #t))
     
     ; Reveal the top card of the player's library
     ; ( -> card% )
     (define/public (reveal-toplibrary)
       (let ((card (send library look-top)))
         card))
     
     ; ( -> )
     (define/public (shuffle-library)
       (send library shuffle))
     
     ; Untap permanents, e.g. at untap step
     ; There should be different untap procedures, e.g. untap only permanents of a certain type
     ; ( -> )
     (define/public (untap-permanents)
       (send lands for-each (lambda (card) (send card untap)))
       (send nonlands for-each (lambda (card) (send card untap))))
     
     ; ( card% -> )
     (define/public (discard-card card)
       (let ((found-card (send hand remove-card card)))
         (send card set-face-up)
         (send graveyard add-card card)))
     
     ; ( -> card% )
     (define/public (discard-random)
       (let ((card (send hand remove-random)))
         (send card set-face-up)
         (send graveyard add-card card)))
     
     ; ( -> card% )
     (define/public (reveal-card card)
       (let ((found-card (send hand find-card card)))
         found-card))
     
     ; ( -> card% )
     (define/public (reveal-random)
       (send hand reveal-random))
     
     ; ( permanent% -> )
     (define/public (play-permanent card)
       (let ((found-card (send hand remove-card card)))
         (send card set-face-up)
         (if (is-a? found-card land%)
             (begin (send lands add-card found-card) (set! played-land? #t))
             (send nonlands add-card found-card))))
          
     ; ( card% -> )
     (define/public (play-spell card)
      (let ((found-card (send hand remove-card card)))
        (send graveyard add-card found-card)))
     
     ; ( card% -> )
     (define/public (play-card-facedown card)
       (let ((found-card (send hand remove-card card)))
         (if (is-a? found-card land%)
             (send lands add-card found-card)
             (send nonlands add-card found-card))))
     
     ; ( card% -> )
     (define/public (inplay-to-gy card)
       (let ((found-card (if (is-a? card land%)
                             (send lands remove-card card) 
                             (send nonlands remove-card card))))
         (send graveyard add-card found-card)))
     
     ; ( card% -> )
     (define/public (inplay-to-rfg card)
       (let ((found-card (if (is-a? card land%)
                             (send lands remove-card card) 
                             (send nonlands remove-card card))))
         (send rfg add-card found-card)))
     
     ; ( card% -> )
     (define/public (library-to-hand c)
       (let ((library-card (send library remove c)))
         (if library-card
             (send hand add-card library-card))))
     
     ; ( card% -> )
     (define/public (library-to-play c)
       (let ((library-card (send library remove c)))
         (if library-card
             (if (is-a? c land%)
                 (send lands add-card library-card)
                 (send nonlands add-card library-card))))))))

