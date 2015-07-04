#!r6rs

(import 
 (rnrs base (6))
 (scheme class)
 (only (scheme base) require planet)
 (magicthegathering permanents)
 (magicthegathering hand)
 (magicthegathering mana-cost))

(require (planet schematics/schemeunit:3:4))
(require (planet schematics/schemeunit:3/text-ui))

(define hand-tests
  (let ((card1 (new creature% (name 'card1) (mana-cost (new mana-cost% (colorless 2) (red 1))) (expansion 'tenth) (init-power 2) (init-toughness 2) (creature-type 'goblin)))
        (card2 (new creature% (name 'card2) (mana-cost (new mana-cost% (colorless 2) (green 1))) (expansion 'tenth) (init-power 2) (init-toughness 2) (creature-type 'bear)))
        (card3 (new artifact-creature% (name 'card3) (mana-cost (new mana-cost% (colorless 2) (red 1) (green 1))) (expansion 'tenth) (init-power 2) (init-toughness 2) (creature-type 'goblin)))
        (hand (new hand%)))
    
    (test-suite
     "Tests for class hand%"
     
     (check = (send hand size) 0 "Check hand size")
     (check = (get-field max-size hand) 7 "Check max hand size")
     
     (send hand add-card card1)
     (send hand add-card card2)
     (send hand add-card card3)
     
     (check = (send hand size) 3 "Check hand size")
     
     (check-equal? (send hand find-card card2) card2 "Test find-card")
     (check-equal? (send hand find-card card3) card3 "Test find-card")
     (check-equal? (send hand remove-card card3) card3 "Test remove-card")
     (check-false (send hand find-card card3) "Test find-card"))))

(run-tests hand-tests)


