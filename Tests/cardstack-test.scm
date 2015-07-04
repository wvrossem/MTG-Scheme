#!r6rs

(import 
 (rnrs base (6))
 (scheme class)
 (only (scheme base) require planet)
 (magicthegathering permanents)
 (magicthegathering cardstack)
 (magicthegathering mana-cost))

(require (planet schematics/schemeunit:3:4))
(require (planet schematics/schemeunit:3/text-ui))

(define cardstack-tests
  (let ((card1 (new creature% (name 'card1) (mana-cost (new mana-cost% (colorless 2) (red 1))) (expansion 'tenth) (init-power 2) (init-toughness 2) (creature-type 'goblin)))
        (card2 (new creature% (name 'card2) (mana-cost (new mana-cost% (colorless 2) (green 1))) (expansion 'tenth) (init-power 2) (init-toughness 2) (creature-type 'bear)))
        (card3 (new artifact-creature% (name 'card3) (mana-cost (new mana-cost% (colorless 2) (red 1) (green 1))) (expansion 'tenth) (init-power 2) (init-toughness 2) (creature-type 'goblin)))
        (cardstack (new cardstack%)))
    
    (test-suite
     "Tests for class cardstack%"
     
     (send cardstack add-card card1)
     (send cardstack add-cards (list card2 card3))
     
     (check-equal? (send cardstack look-top) card3 "Test look-top")
     (check-equal? (send cardstack remove-top) card3 "Test remove-top")
     (check-equal? (send cardstack remove-top) card2 "Test remove-top")
     (check-equal? (send cardstack remove-top) card1 "Test remove-top")
     
     (check-true (send cardstack empty?)))))

(run-tests cardstack-tests)