#!r6rs

(import 
 (rnrs base (6))
 (scheme class)
 (only (scheme base) require planet)
 (magicthegathering permanents)
 (magicthegathering zone)
 (magicthegathering mana-cost))

(require (planet schematics/schemeunit:3:4))
(require (planet schematics/schemeunit:3/text-ui))

(define zone-tests
  (let ((card1 (new creature% (name 'card1) (mana-cost (new mana-cost% (colorless 2) (red 1))) (expansion 'tenth) (init-power 2) (init-toughness 2) (creature-type 'goblin)))
        (card2 (new creature% (name 'card2) (mana-cost (new mana-cost% (colorless 2) (green 1))) (expansion 'tenth) (init-power 2) (init-toughness 2) (creature-type 'bear)))
        (card3 (new artifact-creature% (name 'card3) (mana-cost (new mana-cost% (colorless 2) (red 1) (green 1))) (expansion 'tenth) (init-power 2) (init-toughness 2) (creature-type 'goblin)))
        (zone (new zone%)))
    
    (test-suite
     "Tests for class zone%"
     
     (check = (send zone size) 0 "Check zone size")
     
     (send zone add-card card1)
     (send zone add-card card2)
     (send zone add-card card3)
     
     (check = (send zone size) 3 "Check zone size")
     
     (check-equal? (send zone find-card card2) card2 "Test find-card")
     (check-equal? (send zone find-card card3) card3 "Test find-card")
     
     (send zone for-each (lambda (c) (send c tap)))
     (check-equal? (get-field tapped? card2) #t)
     
     (check-equal? (send zone remove-card card3) card3 "Test remove-card")
     (check-false (send zone find-card card3) "Test find-card"))))

(run-tests zone-tests)
