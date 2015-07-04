#!r6rs

(import 
 (rnrs base (6))
 (scheme class)
 (only (scheme base) require planet)
 (magicthegathering permanents)
 (magicthegathering card)
 (magicthegathering mana-cost))

(require (planet schematics/schemeunit:3:4))
(require (planet schematics/schemeunit:3/text-ui))

(define permanent-tests
  (let ((card1 (new creature% (name 'card1) (mana-cost (new mana-cost% (colorless 2) (red 1))) (expansion 'tenth) (init-power 2) (init-toughness 2) (creature-type 'goblin)))
        (card2 (new artifact-creature% (name 'card2) (mana-cost (new mana-cost% (colorless 4))) (expansion 'tenth) (init-power 3) (init-toughness 3) (creature-type 'goblin))))
    
    (test-suite
     "Tests for class permanent-pool%"
     
     (check-equal? (get-field name card1) 'card1 "Check name")
     (check-equal? (get-field color card1) 'red "Check color")
     
     (check-true (is-a? card1 creature%))
     (check-true (is-a? card2 creature%))
     (check-true (is-a? card1 card%))
     (check-false (is-a? card1 artifact%))
     
     (check = (send card1 power) 2 "Check power")
     (check-equal? (get-field creature-type card2) 'goblin "Check creature-type"))))

(run-tests permanent-tests)
