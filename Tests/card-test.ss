#!r6rs

(import 
 (rnrs base (6))
 (scheme class)
 (only (scheme base) require planet)
 (magicthegathering card)
 (magicthegathering mana-cost))

(require (planet schematics/schemeunit:3:4))
(require (planet schematics/schemeunit:3/text-ui))

(define card-tests
  (let ((card1 (new card% (name 'card1) (mana-cost (new mana-cost% (colorless 2) (green 1))) (expansion 'tenth)))
        (card2 (new card% (name 'card2) (mana-cost (new mana-cost% (colorless 2) (red 1) (green 1))) (expansion 'tenth)))
        (card3 (new card% (name 'card3) (mana-cost (new mana-cost% (colorless 2))) (expansion 'tenth))))
    
    (test-suite
     "Tests for class card%"
     
     (check-equal? (get-field name card1) 'card1 "Check name")
     
     (check-equal? (get-field color card1) 'green "Check color")
     (check-equal? (get-field color card2) 'multi-color "Check color")
     (check-equal? (get-field color card3) 'colorless "Check color"))))

(run-tests card-tests)