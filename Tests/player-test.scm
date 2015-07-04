#!r6rs

(import 
 (rnrs base (6))
 (scheme class)
 (only (scheme base) require planet)
 (magicthegathering permanents)
 (magicthegathering player)
 (magicthegathering mana-cost))

(require (planet schematics/schemeunit:3:4))
(require (planet schematics/schemeunit:3/text-ui))

(define player-tests
  (let* ((card1 (new creature% (name 'card1) (mana-cost (new mana-cost% (colorless 2) (red 1))) (expansion 'tenth) (init-power 2) (init-toughness 2) (creature-type 'goblin)))
         (card2 (new creature% (name 'card2) (mana-cost (new mana-cost% (colorless 2) (green 1))) (expansion 'tenth) (init-power 2) (init-toughness 2) (creature-type 'bear)))
         (card3 (new artifact-creature% (name 'card3) (mana-cost (new mana-cost% (colorless 2) (red 1) (green 1))) (expansion 'tenth) (init-power 2) (init-toughness 2) (creature-type 'goblin)))
         (player (new player% (name "wouter") (deck (list card1 card2 card3)))))
    
    (test-suite
     "Tests for class player%"
     
     (check-equal? (get-field name player) "wouter")
     (check = (get-field life player) 20)
     
     (send player dec-life 3)
     (check = (get-field life player) 17)
     
     (send (get-field mana-pool player) inc-mana 3 'white)
     (check = (get-field white (get-field mana-pool player)) 3)
     (send player reset-mana)
     (check = (get-field life player) 14))))

(run-tests player-tests)



