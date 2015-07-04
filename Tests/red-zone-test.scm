#!r6rs

(import 
 (rnrs base (6))
 (scheme class)
 (only (scheme base) require planet)
 (magicthegathering permanents)
 (magicthegathering red-zone)
 (magicthegathering mana-cost))

(require (planet schematics/schemeunit:3:4))
(require (planet schematics/schemeunit:3/text-ui))

(define redzone-tests
  (let ((goblin (new creature% (name 'goblin) (mana-cost (new mana-cost% (red 1))) (expansion 'tenth) (init-power 1) (init-toughness 1) (creature-type 'goblin)))
        (bear (new creature% (name 'bear) (mana-cost (new mana-cost% (colorless 1) (green 1))) (expansion 'tenth) (init-power 2) (init-toughness 2) (creature-type 'bear)))
        (red-zone (new red-zone%)))
    
    (test-suite
     "Tests for class red-zone%"

     (send red-zone add-attacking goblin)
     (send red-zone add-blocking goblin bear)
     (send red-zone assign-damage-to-blocker goblin bear 1)
     (send red-zone assign-damage-to-attacker bear goblin 2)
     
     (check = (get-field damage bear) 1)
     (check = (get-field damage goblin) 2)
     
     (send goblin reset)
     (check = (get-field damage goblin) 0)
     
     (send bear reset)
     (check = (get-field damage bear) 0))))

(run-tests redzone-tests)
