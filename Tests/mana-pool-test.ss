#!r6rs

(import 
 (rnrs base (6))
 (scheme class)
 (only (scheme base) require planet)
 (magicthegathering mana-pool))

(require (planet schematics/schemeunit:3:4))
(require (planet schematics/schemeunit:3/text-ui))

(define manapool-tests
  (let ((mana-pool (new mana-pool%)))
    
    (test-suite
     "Tests for class mana-pool%"
     
     (send mana-pool inc-mana 3 'white)
     (check = (get-field white mana-pool) 3 "Check white mana")
     
     (check = (send mana-pool reset) 3 "Check reset")
     (check = (get-field white mana-pool) 0 "Check white mana after reset"))))

(run-tests manapool-tests)

