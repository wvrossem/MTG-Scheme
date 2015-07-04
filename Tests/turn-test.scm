#!r6rs

(import 
 (rnrs base (6))
 (scheme class)
 (only (scheme base) require planet)
 (magicthegathering turn))

(require (planet schematics/schemeunit:3:4))
(require (planet schematics/schemeunit:3/text-ui))

(define turn-tests
  (let ((turn (new turn%)))
    
    (test-suite
     "Tests for class turn%"

     (check-equal? (get-field name (send turn current-phase)) 'Beginning-phase)
     (check-equal? (get-field name (send turn current-step)) 'Untap-step)
     
     (send turn next)
     
     (check-equal? (get-field name (send turn current-phase)) 'Beginning-phase)
     (check-equal? (get-field name (send turn current-step)) 'Upkeep-step)

     (send turn next)
     (send turn next)
     
     (check-equal? (get-field name (send turn current-phase)) 'Main-phase1)
     (check-equal? (get-field name (send turn current-step)) 'Main-phase1))))
     
(run-tests turn-tests)




