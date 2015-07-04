#!r6rs

(import 
 (rnrs base (6))
 (scheme class)
 (only (scheme base) require planet)
 (magicthegathering non-permanents)
 (magicthegathering card)
 (magicthegathering mana-cost))

(require (planet schematics/schemeunit:3:4))
(require (planet schematics/schemeunit:3/text-ui))

(define nonpermanent-tests
  (let ((card (new sorcery% (name 'incinerate) (mana-cost (new mana-cost% (red 1) (colorless 1))) (expansion 'tenth) (effect (lambda () 'ok)))))
    
    (test-suite
     "Tests for class nonpermanent-pool%"
     
     (check-equal? (get-field name card) 'incinerate "Check name")
     (check-equal? (get-field color card) 'red "Check color")
     
     (check-true (is-a? card sorcery%))
     (check-false (is-a? card instant%))
     (check-true (is-a? card card%)))))

(run-tests nonpermanent-tests)

