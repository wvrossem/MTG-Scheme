#|
File : decks
Author : Wouter Van Rossem 
|#

#!r6rs

(library 
 (decks)
 (export find-deck decks)
 (import (rnrs base (6))
         (rnrs control (6))
         (scheme class)
         (only (scheme base) void?)
         (prefix (scheme base) s:)
         (magicthegathering permanents)
         (magicthegathering non-permanents)
         (magicthegathering abilities)
         (magicthegathering mana-cost))

 ; ( symbol -> deck% )
 (define (find-deck deckname)
   (cond
     ((equal? deckname "Molimo's Might") (new DECK-Molimos_Might))
     (else (error "find-deck" deckname))))
 
 ; This is a scheme/base list, because user-input-choices requires this
 (define decks
   (s:list "Molimo's Might"))
 
 (define DECK-Molimos_Might
   (class object%
     (super-new)
     
     (field (decklist 
             (list
              ; 16 forest
              (new land% (name "Forest") (expansion "tenth") (basic? #t) (land-type 'forest))
              (new land% (name "Forest") (expansion "tenth") (basic? #t) (land-type 'forest))
              (new land% (name "Forest") (expansion "tenth") (basic? #t) (land-type 'forest))
              (new land% (name "Forest") (expansion "tenth") (basic? #t) (land-type 'forest))
              (new land% (name "Forest") (expansion "tenth") (basic? #t) (land-type 'forest))
              (new land% (name "Forest") (expansion "tenth") (basic? #t) (land-type 'forest))
              (new land% (name "Forest") (expansion "tenth") (basic? #t) (land-type 'forest))
              (new land% (name "Forest") (expansion "tenth") (basic? #t) (land-type 'forest))
              (new land% (name "Forest") (expansion "tenth") (basic? #t) (land-type 'forest))
              (new land% (name "Forest") (expansion "tenth") (basic? #t) (land-type 'forest))
              (new land% (name "Forest") (expansion "tenth") (basic? #t) (land-type 'forest))
              (new land% (name "Forest") (expansion "tenth") (basic? #t) (land-type 'forest))
              (new land% (name "Forest") (expansion "tenth") (basic? #t) (land-type 'forest))
              (new land% (name "Forest") (expansion "tenth") (basic? #t) (land-type 'forest))
              (new land% (name "Forest") (expansion "tenth") (basic? #t) (land-type 'forest))
              (new land% (name "Forest") (expansion "tenth") (basic? #t) (land-type 'forest))
              ; 2 canopy spider
              (new creature% (name "Canopy Spider") (expansion "tenth") (mana-cost (new mana-cost% (colorless 1) (green 1)))
                   (init-power 1) (init-toughness 3) (creature-type 'Spider)
                   (init-keyword (new keyword-ability% (keyword-name 'Reach))))
              (new creature% (name "Canopy Spider") (expansion "tenth") (mana-cost (new mana-cost% (colorless 1) (green 1)))
                   (init-power 1) (init-toughness 3) (creature-type 'Spider)
                   (init-keyword (new keyword-ability% (keyword-name 'Reach))))
              ; 2 civic wayfinder
              (new creature% (name "Civic Wayfinder") (expansion "tenth") (mana-cost (new mana-cost% (colorless 2) (green 1)))
                   (init-power 2) (init-toughness 2) (creature-type (list 'Elf 'Warrior 'Druid))
                   (init-triggered (new triggered-ability% (trigger 'CIP) 
                                        (effect (lambda (mtg p t) (send mtg select-card-from-library p 'all (lambda (c) (or (void? c) (and (is-a? c land%) (get-field basic? c))))
                                                                        (lambda (mtg p c) (send p library-to-hand c) 
                                                                          (send mtg notify 'add-card 'hand c p)
                                                                          (send mtg notify 'update-library-size p)
                                                                          (send p shuffle-library))))))))
              (new creature% (name "Civic Wayfinder") (expansion "tenth") (mana-cost (new mana-cost% (colorless 2) (green 1)))
                   (init-power 2) (init-toughness 2) (creature-type (list 'Elf 'Warrior 'Druid))
                   (init-triggered (new triggered-ability% (trigger 'CIP) 
                                        (effect (lambda (mtg p t) (send mtg select-card-from-library p 'all (lambda (c) (or (void? c) (and (is-a? c land%) (get-field basic? c))))
                                                                        (lambda (mtg p c) (send p library-to-hand c) 
                                                                          (send mtg notify 'add-card 'hand c p)
                                                                          (send mtg notify 'update-library-size p)
                                                                          (send p shuffle-library))))))))
              ; 2 rootwalla
              (new creature% (name "Rootwalla") (expansion "tenth") (mana-cost (new mana-cost% (colorless 2) (green 1)))
                   (init-power 2) (init-toughness 2) (creature-type 'Lizard)
                   (init-activated (new activated-ability% (cost (new mana-cost% (green 1) (colorless 1))) 
                                        (effect (lambda (mtg p c t) (send c inc-power 2) (send c inc-toughness 2))))))
              (new creature% (name "Rootwalla") (expansion "tenth") (mana-cost (new mana-cost% (colorless 2) (green 1)))
                   (init-power 2) (init-toughness 2) (creature-type 'Lizard)
                   (init-activated (new activated-ability% (cost (new mana-cost% (green 1) (colorless 1))) 
                                        (effect (lambda (mtg p c t) (send c inc-power 2) (send c inc-toughness 2))))))
              ; 1 stampeding wildebessts
              (new creature% (name "Stampeding Wildebeests") (expansion "tenth") (mana-cost (new mana-cost% (colorless 2) (green 2)))
                   (init-power 5) (init-toughness 4) (creature-type 'Beast)
                   (init-triggered (new triggered-ability% (trigger 'beginning-of-upkeep) (effect (lambda (p) ()))))
                   (init-keyword (new keyword-ability% (keyword-name 'Trample))))
              ; 2 spined wurm
              (new creature% (name "Spined Wurm") (expansion "tenth") (mana-cost (new mana-cost% (colorless 4) (green 1)))
                   (init-power 5) (init-toughness 4) (creature-type 'Wurm))
              (new creature% (name "Spined Wurm") (expansion "tenth") (mana-cost (new mana-cost% (colorless 4) (green 1)))
                   (init-power 5) (init-toughness 4) (creature-type 'Wurm))
              ; 1 kavu climber
              (new creature% (name "Kavu Climber") (expansion "tenth") (mana-cost (new mana-cost% (colorless 3) (green 2)))
                   (init-power 3) (init-toughness 3) (creature-type 'Kavu)
                   (init-triggered (new triggered-ability% (trigger 'CIP) 
                                        (effect (lambda (mtg p c) (send mtg draw-card p))))))
              ; 1 craw wurm
              (new creature% (name "Craw Wurm") (expansion "tenth") (mana-cost (new mana-cost% (colorless 4) (green 2)))
                   (init-power 6) (init-toughness 4) (creature-type 'Wurm))
              ; 1 enormous balloth
              (new creature% (name "Enormous Baloth") (expansion "tenth") (mana-cost (new mana-cost% (colorless 6) (green 1)))
                   (init-power 7) (init-toughness 7) (creature-type 'Beast))
              ; 1 molimo
              (new creature% (name "Molimo, Maro-Sorcerer") (expansion "tenth") (mana-cost (new mana-cost% (colorless 4) (green 3))) (legendary? #t)
                   (init-power 0) (init-toughness 0) (creature-type 'Elemental)
                   (init-keyword (new keyword-ability% (keyword-name 'Trample)))
                   (init-static (new static-ability% (effect (lambda (mtg p c)
                                                               (send c set-power (send (get-field lands p) size)) (send c set-toughness (send (get-field lands p) size)))))))
              ; 1 mantis engine
              (new artifact-creature% (name "Mantis Engine") (expansion "tenth") (mana-cost (new mana-cost% (colorless 5)))
                   (init-power 3) (init-toughness 3) (creature-type 'Insect)
                   (init-activated 
                    (list 
                     (new activated-ability% (cost (new mana-cost% (colorless 2))) (effect (lambda (mtg p c t) (send c add-ability (new keyword-ability% (keyword-name 'Flying))))))
                     (new activated-ability% (cost (new mana-cost% (colorless 2))) (effect (lambda (mtg p c t) (send c add-ability (new keyword-ability% (keyword-name 'First-Strike)))))))))
              ; 2 commune with nature
              (new sorcery% (name "Commune with Nature") (expansion "tenth") (mana-cost (new mana-cost% (green 1)))
                   (effect (lambda (mtg p c t) 
                             (send mtg select-card-from-library p 5 (lambda (c) (or (void? c) (is-a? c creature%)))
                                   (lambda (mtg p c) (if (void? c)
                                                         (send (get-field library p) top-to-bottom 5)
                                                         (begin (send p library-to-hand c)
                                                                (send mtg notify 'add-card 'hand c p)
                                                                (send (get-field library p) top-to-bottom 4))))))))
              (new sorcery% (name "Commune with Nature") (expansion "tenth") (mana-cost (new mana-cost% (green 1)))
                   (effect (lambda (mtg p c t) 
                             (send mtg select-card-from-library p 5 (lambda (c) (or (void? c) (is-a? c creature%)))
                                   (lambda (mtg p c) (if (void? c)
                                                         (send (get-field library p) top-to-bottom 5)
                                                         (begin (send p library-to-hand c)
                                                                (send mtg notify 'add-card 'hand c p)
                                                                (send (get-field library p) top-to-bottom 4))))))))
              ; 1 giant growth
              (new instant% (name "Giant Growth") (expansion "tenth") (mana-cost (new mana-cost% (green 1)))
                   (targets 1) (target? (lambda (t p) (is-a? t creature%)))
                   (effect (lambda (mtg p s c) (send c inc-power 3) (send c inc-toughness 3))))
              ; 2 rampant growth
              (new sorcery% (name "Rampant Growth") (expansion "tenth") (mana-cost (new mana-cost% (colorless 1) (green 1)))
                   (effect (lambda (mtg p s t) (send mtg select-card-from-library p 'all (lambda (c) (or (void? c) (and (is-a? c land%) (get-field basic? c))))
                                                                        (lambda (mtg p c) (send p library-to-play c)
                                                                          (send mtg notify 'update-library-size p)
                                                                          (send mtg notify 'add-card 'lands c p)
                                                                          (send mtg notify 'tap-permanent c p)
                                                                          (send c tap)
                                                                          (send p shuffle-library))))))
              (new sorcery% (name "Rampant Growth") (expansion "tenth") (mana-cost (new mana-cost% (colorless 1) (green 1)))
                   (effect (lambda (mtg p s t) (send mtg select-card-from-library p 'all (lambda (c) (or (void? c) (and (is-a? c land%) (get-field basic? c))))
                                                                        (lambda (mtg p c) (send p library-to-play c)
                                                                          (send mtg notify 'update-library-size p)
                                                                          (send mtg notify 'add-card 'lands c p)
                                                                          (send mtg notify 'tap-permanent c p)
                                                                          (send c tap)
                                                                          (send p shuffle-library))))))
              ; 2 blanchwood armor
              (new aura% (name "Blanchwood Armor") (expansion "tenth") (mana-cost (new mana-cost% (colorless 2) (green 1)))
                   (targets 1) (target? (lambda (c p) (is-a? c creature%)))
                   (init-static (new static-ability% 
                                     (effect (lambda (mtg p c)
                                               (let ((nr-of-forests 0))
                                                 (send (get-field lands p) for-each (lambda (l) (if (eq? (get-field land-type l) 'forest)
                                                                                                    (set! nr-of-forests (+ nr-of-forests 1)))))
                                                 (send (get-field target c) inc-power nr-of-forests)
                                                 (send (get-field target c) inc-toughness nr-of-forests)))))))
              (new aura% (name "Blanchwood Armor") (expansion "tenth") (mana-cost (new mana-cost% (colorless 2) (green 1)))
                   (targets 1) (target? (lambda (c p) (is-a? c creature%)))
                   (init-static (new static-ability% 
                                     (effect (lambda (mtg p c)
                                               (let ((nr-of-forests 0))
                                                 (send (get-field lands p) for-each (lambda (l) (if (eq? (get-field land-type l) 'forest)
                                                                                                    (set! nr-of-forests (+ nr-of-forests 1)))))
                                                 (send (get-field target c) inc-power nr-of-forests)
                                                 (send (get-field target c) inc-toughness nr-of-forests)))))))
              ; 1 overrun
              (new sorcery% (name "Overrun") (expansion "tenth") (mana-cost (new mana-cost% (colorless 2) (green 3)))
                   (effect (lambda (mtg p s t) (send (get-field nonlands p) for-each (lambda (c)
                                                                                     (when (is-a? c creature%)
                                                                                       (send c inc-power 3)
                                                                                       (send c inc-toughness 3)
                                                                                       (send c add-ability (new keyword-ability% (keyword-name 'Trample)))))))))
              ; 1 hurricane
              (new sorcery% (name "Hurricane") (expansion "tenth") (mana-cost (new mana-cost% (x? #t) (green 1)))
                   (effect (lambda (mtg p s t) (let ((x (get-field x? (get-field mana-cost s))))
                                                 (send mtg for-each-player 
                                                   (lambda (p) (send p dec-life x)
                                                     (send mtg notify 'update-life p)
                                                     (send (get-field nonlands p) for-each 
                                                           (lambda (c) (if (and (is-a? c creature%) (send c has-keyword-ability? 'flying))
                                                                           (send c add-damage x))))))))))
              ; 1 wurm's tooth
              (new artifact% (name "Wurm's Tooth") (expansion "tenth") (mana-cost (new mana-cost% (colorless 2)))
                   (init-triggered (new triggered-ability% (trigger 'green-spell) (effect (lambda (mtg p t) (send p inc-life 1) (send mtg notify 'update-life p)))))
                   (initialize-proc (lambda (m) (send m add-observer this 'green-spell-played)))
                   (update-proc (lambda (u args) (send (s:car args) put-ability-on-stack (get-field triggered-abilities this) (s:cadr args))))))))))) 





