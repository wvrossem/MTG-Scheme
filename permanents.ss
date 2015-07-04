#|
File : permanents
Author : Wouter Van Rossem 
|#

#!r6rs

(library
 (permanent)
 (export permanent% enchantment% aura% artifact% creature% 
         artifact-creature% land% planeswalker%)
 (import (rnrs base (6))
         (scheme class)
         (rnrs control (6))
         (magicthegathering card)
         (magicthegathering abilities))
 
 ; Class name: permanent
 ; Superclass: card%
 ; Attributes: - public: legendary?, tapped?, flipped?, face-down?, 
 ;                       init-static, static-abilities
 ;             - private: none       
 ; Methods:
 ;  - public: 
 ;     * untap: 
 ;         arguments: none
 ;         output : none
 ;         description: untap the permanent, i.e. set tapped to #f
 ;     * tap: 
 ;         arguments: none
 ;         output: none
 ;         description: tap the permanent, i.e. set tapped to #t
 ;     * flip: 
 ;         arguments: none
 ;         output: none
 ;         description: flip the card, i.e. set flipped #t
 ;     * unflip: 
 ;         arguments: none
 ;         output: none
 ;         description: unflip the permanent, i.e. set flipped to #f
 ;     * set-face-down: 
 ;         arguments: none
 ;         output: none
 ;         description: set the permanent face down , i.e. set face-down to #t
 ;     * set-face-up: 
 ;         arguments: none
 ;         output: none
 ;         description: set the permanent face up , i.e. set face-down to #f
 ;     * add-ability: 
 ;         arguments: ability to be added
 ;         output: none
 ;         description: overridden method from card. If the ability to be added is a
 ;                      static ability, permanent will handle it. Otherwise the overriden 
 ;                      method will be called, and superclass card% will add the ability.
 ;     * reset: 
 ;         arguments: none
 ;         output: none
 ;         description: overridden method fom card. Static abilities will be reset to initial ones.
 ;                      Then superclass card% will reset the rest.
 ;  - private: none
 
 (define permanent%
   (class card%
     (init-field (legendary? #f))
     ; Permanent status : face up/down, flipped/unflipped, tapped/untapped
     (init-field (tapped? #f))
     (init-field (flipped? #f))
     (init-field (face-down? #f))
     (init-field (token? #f))
     (init-field (init-static '()))
     (super-new)
     (field (static-abilities (if (list? init-static) init-static (list init-static))))
     
     ; ( -> )
     (define/public (untap)
       (set! tapped? #f))
     
     ; ( -> )
     (define/public (tap)
       (set! tapped? #t))
     
     ; ( -> )
     (define/public (flip)
       (set! flipped? #t))
     
     ; ( -> )
     (define/public (unflip)
       (set! flipped? #f))
     
     ; ( -> )
     (define/public (set-face-down)
       (set! face-down? #t))
     
     ; ( -> )
     (define/public (set-face-up)
       (set! face-down? #f))
     
     ; ( ability% -> )
     (define/override (add-ability ability)
       (if (is-a? ability static-ability%)
           (set! static-abilities (cons ability static-abilities))
           (super add-ability ability)))
     
     ; ( -> )
     (define/override (reset)
       (begin 
         (set! static-abilities (if (list? init-static) init-static (list init-static)))
         (super reset)))))
 
 ; Class name: enchantment
 ; Superclass: permanent%  
 ; Attributes: - public: enchantment-type
 ;             - private: none  
 ; Methods:
 ;  - public: none
 ;  - private: none
 
 (define enchantment%
   (class permanent%
     (init-field (enchantment-type '()))
     (super-new)))
 
 ; Class name: aura
 ; Superclass: enchantment%  
 ; Attributes: - public: targets, target?, target
 ;             - private: none  
 ; Methods:
 ;  - public:
 ;     * set-target 
 ;         arguments: target
 ;         output: none
 ;         description: set the target of this aura
 ;  - private: none
 
 (define aura%
   (class enchantment%
     (init-field (targets 1))
     (init-field (target? 'none))
     (super-new (enchantment-type 'aura))
     (field (target 0))
     
     (define/public (set-target t)
       (set! target t))))
    
 
 ; Class name: artifact
 ; Superclass: permanent%  
 ; Attributes: - public: artifact-type
 ;             - private: none  
 ; Methods:
 ;  - public: none
 ;  - private: none
 
 (define artifact%
   (class permanent%
     (init-field (artifact-type '()))
     (super-new)))
 
 ; Class name: creature
 ; Superclass: permanent%  
 ; Attributes: - public: creature-type, summoning-sickness?, damage, counters
 ;             - private: init-power, temp-power, init-toughness, temp-toughness
 ; Methods:
 ;  - public:
 ;     * power: 
 ;         arguments: none
 ;         output: power of the creature
 ;     * toughness: 
 ;         arguments: none
 ;         output: toughness of the creature
 ;     * set-power/toughness: 
 ;         arguments: new-power/toughness 
 ;         output: none
 ;         description: set the power/toughness of the creature to a new-power/toughness
 ;     * inc-power/toughness: 
 ;         arguments: amount 
 ;         output: none
 ;         description: increase the power/toughness of the creature with an amount
 ;     * dec-power/toguhenss: 
 ;         arguments: amount
 ;         output: none
 ;         description: decrease the power/toughess of the creature with an amount
 ;     * add-damage: 
 ;         arguments: amount of damage to be added
 ;         output: none
 ;         description: add an amount of damage to this creature
 ;     * add-counter:
 ;         arguments: counter to be added = lambda
 ;         output: none
 ;         description: add a counter to the list of counters
 ;     * remove-summoning-sickness: 
 ;         arguments: none
 ;         output: none
 ;         description: set summoning-scikness? to #f
 ;     * lethal-damage?:
 ;         arguments: none
 ;         output: bool
 ;         description: does the creature have lethal damage?
 ;     * remove-damage:
 ;         arguments: none
 ;         output: none
 ;         description: remove all the damage from the creature
 ;     * reset: 
 ;         arguments: none
 ;         output: none
 ;         description: overridden method fom permanent. Power and toughness will be reset to
 ;                      initial power and toughness. Damage will be reset to 0, counters will be 
 ;                      reset to the empty list. Then overridden method from superclass will be called.
 ;                      
 ;  - private: none
 
 (define creature%
   (class permanent%
     (init-field init-power)
     (init-field init-toughness)
     (init-field creature-type)
     (super-new)
     (define temp-power init-power)
     (define temp-toughness init-toughness)
     (field (summoning-sickness? #t))
     (field (damage 0))
     (field (counters '()))
     
     ; ( -> number )
     (define/public (power)
       temp-power)
     
     ; ( -> number )
     (define/public (toughness)
       temp-toughness)
     
     ; ( number -> )
     (define/public (set-power new-power)
       (set! temp-power new-power))
     
     ; ( number -> )
     (define/public (set-toughness new-toughness)
       (set! temp-toughness new-toughness))
     
     ; ( number -> )
     (define/public (inc-power amount)
       (set! temp-power (+ temp-power amount)))
     
     ; ( number -> )
     (define/public (dec-power amount)
       (set! temp-power (- temp-power amount)))
     
     ; ( number -> )
     (define/public (inc-toughness amount)
       (set! temp-toughness (+ temp-toughness amount)))
     
     ; ( number -> )
     (define/public (dec-toughness amount)
       (set! temp-toughness (- temp-toughness amount)))
     
     ; ( number -> )
     (define/public (add-damage amount)
       (set! damage (+ damage amount)))
     
     ; ( pair -> )
     (define/public (add-counter counter)
       (set! counters (cons counter counters)))
     
     ; ( -> )
     (define/public (remove-summoning-sickness)
       (when summoning-sickness?
         (set! summoning-sickness? #f)))
     
     ; ( -> bool )
     (define/public (lethal-damage?)
       (>= damage temp-toughness))
     
     ; ( -> )
     (define/public (remove-damage)
       (set! damage 0))
     
     ; ( -> )
     (define/override (reset)
       (set! temp-power init-power)
       (set! temp-toughness init-toughness)
       (set! damage 0)
       (super reset))))
 
 ; Name class: artifact-creature
 ; Superclass: permanent%  
 ; Attributes: - public: artifact-type
 ;             - private: none  
 ; Methods:
 ;  - public: none
 ;  - private: none

 (define artifact-creature%
   (class creature%
     (init-field (artifact-type '()))
     (super-new)))
 
 ; Name class: land
 ; Superclass: permanent%  
 ; Attributes: - public: basic?, land-type
 ;             - private: none  
 ; Methods:
 ;  - public: none
 ;  - private: none
 
 (define land%
   (class permanent%
     (init-field (basic? #f))
     (init-field (land-type '()))
     (super-new)))
 
 ; Name class: planeswalker
 ; Superclass: permanent%  
 ; Attributes: - public: planeswalker-type, loyalty
 ;             - private: none  
 ; Methods:
 ;  - public: none
 ;  - private: none
 
 (define planeswalker%
   (class permanent%
     (init-field planeswalker-type)
     (init loyalty)
     (super-new))))
 
