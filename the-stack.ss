#|
File : the stack
Author : Wouter Van Rossem 
|#

#!r6rs

(library 
 (the-stack)
 (export stack-spell/abi% the-stack%)
 (import (rnrs base (6))
         (rnrs io simple)
         (scheme class)
         (prefix (scheme base) s:)
         (prefix (magicthegathering datastructures stack linked) stack:)
         (magicthegathering abilities)
         (magicthegathering permanents)
         (magicthegathering non-permanents))
 
 ; Class name: stack-spell/abi
 ; Superclass: object%
 ; Description: This is a decorator class for cards being announced or on the stack
 ; Attributes: - public: spell/abi, owner, state-played, card, targets, mc
 ;             - private: mana-payed, nr-of-targets     
 ; Methods:
 ;  - public: 
 ;     * check-target: 
 ;         arguments: target, owner
 ;         output : bool
 ;         description: check if the target is good for the spell/abi
 ;     * select-target: 
 ;         arguments: target
 ;         output : none
 ;         description: select a target for the spell/abi
 ;     * targets-done,: 
 ;         arguments: none
 ;         output : bool
 ;         description: are all the targets chosen?
 ;     * pay-mana: 
 ;         arguments: none
 ;         output : none
 ;         description: pay-mana of a color
 ;     * set-x: 
 ;         arguments: x
 ;         output : none
 ;         description: set the x of the mana-cost
 ;     * mana-payed?: 
 ;         arguments: none
 ;         output : bool
 ;         description: has all the mana been payed?
 ;     * determine-mana-cost: 
 ;         arguments: none
 ;         output : none
 ;         description: determine the total cost of this spell 
 ;           (not yet implemented - not needed at the moment)
 ;     * revert-mana-cost: 
 ;         arguments: none
 ;         output : none
 ;         description: when the announced spell is canceled 
 ;            return all the spent mana to the owner's mana-pool
 ;     * reset-mana-cost: 
 ;         arguments: none
 ;         output : none
 ;         description: after we payed all the mana, we reset the mana
 ;            for later use
 ;  - private: none
 
 (define stack-spell/abi%
   (class object%
     (init-field spell/abi)
     (init-field owner)
     (init-field state-played)
     (init-field card)
     (super-new)
     (field (mc (if (is-a? spell/abi activated-ability%) (get-field cost spell/abi) (get-field mana-cost spell/abi))))
     (define mana-payed '())
     (field (targets '()))
     (define nr-of-targets 0)
     
     ; ( object% player% -> bool )
     (define/public (check-target target owner)
       ((get-field target? spell/abi) target owner))
     
     ; ( object% -> )
     (define/public (select-target target)
       (set! targets (cons target targets))
       (set! nr-of-targets (+ nr-of-targets 1)))
     
     ; ( -> bool )
     (define/public (targets-done?)
       (= nr-of-targets (get-field targets spell/abi)))
     
     ; ( symbol -> )
     (define/public (pay-mana color)
       (begin 
         (case color
           ((white) (if (= (get-field white mc) 0) (send mc pay-mana 'colorless 1) (send mc pay-mana 'white 1)))
           ((blue) (if (= (get-field blue mc) 0) (send mc pay-mana 'colorless 1) (send mc pay-mana 'blue 1)))
           ((black) (if (= (get-field black mc) 0) (send mc pay-mana 'colorless 1) (send mc pay-mana 'black 1)))
           ((red) (if (= (get-field red mc) 0) (send mc pay-mana 'colorless 1) (send mc pay-mana 'red 1)))
           ((green) (if (= (get-field green mc) 0) (send mc pay-mana 'colorless 1) (send mc pay-mana 'green 1)))
           ((colorless) (send mc pay-mana 'colorless 1)))
         (set! mana-payed (cons color mana-payed))))
     
     ; ( number -> )
     (define/public (set-x x)
       (send mc set-x x))
     
     ; ( -> bool )
     (define/public (mana-payed?)
       (send mc mana-payed?))
     
     ; ( -> )
     (define/public (determine-mana-cost)
       'ok)
     
     ; ( -> )
     (define/public (revert-mana-payed)
       (for-each (lambda (c) (send (get-field mana-pool owner) inc-mana 1 c)) mana-payed))
     
     ; ( -> )
     (define/public (reset-mana-cost)
       (send mc reset))))
 
 ; Class name: cardstack
 ; Superclass: object%
 ; Attributes: - public: none
 ;             - private: stack      
 ; Methods:
 ;  - public: 
 ;     * empty?: 
 ;         arguments: none
 ;         output : bool
 ;         description: check if the-stack is empty
 ;     * announce-spell/abi: 
 ;         arguments: spell/abi owner state-played, card wfor abilities
 ;         output : none
 ;         description: announce a spell/abi
 ;     * put-ability-on-stack: 
 ;         arguments: ability owner state card
 ;         output : none
 ;         description: put an ability on the stack
 ;     * resolve-top: 
 ;         arguments: none
 ;         output: stack-spell%
 ;         description: remolve the top card of the-stack
 ;     * look-top: 
 ;         arguments: none
 ;         output: stack-spell%
 ;         description: look at the top card of the-stack
 ;     * remove-announced: 
 ;         arguments: none
 ;         output: none
 ;         description: cancel the announced-spell/abi
 ;     * lock-announced: 
 ;         arguments: none
 ;         output: none
 ;         description: put the announced-spell/abi on the stack
 ;  - private: none
 
 (define the-stack%
   (class object%
     (super-new)
     (define stack (stack:new))
     (field (announced-spell/abi 0))
          
     ; ( -> bool )
     (define/public (empty?)
       (stack:empty? stack))
     
     ; ( card%/ability% player% -> )
     (define/public (announce-spell/abi spell/abi owner state-played . card)
       (set! announced-spell/abi (new stack-spell/abi% (spell/abi spell/abi) (owner owner) (state-played state-played) (card (if (null? card) '() (s:car card))))))
     
     ; ( card%/ability% player% -> )
     (define/public (put-ability-on-stack ability owner state . card)
       (stack:push! (new stack-spell/abi% (spell/abi ability) (owner owner) (state-played state) (card (if (null? card) '() (s:car card))))))
     
     ; ( -> stack-spell%)
     (define/public (resolve-top)
       (if (empty?)
           (error "THE-STACK -- remove-top from empty cardstack" stack)
           (stack:pop! stack)))
     
     ; ( -> stack-spell% )
     (define/public (look-top)
       (if (empty?)
           (error "THE-STACK-- look-top from empty cardstack" stack)
           (stack:top stack)))
     
     ; ( -> )
     (define/public (remove-announced)
       (begin
         (send announced-spell/abi revert-mana-payed)
         (send announced-spell/abi reset-mana-cost)
         (set! announced-spell/abi 0)))
 
     ; ( card% player% pair -> )
     (define/public (lock-announced)
       (stack:push! stack announced-spell/abi)
       (send announced-spell/abi reset-mana-cost)
       (set! announced-spell/abi 0)))))