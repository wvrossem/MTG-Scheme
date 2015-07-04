#|
File : card
Author : Wouter Van Rossem 
|#

#!r6rs

(library
 (card)
 (export card%)
 (import (rnrs base (6))
         (rnrs io simple)
         (scheme class)
         (magicthegathering observer-pattern)
         (magicthegathering abilities)
         (magicthegathering mana-cost))
 
 ; Class name: card
 ; Superclass: observer%
 ; Attributes: - public: name, expansion, mana-cost, tribal, supertypes, initial-abilities, 
 ;                       initialize-proc, update-proc, color, converted-mana-cost, 
 ;                       triggered-abilities, activated-abilities, keyword-abilities
 ;             - private: init-triggered, init-activated, init-keyword        
 ; Methods:
 ;  - public: 
 ;     * add-ability: 
 ;         arguments: ability to be added
 ;         output: none
 ;         description: add a new ability to the card
 ;     * has-keyword-ability?: 
 ;         arguments: keyword-name
 ;         output: bool
 ;         description: check if the card has a keyword-ability with the given name
 ;     * has-cip-ability?: 
 ;         arguments: none
 ;         output: bool
 ;         description: check if the card has a 'comes into play' triggered ability
 ;     * reset: 
 ;         arguments: none
 ;         output: none
 ;         description: reset the card, i.e. set current abilities to initial abilities
 ;     * initialize:
 ;         arguments: manager
 ;         output: none
 ;         description: execute the initialize procedure
 ;            If the card had such a procedure it will be used to let the manager know
 ;            in wich subjects the card is interested
 ;     * update:
 ;         arguments: update-name, optionally some arguments
 ;         output: none
 ;         description: execute the update procedure
 ;            If the card has such a procedure it will be called when the card gets 
 ;            a message to update from the manager
 ;  - private: none
 ; Extra info: Each card is an observer, this way cards can subscribe to updates from certain 
 ;    subjects. This is useful for e.g. triggers.
 
 (define card%
   (class observer%
     (init-field name)     
     (init-field expansion)
     (init-field (mana-cost (new mana-cost%)))
     (init-field (tribal #f))
     (init-field (supertypes '()))
     (init-field (init-triggered '()))
     (init-field (init-activated '()))
     (init-field (init-keyword '()))
     (init-field (initialize-proc (lambda (manager) 'ok)))
     (init-field (update-proc (lambda (manager) 'ok)))
     (super-new)
     (field (color (send mana-cost derive-color)))
     (field (converted-mana-cost (send mana-cost converted-mana-cost)))
     (field (triggered-abilities (if (list? init-triggered) init-triggered (list init-triggered))))
     (field (activated-abilities (if (list? init-activated) init-activated (list init-activated))))
     (field (keyword-abilities (if (list? init-keyword) init-keyword (list init-keyword))))
     
     ; ( ability% -> )
     (define/public (add-ability ability)
       (cond
         ((is-a? ability triggered-ability%) (set! triggered-abilities (cons ability triggered-abilities)))
         ((is-a? ability activated-ability%) (set! activated-abilities (cons ability activated-abilities)))
         ((is-a? ability keyword-ability%) (set! keyword-abilities (cons ability keyword-abilities)))
         (else (error "CARD -- unknown ability type" ability))))
     
     ; ( symbol -> )
     (define/public (has-keyword-ability? keyword-name)
       (let loop ((keyword-abilities keyword-abilities))
         (cond
           ((and (not (list? keyword-abilities)) (eq? (get-field keyword-name keyword-abilities) keyword-name)) #t)
           ((null? keyword-abilities) #f)
           ((eq? (get-field keyword-name (car keyword-abilities)) keyword-name) #t)
           (else (loop (cdr keyword-abilities))))))
     
     ; ( -> bool )
     (define/public (has-cip-ability?)
       (let loop ((triggered-abilities triggered-abilities))
         (cond
           ((and (not (list? triggered-abilities)) (eq? (get-field trigger triggered-abilities) 'CIP)) triggered-abilities)
           ((null? triggered-abilities) #f)
           ((eq? (get-field trigger (car triggered-abilities)) 'CIP) (car triggered-abilities))
           (else (loop (cdr triggered-abilities))))))
     
     ; ( -> )
     (define/public (reset)
       (set! triggered-abilities (if (list? init-triggered) init-triggered (list init-triggered)))
       (set! activated-abilities (if (list? init-activated) init-activated (list init-activated)))
       (set! keyword-abilities (if (list? init-keyword) init-keyword (list init-keyword))))
     
     ; ( -> )
     (define/public (initialize manager)
       (initialize-proc manager))
     
     ; ( -> )
     (define/override (update update-name . update-args)
       (if (null? update-name)
           (update-proc)
           (update-proc (car update-name)))))))