#|
File : hand
Author : Wouter Van Rossem 
|#

#!r6rs

(library
 (hand)
 (export hand%)
 (import (rnrs base (6))
         (rnrs io simple)
         (scheme class)
         (only (scheme base) random)
         (prefix (magicthegathering datastructures positional-list adt) pl:)
         (magicthegathering card))
 
 ; Class name: cardstack
 ; Superclass: object%
 ; Attributes: - public: max-size
 ;             - private: hand = positional-list       
 ; Methods:
 ;  - public: 
 ;     * size: 
 ;         arguments: none
 ;         output : number = the size of the list
 ;         description: return the size of the hand
 ;     * add-card: 
 ;         arguments: card%
 ;         output: none
 ;         description: add a card to the hand
 ;     * find-card: 
 ;         arguments: card%
 ;         output: card% or #f
 ;         description: find a card in the hand
 ;     * remove-card:
 ;         arguments: card%
 ;         output: card% or #f
 ;         description: find a card in the hand, if found remove it from the list and return it
 ;     * remove-random:
 ;         arguments: none
 ;         output: card%
 ;         description: remove a random card from the hand
 ;     * reveal-random:
 ;         arguments: none
 ;         output: card%
 ;         description: reveal a random card from the hand
 ;     * for-each:
 ;         arguments: lambda
 ;         output: none
 ;         description: apply a function to each card of the hand
 ;     * reset:
 ;         arguments: none
 ;         output: none
 ;         description: reset the hand, i.e. make a new positional list
 ;  - private:
 ;     * find-card-pos
 ;         arguments: card%
 ;         output: the position of the card in the list
 
 (define hand%
   (class object%
     (init-field (max-size 7))
     (super-new)     
     (define hand (pl:new eq?))
     
     ; ( -> number )
     (define/public (size)
       (pl:length hand))
     
     ; ( card% -> )
     (define/public (add-card card)
       (if (is-a? card card%)
           (pl:add-before! hand card)
           (error "HAND -- trying to add a non-card to a hand" card)))
     
     ; Returns position
     ; ( card% -> number )
     (define/private (find-card-pos card)
       (pl:find hand card))
     
     ; ( card% -> card% )
     (define/public (find-card card)
       (let ((pos (find-card-pos card)))
         (if pos
             (pl:peek hand pos)
             #f)))
     
     ; ( card% -> card% )
     (define/public (remove-card card)
       (let* ((pos (find-card-pos card))
              (found-card (if pos (pl:peek hand pos) #f)))
         (if pos 
             (begin (pl:delete! hand pos)
                    found-card)
             #f)))
     
     ; ( -> card% )
     (define/public (remove-random)
       (if (not (= (size) 0))
           (let* ((random-pos (random (size)))
                  (card (pl:peek-pos hand random-pos)))
             (pl:delete! hand (pl:find hand card))
             card)))
     
     ; ( -> card% )
     (define/public (reveal-random)
       (if (not (= (size) 0))
           (let ((random-pos (random (size))))
             (pl:peek-pos hand random-pos))))
     
     ; ( ( card% -> ) -> )
     (define/public (for-each func)
       (pl:for-each hand func))
     
     ; ( -> )
     (define/public (reset)
       (set! hand (pl:new eq?))))))
 