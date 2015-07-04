#|
File : zone
Author : Wouter Van Rossem 
|#

#!r6rs

(library
 (zone)
 (export zone%)
 (import (rnrs base (6))
         (rnrs io simple)
         (scheme class)
         (prefix (magicthegathering datastructures positional-list adt) pl:))
 
 ; Class name: zone
 ; Superclass: object%
 ; Attributes: - public: none
 ;             - private: zone = positional-list       
 ; Methods:
 ;  - public: 
 ;     * size: 
 ;         arguments: none
 ;         output : the size of the list
 ;     * add-card: 
 ;         arguments: card%
 ;         output: none
 ;         description: add a card to the zone
 ;     * find-card: 
 ;         arguments: card%
 ;         output: card% or #f
 ;         description: find a card in the zone
 ;     * remove-card:
 ;         arguments: card%
 ;         output: card% or #f
 ;         description: find a card in the hand, if found remove it from the list and return it
 ;     * for-each:
 ;         arguments: function 
 ;         output: none
 ;         description: apply a function on each card in the zone
 ;     * reset:
 ;         arguments: none
 ;         output: none
 ;         description: reset the zone, i.e. make a new positional list
 ;  - private:
 ;     * find-card-pos
 ;         arguments: card%
 ;         output: the position of the card in the list
 
 (define zone%
   (class object%
     (super-new)
     (define zone (pl:new eq?))
     
     ; ( -> number )
     (define/public (size)
       (pl:length zone))
     
     ; ( card% -> )
     (define/public (add-card card)
       (pl:add-before! zone card))
     
     ; ( card% -> number )
     (define/private (find-card-pos card)
       (pl:find zone card))
     
     ; ( card% -> card% )
     (define/public (find-card card)
       (let ((pos (find-card-pos card)))
         (if pos
             (pl:peek zone pos)
             #f)))
     
     ; ( card% -> card% )
     (define/public (remove-card card)
       (let* ((pos (find-card-pos card))
              (found-card (if pos (pl:peek zone pos))))
         (if pos 
             (begin (pl:delete! zone pos)
                    found-card)
             #f)))
     
     ; ( (card% -> ) -> )
     (define/public (for-each func)
       (pl:for-each zone func))
     
     ; ( -> )
     (define/public (reset)
       (set! zone (pl:new eq?))))))
     
     ;(define/public (untap-cards)
       ;(pl:for-each zone (lambda (card) (card 'untap))))
     
     