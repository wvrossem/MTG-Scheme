#|
File : cardstack
Author : Wouter Van Rossem 
|#

#!r6rs

(library 
 (cardstack)
 (export cardstack%)
 (import (rnrs base (6))
         (rnrs io simple)
         (rnrs control)
         (scheme class)
         (prefix (scheme base) s:)
         (prefix (magicthegathering datastructures positional-list adt) pl:)
         (prefix (magicthegathering datastructures stack linked) stack:)
         (magicthegathering card))
 
 ; Class name: cardstack
 ; Superclass: object%
 ; Attributes: - public: searchable?
 ;             - private: stack = stack      
 ; Methods:
 ;  - public: 
 ;     * empty?: 
 ;         arguments: none
 ;         output : bool
 ;         description: check if the cardstack is empty
 ;     * size: 
 ;         arguments: none
 ;         output : number
 ;         description: return the size of the cardstack
 ;     * add-card:
 ;         arguments: card%
 ;         output: none
 ;         description: add a new card to the cardstack
 ;     * add-cards:
 ;         arguments: list of card% objects
 ;         output: none
 ;         description: add different cards (in a list) to the cardstack
 ;     * remove-top: 
 ;         arguments: none
 ;         output: card%
 ;         description: remove the top card of the cardstack
 ;     * look-top: 
 ;         arguments: none
 ;         output: card%
 ;         description: look at the top card of the cardstack
 ;     * shuffle:
 ;         arguments: none
 ;         output: none
 ;         description: shuffle the cardstack, i.e. rearrange all objects randomly
 ;     * top-to-bottom: 
 ;         arguments: none, optionnaly n
 ;         output: none
 ;         description: put the top card of the cardstack on the bottom
 ;            Repeat this n times
 ;     * find: 
 ;         arguments: card%
 ;         output: card%
 ;         description: find a card in the cardstack
 ;     * remove: 
 ;         arguments: card%
 ;         output: card%
 ;         description: delete a card from the cardstack
 ;     * for-each: 
 ;         arguments: lambda
 ;         output: none
 ;         description: apply a procedure to each element of the cardstack
 ;     * set-(un)searchable: 
 ;         arguments: none
 ;         output: none
 ;         description: set if this cardstack is searchable or not
 ;     * reset:
 ;         arguments: none
 ;         output: none
 ;         description: reset the cardstack, i.e. make a new positional list
 ;  - private:
 ;     * find-pos:
 ;         arguments: card
 ;         output: position
 ;         description: find the position of a card in the cardstack
 
 (define cardstack%
   (class object%
     (init-field (searchable? #f))
     (super-new)
     (define stack (stack:new)) 
     
     ; ( -> bool )
     (define/public (empty?)
       (stack:empty? stack))
     
     ; ( -> number )
     (define/public (size)
       (stack:size stack))
     
     ; ( card% -> )
     (define/public (add-card card)
       (if (is-a? card card%)
           (stack:push! stack card)
           (error "CARDSTACK -- trying to add a non-card to a cardstack" card)))
     
     ; ( pair -> )
     (define/public (add-cards card-list)
       (unless (null? card-list)
         (add-card (car card-list)) 
         (add-cards (cdr card-list))))
     
     ; ( -> card% )
     (define/public (remove-top)
       (if (empty?)
           (error "CARDSTACK -- remove-top from empty cardstack" stack)
           (stack:pop! stack)))
     
     ; ( -> card% )
     (define/public (look-top)
       (if (empty?)
           (error "CARDSTACK -- look-top from empty cardstack" stack)
           (stack:top stack)))
     
     ; ( -> )
     (define/public (shuffle)
       (if (empty?)
           (error "CARDSTACK -- shuffle on empty cardstack" stack)
           (stack:shuffle stack)))
     
     ; ( -> )
     (define/public (top-to-bottom . n)
       (cond
         ((null? n) (stack:top-to-bottom stack))
         ((= (s:car n) 0) )
         (else (stack:top-to-bottom stack)
               (top-to-bottom (- (s:car n) 1)))))
     
     ; ( card% -> pos )
     (define/private (find-pos card)
       (stack:find stack card))
     
     ; ( card% -> card% )
     (define/public (find card)
       (let ((pos (find-pos card)))
         (if pos
             (pl:peek (cdr stack) pos))))

     ; ( card% -> card% )
     (define/public (remove card)
       (let* ((found? (find-pos card))
              (card (pl:peek stack found?)))
         (if found?
             (stack:delete! stack found?)
             #f)))
     
      ; ( ( card% -> ) -> )     
     (define/public (for-each func)
       (stack:for-each stack func))
     
     ; ( -> )
     (define/public (set-searchable)
       (unless searchable?
         (set! searchable? #t)))
     
     ; ( -> )
     (define/public (set-unsearchable)
       (when searchable?
         (set! searchable? #f)))
     
     ; ( -> )
     (define/public (reset)
       (set! stack (stack:new))))))