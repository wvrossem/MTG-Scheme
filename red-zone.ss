#|
File : red-zone
Author : Wouter Van Rossem 
|#

#!r6rs

(library
 (red-zone)
 (export red-zone%)
 (import (rnrs base (6))
         (scheme class)
         (rnrs io simple)
         (prefix (magicthegathering datastructures positional-list adt) pl:))
 
 ; Class name: red-zone
 ; Superclass: object%
 ; Attributes: - public: none
 ;             - private: red-zone = positional-list       
 ; Methods:
 ;  - public: 
 ;     * add-attacking: 
 ;         arguments: attacking creature to add
 ;         output : none
 ;         description: add a creature to the attackers list
 ;     * add-blocking: 
 ;         arguments: attacking creature, blocking creature
 ;         output: none
 ;         description: let the blocker block an attacker
 ;     * assign-damage-to-blocker: 
 ;         arguments: attacker, blocker, amount of damage
 ;         output: none
 ;         description: the attacker assign an amount of damage to the blocker
 ;                      blocker must block the attacker
 ;                      attacker can't assign more damage than its power
 ;     * assign-damage-to-attacker:
 ;         arguments: attacker, blocker, amount of damage
 ;         output: none
 ;         description: the blocker assigns an amount of damage to the attacker
 ;                      blocker must block attacker
 ;                      blocker can't assign more damage than its power
 ;     * for-each-attacker/blocker:
 ;         arguments: lambda
 ;         output: none
 ;         description: apply a function to each attacker/blocker
 ;     * unblocked-damage:
 ;         arguments: none
 ;         output: the total amount of unblocked damage
 ;     * reset:
 ;         arguments: none
 ;         output: none
 ;         description: reset the red-zone list, i.e. make a new one
 ;  - private:
 ;     * find-attacker-blocker
 ;         arguments: attacker
 ;         output: vector of #(attacker blocker-list) or #f
 ;         description: find the attacker-blocker vector in the red-zone list
 ;     * find-attacker
 ;         arguments: attacker
 ;         output: attacker or #f
 ;         description: find the attacker in the red-zone list
 ;     * find-blocker
 ;         arguments: attacker-blocker vector, blocker
 ;         output: blocker or #f
 ;         description: find the blocker in the blockerlist of an attacker-blocker vector

 
 (define red-zone%
   (class object%
     (super-new)
     (define red-zone (pl:new (lambda (c1 c2) (eq? c1 (vector-ref c2 0)))))
     
     ; ( creature% -> vector)
     (define/private (find-attacker-blocker attacker)
       (let ((pos (pl:find red-zone attacker)))
         (if pos
             (pl:peek red-zone pos)
             #f)))
     
     ; ( creature% -> creature%)
     (define/private (find-attacker attacker)
       (let ((attacker-blocker (find-attacker-blocker attacker)))
         (if attacker-blocker
             (vector-ref attacker-blocker 0)
             #f)))
     
     ; ( vector creature% -> creature%)
     (define/private (find-blocker attacker-blocker blocker)
       (let loop ((blockers (vector-ref attacker-blocker 1)))
         (cond ((null? blockers) #f) 
               ((eq? (car blockers) blocker) (car blockers))
               (else (loop (cdr blockers) blocker)))))
     
     ; ( creature% -> )
     (define/public (add-attacking creature)
       (pl:add-before! red-zone (vector creature '())))
     
     ; ( creature% creature% -> )
     (define/public (add-blocking attacker blocker)
       (let ((attacker-blocker (find-attacker-blocker attacker)))
         (if attacker-blocker
             (vector-set! attacker-blocker 1 (cons blocker (vector-ref attacker-blocker 1))))))
     
     ; ( creature% creature% number -> )
     (define/public (assign-damage-to-blocker attacker blocker amount)
       (let* ((attacker-blocker (find-attacker-blocker attacker))
              (blocker (find-blocker attacker-blocker blocker)))
         (if (and attacker-blocker blocker)
             (if (> amount (send (vector-ref attacker-blocker 0) power))
                 (error "RED-ZONE -- assign-damage : amount too high")
                 (send blocker add-damage amount))
             #f)))
     
     ; ( creature% creature% number -> )
     (define/public (assign-damage-to-attacker blocker attacker amount)
       (let* ((attacker-blocker (find-attacker-blocker attacker))
              (blocker (find-blocker attacker-blocker blocker)))
         (if (and attacker-blocker blocker)
             (if (> (send blocker power) amount)
                 (error "RED-ZONE -- assign-damage : amount too high")
                 (send (vector-ref attacker-blocker 0) add-damage amount))
             #f)))
     
     ; ( ( creature% -> ) -> )
     (define/public (for-each-attacker func)
       (pl:for-each red-zone (lambda (a-b) (func (vector-ref a-b 0)))))
     
     ; ( ( creature% -> ) -> )
     (define/public (for-each-blocker func)
       (pl:for-each red-zone (lambda (a-b)
                               (for-each func (vector-ref a-b 1)))))
     
     ; ( -> number )
     (define/public (unblocked-damage)
       (let ((damage 0))
         (begin 
           (pl:for-each red-zone (lambda (a-b)
                                   (if (null? (vector-ref a-b 1))
                                       (set! damage (+ damage (send (vector-ref a-b 0) power))))))
           damage)))
         
     ; ( -> )
     (define/public (reset)
       (set! red-zone (pl:new (lambda (c1 c2) (eq? c1 (vector-ref c2 0)))))))))
