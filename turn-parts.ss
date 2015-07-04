#|
File : card
Author : Wouter Van Rossem 
|#

#!r6rs


(library
 (turn-parts)
 (export step% phase%)
 (import (rnrs base (6))
         (rnrs io simple)
         (scheme class)
         (prefix (magicthegathering datastructures positional-list adt) pl:))
 
 ; Class name: step
 ; Superclass: object%  
 ; Attributes: - public: name, start-actions, end-actions
 ;             - private: none
 ; Methods:
 ;  - public: none                    
 ;  - private: none
 
 (define step%
   (class object%
     (init-field name)
     (init-field (start-actions (lambda (x) x)))
     (init-field (end-actions (lambda (x) x)))
     (super-new)))
 
 (define empty-step
   (new step% (name 'no-step)))
 
 ; Class name: phase
 ; Superclass: object%  
 ; Attributes: - public: name, steps, start-actions, end-actions
 ;             - private: current-step, steps-list = positional list
 ; Methods:
 ;  - public:
 ;     * last-step?: 
 ;         arguments: none
 ;         output: bool
 ;         description: check if the current-step is the last step of this phase
 ;     * current-step: 
 ;         arguments: none
 ;         output: step%
 ;         description: return the current-step
 ;     * next-step: 
 ;         arguments: none
 ;         output: none
 ;         description: go to the next step, i.e. increase current-step with 1
 ;     * set-step-to-first:
 ;         arguments: none
 ;         output: none
 ;         description: set the current step to the first step of the phase
 ;     * add-step:
 ;         arguments: step% to be added
 ;         output: none
 ;         description: add a new step to this phase                      
 ;  - private: none
 
 (define phase%
   (class object%
     (init-field name)
     (init-field (steps (list empty-step)))
     (init-field (start-actions (lambda (x) x)))
     (init-field (end-actions (lambda (x) x)))
     (super-new)
     (define curr-step 0)
     (define steps-list 
       (pl:from-scheme-list steps (lambda (s1 s2) eq?)))
     
     ; ( -> bool )
     (define/public (last-step?)
       (= (+ curr-step 1) (pl:length steps-list)))
     
     ; ( -> step% )
     (define/public (current-step)
       (pl:peek-pos steps-list curr-step))
     
     ; ( -> )
     (define/public (next-step)
       (if (not (last-step?))
           (set! curr-step (+ curr-step 1))))
     
     ; ( -> )
     (define/public (set-step-to-first)
       (set! curr-step 0))
     
     ; ( step% -> )
     (define/public (add-step step)
       (let ((after (pl:find steps-list step)))
         (if after
             (pl:add-after! steps-list step after)))))))
     
     