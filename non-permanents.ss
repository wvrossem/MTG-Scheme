#|
File : non-permanents
Author : Wouter Van Rossem 
|#

#!r6rs

(library
 (non-permanent)
 (export non-permanent% sorcery% instant%)
 (import (rnrs base (6))
         (scheme class)
         (magicthegathering card))
 
 ; Class name: non-permanent
 ; Superclass: card%
 ; Attributes: - public: effect, spell-type, targets, target?, modes
 ;             - private: none      
 ; Methods:
 ;  - public: none
 ;  - private: none
 ; Extra info:
 ;    target? is a procedure to check if a target is valid
 ;    targets is the number of targets the ability needs  
 ;    modes is for when the player has to choose between effects (e.g. choose one)
 
 (define non-permanent%
   (class card%
     (init-field effect)
     (init-field (spell-type '()))
     (init-field (targets 0))
     (init-field (target? 'none))
     (init-field (modes '())) ; list of effects
     (super-new)))
 
 ; Class name: sorcery
 ; Superclass: non-permanent%
 ; Attributes: - public: none
 ;             - private: none      
 ; Methods:
 ;  - public: none
 ;  - private: none
 
 (define sorcery%
   (class non-permanent%
     (super-new)))
 
 ; Class name: instant
 ; Superclass: non-permanent%  
 ; Attributes: - public: none
 ;             - private: none  
 ; Methods:
 ;  - public: none
 ;  - private: none
 
 (define instant%
   (class non-permanent% 
     (super-new))))