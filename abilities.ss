#|
File : abilities
Author : Wouter Van Rossem 
|#

#!r6rs

(library
 (abilities)
 (export ability% activated-ability% triggered-ability% static-ability% keyword-ability%)
 (import (rnrs base (6))
         (scheme class))
 
 ; Class name: ability
 ; Superclass: object%
 ; Description: This is the main superclass for all abilities
 ; Attributes: - public: target?, targets, modes
 ;             - private: none      
 ; Methods: - public: none
 ;          - private: none
 ; Extra info: There should be no instantiations of this class
 ;    target? is a procedure to check if a target is valid
 ;    targets is the number of targets the ability needs  
 ;    modes is for when the player has to choose between effects (e.g. choose one)
 
 (define ability%
   (class object%
     (init-field (target? 'none))
     (init-field (targets 0))
     (init-field (modes '()))
     (super-new)))
 
 ; Class name: activated-ability
 ; Superclass: ability%
 ; Description: 
 ; Attributes: - public: cost, effect, mana-ability?
 ;             - private: none  
 ; Methods: - public: none
 ;          - private: none
 ; Extra info: mana-abilities are abilities that can't be countered
 
 (define activated-ability%
   (class ability%
     (init-field cost)
     (init-field effect)
     (init-field (mana-ability? #f))
     (super-new)))
 
 ; Class name: triggered-ability
 ; Superclass: ability%
 ; Attributes: - public: trigger, effect, mana-ability?
 ;             - private: none      
 ; Methods: - public: none
 ;          - private: none
 ; Extra info: mana-abilities are abilities that can't be countered
 ;    trigger should be a symbol, the name of the trigger
 
 (define triggered-ability%
   (class ability%
     (init-field trigger)
     (init-field effect)
     (init-field (mana-ability? #f))
     (super-new)))
 
 ; Class name: static-ability
 ; Superclass: ability%
 ; Attributes: - public: effect
 ;             - private: none      
 ; Methods: - public: none
 ;          - private: none
 
 (define static-ability%
   (class ability%
     (init-field effect)
     (super-new)))
 
 ; Class name: keyword-ability
 ; Superclass: ability%
 ; Attributes: - public: keyword-name, ability
 ;             - private: none      
 ; Methods:  - public: none
 ;           - private: none
 ; Extra info: keyword-name should be a symbol
 
 (define keyword-ability%
   (class ability%
     (init-field keyword-name)
     (init-field (ability 0))
     (super-new))))