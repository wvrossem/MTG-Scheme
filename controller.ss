#|
File : controller
Author : Wouter Van Rossem 
|#

#!r6rs

(library
 (controller)
 (export controller%)
 (import (rnrs base (6))
         (rnrs io simple)
         (rnrs mutable-pairs)
         (scheme class)
         (only (scheme list) flatten)
         (prefix (scheme base) s:)
         (magicthegathering observer-pattern)         
         (magicthegathering mtg)
         (magicthegathering gui))
 
 ; Class name: controller
 ; Superclass: observer%
 ; Description: The controller observes the model for updates and sends these to the view.
 ;    It also receives updates (user interactions) from the view and dispatches these to the model.
 ; Attributes: - public: model, view
 ;             - private: none      
 ; Methods: 
 ;  - public: 
 ;     * update: 
 ;         arguments: update-name, optionally some arguments
 ;         output: none
 ;         description: This method will dispatch the different messages to the model and view
 ;  - private: none
 ; Extra info: When a controller objetc is created it will automatically subscribe (attach) to the 
 ;    the model and view subjects 
 
 (define controller%
   (class observer%
     (init-field model)
     (init-field view)
     (super-new)
     
     ; ( symbol pair -> )
     (define/override (update update-name . args)
       (let ((args (flatten args)))
         (case update-name
           ; view updates -> model
           ((select-card) (send model get-update (case (s:cadr args) ; zone 
                                                   ((nonland-permanents) 'select-nonlandpermanent)
                                                   ((lands) 'select-land)
                                                   ((hand) 'select-hand-card)
                                                   ((graveyard) 'select-graveyard-card)
                                                   ((rfg) 'select-rfg-card))
                                (s:car args) (s:caddr args)))
           ((select-mana) (send model get-update 'select-mana (s:car args) (s:cadr args)))
           ((select-player) (send model get-update 'select-player (s:car args)))
           ((pass-priority) (send model get-update 'pass-priority))
           ((cancel-announced-spell) (send model cancel-announced-spell))
           ((new-game) (send model new-game))
           ; model updates -> view
           ((initialize-hand) (send view initialize-hand (s:car args)))
           ((clear-hand) (send view clear-hand (s:car args)))
           ((update-library-size) (send view update-library-size (s:car args)))
           ((update-graveyard-size) (send view update-graveyard-size (s:car args)))
           ((update-rfg-size) (send view update-rfg-size (s:car args)))
           ((update-life) (send view update-life (s:car args)))
           ((update-phase) (send view update-phase))
           ((update-step) (send view update-step))
           ((update-priority) (send view update-priority))
           ((update-nr-of-turns) (send view update-nr-of-turns))
           ((update-active-player) (send view update-active-player))
           ((add-card) (send view add-card (s:car args) (s:cadr args) (s:caddr args)))
           ((remove-card) (send view remove-card (s:car args) (s:cadr args) (s:caddr args)))
           ((tap-permanent) (send view tap-permanent (s:car args) (s:cadr args)))
           ((untap-permanents) (send view untap-permanents (s:car args)))
           ((update-mana) (send view update-mana (s:car args) (s:cadr args)))
           ((update-all-mana) (send view update-all-mana (s:car args)))
           ((announce-spell) (send view announce-spell (s:car args)))
           ((update-announced-spell-mana) (send view update-announced-spell-mana))
           ((update-announced-spell-state) (send view update-announced-spell-state (s:car args)))
           ((add-stack-spell) (send view add-stack-spell (s:car args)))
           ((announced-spell-done) (send view announced-spell-done))
           ((remove-top-of-stack) (send view remove-top-of-stack))
           ((set-attacking) (send view set-attacking (s:car args) (s:cadr args)))
           ((remove-attacking) (send view remove-attacking (s:car args) (s:cadr args)))
           ((set-blocking) (send view set-blocking (s:car args) (s:cadr args)))
           ((remove-blocking) (send view remove-blocking (s:car args) (s:cadr args)))
           (else (error "CONTROLLER -- unknow update-name" update-name)))))
     
     (send model attach this)
     (send view attach this))))