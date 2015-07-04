#|
File : turn
Author : Wouter Van Rossem 
|#

#!r6rs

(library
 (turn)
 (export turn%)
 (import (rnrs base (6))
         (rnrs io simple)
         (scheme class)
         (prefix (magicthegathering datastructures positional-list adt) pl:)
         (magicthegathering turn-parts)
         (magicthegathering permanents))
 
 ; Class name: turn
 ; Superclass: object%  
 ; Attributes: - public: none
 ;             - private: turn-phases, current-phase
 ; Methods:
 ;  - public:
 ;     * current-phase: 
 ;         arguments: none
 ;         output: the current phase
 ;     * current-step: 
 ;         arguments: none
 ;         output: the current step
 ;     * last-step?: 
 ;         arguments: none
 ;         output: bool
 ;         description: is it the last step of this phase?
 ;     * next: 
 ;         arguments: none
 ;         output: none
 ;         description: go to the next phase, step or go to the beginning of the turn
 ;     * curr-step/phase-start/end-actions
 ;         arguments: mtg
 ;         output: none
 ;         description: execute the start/end actions of the current step/phase
 ;     * add-phase: 
 ;         arguments: phase% to be added
 ;         output: none
 ;         description: add a new phase after this phase 
 ;     * add-step:
 ;         arguments: step% to be added
 ;         output: none
 ;         description: add a new step                     
 ;  - private: none
 
 (define turn%
   (class object%
     (super-new)
     ; The different phases and steps of a turn, with their start and end actions
     (define turn-phases 
       (pl:from-scheme-list 
        (list 
         ; Beginning phase
         (new phase% (name 'Beginning-phase) 
              (steps (list 
                      (new step% (name 'Untap-step) 
                           (start-actions (lambda (mtg) 
                                            (send (get-field manager mtg) notify-observers 'begin-of-untap 'begin-of-untap)
                                            (send mtg set-state! 'not-able-to-play-spells/abi)
                                            (send mtg untap-permanents)))                           
                           (end-actions (lambda (mtg) 
                                          (send (get-field manager mtg) notify-observers 'end-of-untap 'end-of-untap)
                                          (send mtg set-state! 'not-able-to-play-spells/abi))))
                      (new step% (name 'Upkeep-step)
                           (start-actions (lambda (mtg) 
                                            (send (get-field manager mtg) notify-observers 'begin-of-upkeep 'begin-of-upkeep)
                                            (send mtg set-state! 'able-to-play-spells/abi)))
                           (end-actions (lambda (mtg) 
                                          (send (get-field manager mtg) notify-observers 'end-of-upkeep 'end-of-upkeep)
                                          (send mtg set-state! 'able-to-play-spells/abi))))
                      (new step% (name 'Draw-step)
                           (start-actions (lambda (mtg)
                                            (send (get-field manager mtg) notify-observers 'begin-of-draw 'begin-of-draw)
                                            (send mtg draw-card (send mtg active-player))
                                            (send mtg set-state! 'able-to-play-spells/abi)))
                           (end-actions (lambda (mtg)
                                          (send mtg set-state! 'able-to-play-spells/abi))))))
              (start-actions (lambda (mtg) 
                               (send (get-field manager mtg) notify-observers 'begin-of-beginning 'begin-of-beginning)))
              (end-actions (lambda (mtg)
                             (send (get-field manager mtg) notify-observers 'end-of-beginning 'end-of-beginning)
                             (send mtg reset-mana-pools))))
         ; First main phase
         (new phase% (name 'Main-phase1)
              (steps (list (new step% (name 'Main-phase1))))
              (start-actions (lambda (mtg)
                               (send mtg set-state! 'main-phase)
                               (send (get-field manager mtg) notify-observers 'begin-of-first-main 'begin-of-first-main)))
              (end-actions (lambda (mtg)
                             (send (get-field manager mtg) notify-observers 'end-of-first-main 'end-of-first-main)
                             (send mtg reset-mana-pools))))
         ; Combat phase
         (new phase% (name 'Combat-phase)
              (steps (list (new step% (name 'Begin-of-combat)
                                (start-actions (lambda (mtg)
                                                 (send mtg start-combat-phase?)
                                                 (send (get-field manager mtg) notify-observers 'begin-of-begin-combat 'begin-of-begin-combat)))
                                (end-actions (lambda (mtg)
                                               (send (get-field manager mtg) notify-observers 'end-of-begin-combat 'end-of-begin-combat))))
                           (new step% (name 'Declare-attackers)
                                (start-actions (lambda (mtg) 
                                                 (send mtg set-state! 'declaring-attackers)
                                                 (send (get-field manager mtg) notify-observers 'begin-of-declare-attackers 'begin-of-declare-attackers)))
                                (end-actions (lambda (mtg)
                                               (send mtg set-state! 'able-to-play-spells/abi)
                                               (send (get-field manager mtg) notify-observers 'end-of-declare-attackers 'end-of-declare-attackers))))
                           (new step% (name 'Declare-blockers)
                                (start-actions (lambda (mtg)
                                                 (send mtg set-state! 'declaring-blockers)
                                                 (send (get-field manager mtg) notify-observers 'begin-of-declare-blockers 'begin-of-declare-blockers)))
                                (end-actions (lambda (mtg)
                                               (send mtg set-state! 'able-to-play-spells/abi)
                                               (send (get-field manager mtg) notify-observers 'end-of-declare-blockers 'end-of-declare-blockers))))
                           (new step% (name 'Combat-damage)
                                (start-actions (lambda (mtg)
                                                 (send mtg set-state! 'assigning-damage)
                                                 (send (get-field manager mtg) notify-observers 'begin-of-combat-damage 'begin-of-combat-damage)))
                                (end-actions (lambda (mtg)
                                               (send mtg set-state! 'able-to-play-spells/abi)
                                               (send mtg resolve-damage)
                                               (send (get-field manager mtg) notify-observers 'end-of-combat-damage 'end-of-combat-damage))))
                           (new step% (name 'End-of-combat)
                                (start-actions (lambda (mtg)
                                                 (send (get-field manager mtg) notify-observers 'begin-of-end-combat 'begin-of-end-combat)))
                                (end-actions (lambda (mtg)
                                               (send (get-field manager mtg) notify-observers 'end-of-end-combat 'end-of-end-combat))))))
              (end-actions (lambda (mtg)
                             (send mtg end-combat)
                             (send mtg reset-mana-pools))))
         ; Second main phase
         (new phase% (name 'Main-phase2)
              (steps (list (new step% (name 'Main-phase2))))
              (start-actions (lambda (mtg)
                               (send mtg set-state! 'main-phase)
                               (send (get-field manager mtg) notify-observers 'begin-of-second-main 'begin-of-second-main)))
              (end-actions (lambda (mtg)
                             (send (get-field manager mtg) notify-observers 'end-of-second-main 'end-of-second-main)
                             (send mtg reset-mana-pools))))
         ; End phase
         (new phase% (name 'End-phase)
              (steps (list (new step% (name 'End-of-turn)
                                (start-actions (lambda (mtg)
                                                 (send (get-field manager mtg) notify-observers 'begin-of-end 'begin-of-end)))
                                (end-actions (lambda (mtg)
                                               (send (get-field manager mtg) notify-observers 'end-of-end 'end-of-end))))
                           (new step% (name 'Cleanup)
                                (start-actions (lambda (mtg)
                                                 (send (get-field manager mtg) notify-observers 'begin-of-cleanup 'begin-of-cleanup)
                                                 (send mtg clean-up)))
                                (end-actions (lambda (mtg)
                                               (send (get-field manager mtg) notify-observers 'end-of-cleanup 'end-of-cleanup))))))
              (start-actions (lambda (mtg)
                               (send mtg set-state! 'able-to-play-spells/abi)))
              (end-actions (lambda (mtg) 
                             (send mtg reset-mana-pools)
                             (send mtg pass-turn)
                             (send mtg start-turn)))))
        eq?))
     (define curr-phase 0)
     
     ; ( -> phase%)
     (define/public (current-phase)
       (pl:peek-pos turn-phases curr-phase))
     
     ; ( -> step%)
     (define/public (current-step)
       (send (current-phase) current-step))
     
     ; ( -> bool )
     (define/public (last-step?)
       (send (current-phase) last-step?))
     
     ; ( -> )
     (define/public (next)
       (if (last-step?)
           (if (= (+ curr-phase 1) (pl:length turn-phases)) ; Check if it is the last phase = end of turn
               (begin (send (current-phase) set-step-to-first) (set! curr-phase 0))
               (begin (send (current-phase) set-step-to-first) (set! curr-phase (+ curr-phase 1))))
           (send (current-phase) next-step)))
     
     ; ( mtg% -> )
     (define/public (curr-step-start-actions mtg)
       ((get-field start-actions (current-step)) mtg))
     
     ; ( mtg% -> )
     (define/public (curr-step-end-actions mtg)
       ((get-field end-actions (current-step)) mtg))
     
     ; ( mtg% -> )
     (define/public (curr-phase-start-actions mtg)
       ((get-field start-actions (current-phase)) mtg))
     
     ; ( mtg% -> )
     (define/public (curr-phase-end-actions mtg)
       ((get-field end-actions (current-phase)) mtg))
     
      ; (phase% -> )
     (define/public (add-phase phase)
       (let ((after (pl:find turn-phases phase)))
         (if after
             (pl:add-after! turn-phases phase after))))
     
     ; (step% -> )
     (define/public (add-step step)
       (error "not yet implemented" step)))))