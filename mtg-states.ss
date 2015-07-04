#|
File : mtg states
Author : Wouter Van Rossem 
|#

#!r6rs

(library
 (mtg-states)
 (export starting% able-to-play-spells/abi% not-able-to-play-spells/abi%
         main-phase% discarding% choosing-spell/abi-target% spending-mana%
         declaring-attackers% declaring-blockers% assigning-damage% declare-blocker-target%
         assigning-damage-to% clean-up%)
 (import (rnrs base (6))
         (rnrs io simple)
         (scheme class)
         (magicthegathering permanents)
         (magicthegathering non-permanents)
         (magicthegathering gui))
 
 ; Interface name: state     
 ; Methods:
 ;  - public: 
 ;     * select-nonlandpermanent: 
 ;         arguments: permanent, owner
 ;         output : none
 ;         description: what to do in a state when we select a nonland-permanent
 ;     * select-land: 
 ;         arguments: land, owner
 ;         output : none
 ;         description: what to do in a state when we select a land
 ;     * select-hand-card: 
 ;         arguments: card, owner
 ;         output: none
 ;         description: what to do in a state when we select a card in a hand
 ;     * select-graveyard-card: 
 ;         arguments: card, owner
 ;         output : none
 ;         description: what to do in a state when we select a card in a graveyard
 ;     * select-rfg-card: 
 ;         arguments: card, owner
 ;         output : none
 ;         description: what to do in a state when we select a card in the rfg zone
 ;     * select-mana: 
 ;         arguments: mana-color, owner
 ;         output : none
 ;         description: what to do in a state when we select a type of mana
 ;     * select-player: 
 ;         arguments: player
 ;         output : none
 ;         description: what to do in a state when we select a player
 ;     * pass-priority: 
 ;         arguments: none
 ;         output: none
 ;         description: what to do in a state when we get the message pass priority
 ;  - private: none
 
 (define state<%> (interface () select-nonlandpermanent select-land select-hand-card 
                    select-graveyard-card select-rfg-card
                    select-mana select-player pass-priority))
 
 (define starting%
   (class* object% (state<%>)
     (init-field mtg)
     (super-new)
     
     ; ( permanent% player% -> )
     (define/public (select-nonlandpermanent permanent owner)
       (send mtg do-nothing))
     
     ; ( land% player% -> )
     (define/public (select-land land owner)
       (send mtg do-nothing))
     
     ; ( card% player% -> )
     (define/public (select-hand-card card owner)
       (send mtg do-nothing))
     
     ; ( card% player% -> )
     (define/public (select-graveyard-card card owner)
       (send mtg do-nothing))
     
     ; ( card% player% -> )
     (define/public (select-rfg-card card owner)
       (send mtg do-nothing))
     
     ; ( symbol player% -> )
     (define/public (select-mana mana-color owner)
       (send mtg do-nothing))
     
     ; ( player% -> )
     (define/public (select-player player)
       (send mtg do-nothing))
     
     ; ( -> )
     (define/public (pass-priority)
       (send mtg do-nothing))))
 
 (define able-to-play-spells/abi%
   (class* object% (state<%>)
     (init-field mtg)
     (super-new)
     
     ; ( permanent% player% -> )
     (define/public (select-nonlandpermanent permanent owner)
       (send mtg use-ability permanent owner))
     
     ; ( land% player% -> )
     (define/public (select-land land owner)
       (send mtg use-land land owner))
     
     ; ( card% player% -> )
     (define/public (select-hand-card card owner)
       (if (is-a? card instant%)
           (send mtg announce-spell card owner)
           (send mtg do-nothing)))
     
     ; ( card% player% -> )
     (define/public (select-graveyard-card card owner)
       (send mtg use-ability card owner))
     
     ; ( card% player% -> )
     (define/public (select-rfg-card card owner)
       (send mtg do-nothing))
     
     ; ( symbol player% -> )
     (define/public (select-mana mana-color owner)
       (send mtg do-nothing))
     
     ; ( player% -> )
     (define/public (select-player player)
       (send mtg do-nothing))
     
     ; ( -> )
     (define/public (pass-priority)
       (send mtg pass-priority))))
 
 (define not-able-to-play-spells/abi%
   (class* object% (state<%>)
     (init-field mtg)
     (super-new)
     
     ; ( permanent% player% -> )
     (define/public (select-nonlandpermanent permanent owner)
       (send mtg do-nothing))
     
     ; ( land% player% -> )
     (define/public (select-land land owner)
       (send mtg do-nothing))
     
     ; ( card% player% -> )
     (define/public (select-hand-card card owner)
       (send mtg do-nothing))
     
     ; ( card% player% -> )
     (define/public (select-graveyard-card card owner)
       (send mtg do-nothing))
     
     ; ( card% player% -> )
     (define/public (select-rfg-card card owner)
       (send mtg do-nothing))
     
     ; ( symbol player% -> )
     (define/public (select-mana mana-color owner)
       (send mtg do-nothing))
     
     ; ( player% -> )
     (define/public (select-player player)
       (send mtg do-nothing))
     
     ; ( -> )
     (define/public (pass-priority)
       (send mtg pass-priority))))
 
 (define main-phase%
   (class* object% (state<%>)
     (init-field mtg)
     (super-new)
     
     ; ( permanent% player% -> )
     (define/public (select-nonlandpermanent permanent owner)
       (send mtg use-ability permanent owner))
     
     ; ( land% player% -> )
     (define/public (select-land land owner)
       (send mtg use-land land owner))
     
     ; ( card% player% -> )
     (define/public (select-hand-card card owner)
       (send mtg announce-spell card owner))
     
     ; ( card% player% -> )
     (define/public (select-graveyard-card card owner)
       (send mtg use-ability card owner))
     
     ; ( card% player% -> )
     (define/public (select-rfg-card card owner)
       (send mtg use-ability card owner))
     
     ; ( symbol player% -> )
     (define/public (select-mana mana-color owner)
       (send mtg do-nothing))
     
     ; ( player% -> )
     (define/public (select-player player)
       (send mtg do-nothing))
     
     ; ( -> )
     (define/public (pass-priority)
       (send mtg pass-priority))))
 
 (define discarding%
   (class* object% (state<%>)
     (init-field mtg)
     (super-new)
     (field (player-to-discard 0))
     
     (define/public (set-player-to-discard player)
       (set! player-to-discard player))
     
     ; ( permanent% player% -> )
     (define/public (select-nonlandpermanent permanent owner)
       (send mtg do-nothing))
     
     ; ( land% player% -> )
     (define/public (select-land land owner)
       (send mtg fo-nothing))
     
     ; ( card% player% -> )
     (define/public (select-hand-card card owner)
       (send mtg discard-card card owner))
     
     ; ( card% player% -> )
     (define/public (select-graveyard-card card owner)
       (send mtg do-nothing))
     
     ; ( card% player% -> )
     (define/public (select-rfg-card card owner)
       (send mtg do-nothing))
     
     ; ( symbol player% -> )
     (define/public (select-mana mana-color owner)
       (send mtg do-nothing))
     
     ; ( player% -> )
     (define/public (select-player player)
       (send mtg do-nothing))
     
     ; ( -> )
     (define/public (pass-priority)
       (send mtg do-nothing))))
 
 (define choosing-spell/abi-target%
   (class* object% (state<%>)
     (init-field mtg)
     (super-new)
     
     ; ( permanent% player% -> )
     (define/public (select-nonlandpermanent permanent owner)
       (send mtg select-target permanent owner))
     
     ; ( land% player% -> )
     (define/public (select-land land owner)
       (send mtg select-target land owner))
     
     ; ( permanent% player% -> )
     (define/public (select-hand-card card owner)
       (send mtg do-nothing))
     
     ; ( card% player% -> )
     (define/public (select-graveyard-card card owner)
       (send mtg select-target card owner))
     
     ; ( card% player% -> )
     (define/public (select-rfg-card card owner)
       (send mtg select-target card owner))
     
     ; ( symbol player% -> )
     (define/public (select-mana mana-color owner)
       (send mtg do-nothing))
     
     ; ( player% -> )
     (define/public (select-player player)
       (send mtg select-target player player))
     
     ; ( -> )
     (define/public (pass-priority)
       (send mtg do-nothing))))
 
 (define spending-mana%
   (class* object% (state<%>)
     (init-field mtg)
     (super-new)
     
     ; ( permanent% player% -> )
     (define/public (select-nonlandpermanent permanent owner)
       (send mtg do-nothing))
     
     ; ( land% player% -> )
     (define/public (select-land land owner)
       (send mtg use-land land owner))
     
     ; ( permanent% player% -> )
     (define/public (select-hand-card card owner)
       (send mtg do-nothing))
     
     ; ( card% player% -> )
     (define/public (select-graveyard-card card owner)
       (send mtg do-nothing))
     
     ; ( card% player% -> )
     (define/public (select-rfg-card card owner)
       (send mtg do-nothing))
     
     ; ( symbol player% -> )
     (define/public (select-mana mana-color owner)
       (send mtg spend-mana mana-color owner))
     
     ; ( player% -> )
     (define/public (select-player player)
       (send mtg do-nothing))
     
     ; ( -> )
     (define/public (pass-priority)
       (send mtg do-nothing))))
 
 (define declaring-attackers%
   (class* object% (state<%>)
     (init-field mtg)
     (super-new)
     
     ; ( creature% player% -> )
     (define/private (select-creature creature owner)
       (send mtg declare-attacker creature owner))
     
     ; ( permanent% player% -> )
     (define/public (select-nonlandpermanent permanent owner)
       (if (is-a? permanent creature%)
           (select-creature permanent owner)
           (send mtg do-nothing)))
     
     ; ( land% player% -> )
     (define/public (select-land land owner)
       (send mtg do-nothing))
     
     ; ( card% player% -> )
     (define/public (select-hand-card card owner)
       (send mtg do-nothing))
     
     ; ( card% player% -> )
     (define/public (select-graveyard-card card owner)
       (send mtg do-nothing))
     
     ; ( card% player% -> )
     (define/public (select-rfg-card card owner)
       (send mtg do-nothing))
     
     ; ( symbol player% -> )
     (define/public (select-mana mana-color owner)
       (send mtg do-nothing))
     
     ; ( player% -> )
     (define/public (select-player player)
       (send mtg do-nothing))
     
     ; ( -> )
     (define/public (pass-priority)
       (send mtg end-declare-attackers))))
 
 (define declaring-blockers%
   (class* object% (state<%>)
     (init-field mtg)
     (super-new)
     
     ; ( creature% player% -> )
     (define/private (select-creature creature owner)
       (send mtg declare-blocker creature owner))
     
     ; ( permanent% player% -> )
     (define/public (select-nonlandpermanent permanent owner)
       (if (is-a? permanent creature%)
           (select-creature permanent owner)
           (send mtg do-nothing)))
     
     ; ( land% player% -> )
     (define/public (select-land land owner)
       (send mtg do-nothing))
     
     ; ( card% player% -> )
     (define/public (select-hand-card card owner)
       (send mtg do-nothing))
     
     ; ( card% player% -> )
     (define/public (select-graveyard-card card owner)
       (send mtg do-nothing))
     
     ; ( card% player% -> )
     (define/public (select-rfg-card card owner)
       (send mtg do-nothing))
     
     ; ( symbol player% -> )
     (define/public (select-mana mana-color owner)
       (send mtg do-nothing))
     
     ; ( player% -> )
     (define/public (select-player player)
       (send mtg do-nothing))
     
     ; ( -> )
     (define/public (pass-priority)
       (send mtg end-declare-blockers))))
 
 (define declare-blocker-target%
   (class* object% (state<%>)
     (init-field mtg)
     (super-new)
     
     ; ( creature% player% -> )
     (define/private (select-creature creature owner)
       (send mtg declare-blocker-target creature owner))
     
     ; ( permanent% player% -> )
     (define/public (select-nonlandpermanent permanent owner)
       (if (is-a? permanent creature%)
           (select-creature permanent owner)
           (send mtg do-nothing)))
     
     ; ( land% player% -> )
     (define/public (select-land land owner)
       (send mtg do-nothing))
     
     ; ( card% player% -> )
     (define/public (select-hand-card card owner)
       (send mtg do-nothing))
     
     ; ( card% player% -> )
     (define/public (select-graveyard-card card owner)
       (send mtg do-nothing))
     
     ; ( card% player% -> )
     (define/public (select-rfg-card card owner)
       (send mtg do-nothing))
     
     ; ( symbol player% -> )
     (define/public (select-mana mana-color owner)
       (send mtg do-nothing))
     
     ; ( player% -> )
     (define/public (select-player player)
       (send mtg do-nothing))
     
     ; ( -> )
     (define/public (pass-priority)
       (send mtg set-state! 'declaring-blockers))))
 
 (define assigning-damage%
   (class* object% (state<%>)
     (init-field mtg)
     (super-new)
     
     ; ( creature% player% -> )
     (define/private (select-creature creature owner)
       (send mtg assign-damage creature owner))
     
     ; ( permanent% player% -> )
     (define/public (select-nonlandpermanent permanent owner)
       (if (is-a? permanent creature%)
           (select-creature permanent owner)
           (send mtg do-nothing)))
     
     ; ( land% player% -> )
     (define/public (select-land land owner)
       (send mtg do-nothing))
     
     ; ( card% player% -> )
     (define/public (select-hand-card card owner)
       (send mtg do-nothing))
     
     ; ( card% player% -> )
     (define/public (select-graveyard-card card owner)
       (send mtg do-nothing))
     
     ; ( card% player% -> )
     (define/public (select-rfg-card card owner)
       (send mtg do-nothing))
     
     ; ( symbol player% -> )
     (define/public (select-mana mana-color owner)
       (send mtg do-nothing))
     
     ; ( player% -> )
     (define/public (select-player player)
       (send mtg do-nothing))
     
     ; ( -> )
     (define/public (pass-priority)
       (send mtg end-assign-damage))))
 
 (define assigning-damage-to%
   (class* object% (state<%>)
     (init-field mtg)
     (super-new)
     
     ; ( creature% player% -> )
     (define/private (select-creature creature owner)
       (send mtg assign-damage-to creature owner))
     
     ; ( permanent% player% -> )
     (define/public (select-nonlandpermanent permanent owner)
       (if (is-a? permanent creature%)
           (select-creature permanent owner)
           (send mtg do-nothing)))
     
     ; ( land% player% -> )
     (define/public (select-land land owner)
       (send mtg do-nothing))
     
     ; ( card% player% -> )
     (define/public (select-hand-card card owner)
       (send mtg do-nothing))
     
     ; ( card% player% -> )
     (define/public (select-graveyard-card card owner)
       (send mtg do-nothing))
     
     ; ( card% player% -> )
     (define/public (select-rfg-card card owner)
       (send mtg do-nothing))
     
     ; ( symbol player% -> )
     (define/public (select-mana mana-color owner)
       (send mtg do-nothing))
     
     ; ( player% -> )
     (define/public (select-player player)
       (send mtg assign-damage-to player player))
     
     ; ( -> )
     (define/public (pass-priority)
       (send mtg set-state! 'assigning-damage))))
 
 (define clean-up%
   (class* object% (state<%>)
     (init-field mtg)
     (super-new)
     
     ; ( permanent% player% -> )
     (define/public (select-nonlandpermanent permanent owner)
       (send mtg do-nothing))
     
     ; ( land% player% -> )
     (define/public (select-land land owner)
       (send mtg do-nothing))
     
     ; ( card% player% -> )
     (define/public (select-hand-card card owner)
       (send mtg discard-to-max-size card owner))
     
     ; ( card% player% -> )
     (define/public (select-graveyard-card card owner)
       (send mtg do-nothing))
     
     ; ( card% player% -> )
     (define/public (select-rfg-card card owner)
       (send mtg do-nothing))
     
     ; ( symbol player% -> )
     (define/public (select-mana mana-color owner)
       (send mtg do-nothing))
     
     ; ( player% -> )
     (define/public (select-player player)
       (send mtg do-nothing))
     
     ; ( -> )
     (define/public (pass-priority)
       (send mtg do-nothing)))))
     
     
