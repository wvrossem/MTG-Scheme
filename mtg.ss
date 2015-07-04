#|
File : magicthegathering
Author : Wouter Van Rossem 
|#

#!r6rs

(library
 (magicthegathering)
 (export mtg%)
 (import (rnrs base (6))
         (rnrs io simple)
         (rnrs control (6))
         (scheme class)
         (only (scheme base) member random)
         (prefix (scheme base) s:)
         (magicthegathering decks)
         (magicthegathering observer-pattern)
         (magicthegathering mtg-states)
         (magicthegathering player)
         (magicthegathering the-stack)
         (magicthegathering turn)
         (magicthegathering red-zone)
         (magicthegathering abilities)
         (magicthegathering card)
         (magicthegathering non-permanents)
         (magicthegathering permanents)) ;is-a?
 
 ; Class name: mtg
 ; Superclass: subject%
 ; Attributes: - public: view, nr-of-players, priority, turn, stack, manager
 ;                  turn,-nr, standings
 ;             - private: players, curr-player, priorities-passed, red-zone, states       
 ; Methods:
 ;  - public: 
 ;     * (non)active-player: 
 ;         arguments: none
 ;         output: the (non)active-player
 ;     * get-player: 
 ;         arguments: nr
 ;         output: player
 ;     * for-each-player: 
 ;         arguments: lambda
 ;         output: none
 ;         description: apply a procedure to each player
 ;     * set-up: 
 ;         arguments: none
 ;         output: none
 ;         description: sets up the game
 ;     * start 
 ;         arguments: none
 ;         output: none
 ;         description: start the game
 ;     * end: 
 ;         arguments: none
 ;         output: none
 ;         description: end the game
 ;     * new-game: 
 ;         arguments: none
 ;         output: none
 ;         description: start a new game
 ;     * next: 
 ;         arguments: none
 ;         output: none
 ;         description: go to the next step/phase or go to the next turn
 ;            Also execute all start- and end-actions
 ;     * pass-priority: 
 ;         arguments: none
 ;         output: none
 ;         description: pass the priority
 ;           a)When both players pass priority and stack is empty, phase/step ends
 ;           b)When both players pass priority and stack is not empty, resolve the top of stack
 ;     * pass-turn: 
 ;         arguments: none
 ;         output: none
 ;         description: pass the turn
 ;     * draw-card: 
 ;         arguments: player
 ;         output: none
 ;         description: the player draws a card
 ;     * discard-card: 
 ;         arguments: card, player
 ;         output: none
 ;         description: the player discards the card
 ;     * untap-permanents: 
 ;         arguments: none
 ;         output: none
 ;         description: untap the permanents of the active player
 ;     * start-turn: 
 ;         arguments: none
 ;         output: none
 ;         description: start the turn, i.e. do some actions
 ;     * remove-summoning-sicknes: 
 ;         arguments: none
 ;         output: none
 ;         description: remove summoning-sickness from the creatures of the active player
 ;     * reset-mana-pool: 
 ;         arguments: none
 ;         output: none
 ;         description: reset the mana-pools of the players, players can get mana-burn 
 ;     * clean-up: 
 ;         arguments: none
 ;         output: none
 ;         description: do the actions of the clean-up step
 ;     * discard-to-max-size: 
 ;         arguments: card owner
 ;         output: none
 ;         description: used in clean-up step
 ;     * use-land: 
 ;         arguments: land, owner
 ;         output: none
 ;         description: use a land, i.e. add mana to mana-pool of owner
 ;            Currently only checks for basic landtypes, this can be fixed by using an ability of the land 
 ;            if it is not a basic land
 ;     * use-ability:
 ;         arguments: card, owner
 ;         output: none
 ;         description: used an activated ability of a card
 ;            If there are more than one available activated-abilities, the player must choose wich he wants to use
 ;     * put-ability-on-stack:
 ;         arguments: card, owner
 ;         output: none
 ;         description: put an ability on the stack
 ;     * announce-spell: 
 ;         arguments: card owner
 ;         output: none
 ;         description: announce a spell to be played, then start choose-mode
 ;     * cancel-announced-spell: 
 ;         arguments: none
 ;         output: none
 ;         description: cancel the announced spell
 ;     * start-choose-mode: 
 ;         arguments: none
 ;         output: none
 ;         description: if the spell/abi has modes, they will be chosen now
 ;     * start-choose-target: 
 ;         arguments: none
 ;         output: none
 ;         description: targets will now be chosen for the announced spell/abi
 ;     * select-target: 
 ;         arguments: card owner
 ;         output: none
 ;         description: select a target for the announced-spell
 ;            If all targets are chosen, start determine-total-cost
 ;     * determine-total-cost: 
 ;         arguments: none
 ;         output: none
 ;         description: determine the total cost for the spell
 ;            Then start pay-mana
 ;     * start-pay-mana: 
 ;         arguments: none
 ;         output: none
 ;         description: start paying mana for the announced-spell/abi
 ;     * play-land: 
 ;         arguments: land owner
 ;         output: none
 ;         description: owner plays a land (doesn't use the stack)
 ;     * spend-mana: 
 ;         arguments: color player
 ;         output: none
 ;         description: spend mana for the announced-spell/abi
 ;     * play-announced-spell: 
 ;         arguments: none
 ;         output: none
 ;         description: put the announced spell on the stack
 ;     * resolve-top-of-stack: 
 ;         arguments: none
 ;         output: none
 ;         description: resolve the top of the stack
 ;     * resolve-top-of-stack: 
 ;         arguments: none
 ;         output: none
 ;         description: resolve the top of the stack
 ;     * select-card-from-library: 
 ;         arguments: topx, condition, end-action
 ;         output: none
 ;         description: select a card from library
 ;            a) check if target is legal with condition
 ;            b) if so, do end-action with target
 ;     * apply-static-abilities: 
 ;         arguments: player
 ;         output: none
 ;         description: apply all the static abilities of permanents from a player
 ;     * start-combat-phase?: 
 ;         arguments: none
 ;         output: none
 ;         description: ask active-player if he/she wants to skip combat this turn
 ;     * declare-attacker: 
 ;         arguments: creature, owner
 ;         output: none
 ;         description: delcare creature attacker
 ;     * declare-blocker: 
 ;         arguments: creature, owner
 ;         output: none
 ;         description: delcare creature blocker, now choose blocker target
 ;     * declare-blocker-target: 
 ;         arguments: creature, owner
 ;         output: none
 ;         description: set the targer for the blocker
 ;     * end-declare-attackers: 
 ;         arguments: none
 ;         output: none
 ;         description: end the declare attackers
 ;     * end-declare-blockers: 
 ;         arguments: none
 ;         output: none
 ;         description: end the declare blockers
 ;     * end-declare-attacker: 
 ;         arguments: none
 ;         output: none
 ;         description: end the declare attackers
 ;     * assign-damage: 
 ;         arguments: creature, owner
 ;         output: none
 ;         description: assign damage from this creature
 ;     * assign-damage-to: 
 ;         arguments: creature, owner
 ;         output: none
 ;         description: assign damage to this creature
 ;     * end-declare-attacker: 
 ;         arguments: none
 ;         output: none
 ;         description: end the declare attackers
 ;     * end-assign-damage: 
 ;         arguments: none
 ;         output: none
 ;         description: end the assign damage
 ;     * resolve-damage: 
 ;         arguments: none
 ;         output: none
 ;         description: resolve the damage on each creature
 ;     * end-combat: 
 ;         arguments: none
 ;         output: none
 ;         description: end combat
 ;     * do-nothing: 
 ;         arguments: none
 ;         output: none
 ;         description: needed by states, doed nothing special
 ;     * get-update: 
 ;         arguments: update-name 
 ;         output: none
 ;         description: get an update from the view, send this to the current state
 ;     * set-state: 
 ;         arguments: state
 ;         output: none
 ;         description: set the state to the new state
 ;  - private: 
 ;     * check-sbe: 
 ;         arguments: none
 ;         output: none
 ;         description: check the state bases effects
 ;     * land->mana-type: 
 ;         arguments: land%
 ;         output: none
 ;         description: return the color of mana that a basic land produces
 ;     * announced-spell/abi: 
 ;         arguments: none
 ;         output: the current announced-spell/abi
 ;     * update-mana: 
 ;         arguments: color player
 ;         output: none
 ;         description: decrease the mana of color in player's mana-pool with 1
 ;     * resolve-permanent: 
 ;         arguments: stack-spell/abi
 ;         output: none
 ;         description: what to do when we resolve a permanent from the top of the stack
 ;     * resolve-spell: 
 ;         arguments: stack-spell/abi
 ;         output: none
 ;         description: what to do when we resolve a nonpermanent from the top of the stack
 ;     * resolve-ability: 
 ;         arguments: stack-spell/abi
 ;         output: none
 ;         description: what to do when we resolve an ability from the top of the stack
 ;     * skip-combat-phase: 
 ;         arguments: none
 ;         output: none
 ;         description: skip the combat phase, i.e. do next till main-phase2
 ;     * start-combat-phase: 
 ;         arguments: none
 ;         output: none
 ;         description: start the combat phase
 
 (define mtg%
   (class subject% ; attach/detach/notify
     (init-field view)
     (init-field (nr-of-players 2))
     (inherit attach detach notify)
     (super-new (name 'mtg))
     (define players (make-vector nr-of-players))
     (define curr-player 0)
     (field (priority curr-player))
     (define priorities-passed 0)
     (field (turn (new turn%)))
     (field (stack (new the-stack%)))
     (define red-zone (new red-zone%))
     (field (manager (new manager%)))
     ;games info
     (field (turn-nr 1))
     (field (standings (vector 0 0)))
     
     ; states
     (define starting (new starting% (mtg this)))
     (define able-to-play-spells/abi (new able-to-play-spells/abi% (mtg this)))
     (define not-able-to-play-spells/abi (new not-able-to-play-spells/abi% (mtg this)))
     (define choosing-spell/abi-target (new choosing-spell/abi-target% (mtg this)))
     (define spending-mana (new spending-mana% (mtg this)))
     (define main-phase (new main-phase% (mtg this)))
     (define discarding (new discarding% (mtg this)))
     (define declaring-attackers (new declaring-attackers% (mtg this)))
     (define declaring-blockers (new declaring-blockers% (mtg this)))
     (define declare-blocker-target-state (new declare-blocker-target% (mtg this)))
     (define assigning-damage (new assigning-damage% (mtg this)))
     (define assigning-damage-to (new assigning-damage-to% (mtg this)))
     (define clean-up-state (new clean-up% (mtg this)))
     ; current-state
     (define state starting)
      
     ; ( -> player% )
     (define/public (active-player)
       (vector-ref players curr-player))
     ; ( -> player% )
     (define/public (nonactive-player)
       (if (= curr-player 0)
           (vector-ref players (- nr-of-players 1))
           (vector-ref players (- curr-player 1))))
     
     ; ( number -> player% )
     (define/public (get-player nr)
       (vector-ref players (- nr 1)))
     
     ; ( ( player% -> ) -> )
     (define/public (for-each-player func)
       (vector-for-each func players))
     
     ; ( -> )
     (define/public (set-up)
       (define (set-up-players)
         (do ((i 0 (+ i 1)))
           ((= i nr-of-players))
           (let loop ((name (send view get-user-input (string-append "Player " (number->string (+ i 1)) " enter your name")))
                      (deck (send view get-user-choice (string-append "Player " (number->string (+ i 1)) " choose your deck") decks)))
             (cond
               ((and name deck)
                (if (> (string-length name) 7)
                    (vector-set! players i (new player% (name (substring name 0 7)) (deck (get-field decklist (find-deck deck)))))
                    (vector-set! players i (new player% (name name) (deck (get-field decklist (find-deck deck)))))))
               ((not name)
                (send view display-error "Wrong name")
                (loop (send view get-user-input (string-append "Player " (number->string (+ i 1)) " enter your name")) deck))
               ((not deck)
                (send view display-error "Wrong deck")
                (loop name (send view get-user-choice (string-append "Player " (number->string (+ i 1)) " choose your deck") decks)))))))
       ; Set up the players (names and decks)
       (set-up-players)
       ; Pick a random player to start
       (set! curr-player (random nr-of-players))
       ; Ask if this player wants to start of draw
       (let ((start? (send view get-user-yes/no "" (string-append (get-field name (get-player (+ curr-player 1)))
                                                                  ", would you like to start?"))))
         (cond ((eq? start? 'yes))
               ((eq? start? 'no) (if (= curr-player 0) (set! curr-player (- nr-of-players 1)) (set! curr-player (- curr-player 1))))))
       (set! priority curr-player)
       (start))
     
     ; ( -> )
     (define/public (start)
       ; ( player% -> )
       (define (mulligan? player)
         (let ((answer (send view get-user-yes/no "" (string-append "Would you like to take a mulligan " (get-field name player)))))
           (cond 
             ((or (eq? answer 'no) (= (get-field nr-of-mulligans player) 8)) )
             ((eq? answer 'yes) (notify 'clear-hand player)
                                (send player mulligan)
                                (notify 'initialize-hand player)
                                (notify 'update-library-size player)
                                (mulligan? player)))))
       
       (begin
         ; currently only 2 players
         (send view start-mtg (get-player 1) (get-player 2) turn this)
         (for-each-player (lambda (p) (send p start)))
         (for-each-player (lambda (p) 
                            (notify 'initialize-hand p)
                            (notify 'update-library-size p)))
         (mulligan? (active-player))
         (mulligan? (nonactive-player))
         (send view display-message (string-append (get-field name (active-player)) " may now start to play."))
         ; start the turn
         (start-turn)))
         
     ; ( -> )
     (define/public (end-game)
       (send view display-message "The end."))
     
     ; ( -> )
     (define/public (new-game)
       (begin (end-game)
              (set-up)))
     
     ; ( -> )
     (define/public (next)
       (begin 
         (send turn curr-step-end-actions this)
         (if (send turn last-step?)
           (begin 
             (send turn curr-phase-end-actions this)
             (send turn next)
             (notify 'update-phase) (notify 'update-step)
             (send turn curr-phase-start-actions this)
             (send turn curr-step-start-actions this))
           (begin 
             (notify 'update-step)
             (send turn next)
             (notify 'update-step)
             (send turn curr-step-start-actions this)))))     

     ; ( -> )
     (define/public (pass-priority)
       (cond
         ((and (= priorities-passed (- nr-of-players 1)) (send stack empty?))
          (next)
          (set! priorities-passed 0)
          (set! priority curr-player) (notify 'update-priority))          
         ((and (= priorities-passed (- nr-of-players 1)) (not (send stack empty?)))
          (resolve-top-of-stack)
          (set! priorities-passed 0)
          (set! priority curr-player) (notify 'update-priority))
         (else (if (= priority 0) (set! priority (+ priority 1)) (set! priority (+ priority 0)))
               (set! priorities-passed (+ priorities-passed 1))
               (notify 'update-priority))))
     
     ; ( -> )
     (define/public (pass-turn)
       (begin
         (if (= curr-player (- nr-of-players 1))
             (set! curr-player 0)
             (set! curr-player (+ curr-player 1)))
         (notify 'update-nr-of-turns)
         (notify 'update-active-player)))
     
     ; ( player% -> )
     (define/public (draw-card player)
       (if (send (get-field library player) empty?)
           (send player drew-from-empty-lib)
           (let ((card (send player draw-card)))
             (begin (notify 'add-card 'hand card player)
                    (notify 'update-library-size player)))))
     
     ; ( card% player% -> )
     (define/public (discard-card card player)
       ;(when (eq? player (get-field player-to-discard state))
       (send player discard-card card)
       (notify 'remove-card 'hand card player)
       (notify 'update-graveyard-size player))
     
     ; ( -> )
     (define/public (untap-permanents)
       (begin
         (notify 'untap-permanents (active-player))
         (send (active-player) untap-permanents)))
     
     ; ( -> )
     (define/public (start-turn)
       (let ((phase (send turn current-phase))
             (step (send turn current-step)))
         (send turn curr-phase-start-actions this)
         (for-each-player (lambda (p) (apply-static-abilities p)))
         (send (active-player) set-not-played-land)
         (remove-summoning-sickness)
         (send turn curr-step-start-actions this))) 
     
     ; ( -> )
     (define/public (remove-summoning-sickness)
       (send (active-player) remove-summoning-sickness))
     
     ; ( -> )
     (define/public (reset-mana-pools)
       (for-each-player (lambda (p)
                          (send p reset-mana)
                          (notify 'update-all-mana p)
                          (notify 'update-life p))))
           
     ; ( -> )
     (define/private (check-sbe)
       (let ((player1 (get-player 1))
             (player2 (get-player 2)))
         (cond
           ((or (= (get-field life player1) 0)
                (= (get-field life player2) 0))
            (end-game))
           ; check poison counters
           ((or (get-field drew-from-empty-lib? player1)
                (get-field drew-from-empty-lib? player2))
            (end-game))
           (else (let ((cleaned-up-p1 (send player1 clean-up))
                       (cleaned-up-p2 (send player2 clean-up)))
                   (for-each (lambda (c) (notify 'remove-card 'nonland-permanents c player1)) cleaned-up-p1)
                   (for-each (lambda (c) (notify 'remove-card 'nonland-permanents c player2)) cleaned-up-p2)
                   (for-each-player (lambda (p) (notify 'update-graveyard-size p))))))))
                  
     ; ( -> )
     (define/public (clean-up)
       (let ((a-p (active-player)))
         (define (discard-to-max-hand-size)
           (when (> (send (get-field hand a-p) size)
                    (get-field max-size (get-field hand a-p)))
             (discard-card a-p)
             (discard-to-max-hand-size)))
         (set! state clean-up-state)
         (if (> (send (get-field hand a-p) size)
                (get-field max-size (get-field hand a-p)))
             (send view display-message "Discard down to maximum hand size")
             (set! state able-to-play-spells/abi))
         (for-each-player (lambda (p) (send p remove-damage)))
         (for-each-player (lambda (p) (send p reset-cards)))
         (for-each-player (lambda (p) (apply-static-abilities p)))
         (send manager notify-observers 'end-of-Turn 'end-of-Turn)
         (check-sbe)))
         ; should return to start of clean-up if something was played
     
     ; ( card% player% -> )
     (define/public (discard-to-max-size card owner)
       (when (eq? owner (active-player))
         (discard-card card owner)
         (clean-up)))
   
     ; ( land% player% -> )
     (define/public (use-land land player)
       (unless (get-field tapped? land)
         ; tap the land and add the appropriate mana to the player's mana-pool
         (notify 'tap-permanent land player)
         (send land tap)         
         (let ((color (land->mana-type land)))
           (send (get-field mana-pool player) inc-mana 1 color)
           (notify 'update-mana color player))))

     ; ( land% -> symbol )
     (define/private (land->mana-type land)
       (case (get-field land-type land)
         ((plains) 'white)
         ((island) 'island)
         ((swamp) 'swamp)
         ((mountain) 'red)
         ((forest) 'green)
         (else 'colorless)))
;         ((member 'plains (get-field land-type land)) 'white)
;         ((member 'island (get-field land-type land)) 'blue)
;         ((member 'swamp (get-field land-type land)) 'black)
;         ((member 'mountain (get-field land-type land)) 'red)
;         ((member 'forest (get-field land-type land)) 'green)
;         (else 'colorless))) ;should use activated ability then)
     
     ; ********* PLAY SPELLS METHODS *************
     ; *******************************************
     
     ; ( -> stack-spell/abi% )
     (define/private (announced-spell/abi)
       (get-field announced-spell/abi stack))
     
     ; ( card% player% -> )
     (define/public (use-ability card owner)
       (unless (or (null? (get-field activated-abilities card)) (s:void? card))
         (if (not (= (length (get-field activated-abilities card)) 1))
             (do ((i 0 (+ i 1))
                  (choices '() (s:cons (number->string i) choices))
                  (l (length (get-field activated-abilities card))))
               ((= i l) (let ((choice (send view get-user-choice "Which ability do you want to use?" (s:reverse choices))))
                          (when choice
                            (send stack announce-spell/abi (list-ref (get-field activated-abilities card) (string->number choice)) owner state card)
                            (notify 'announce-spell (get-field announced-spell/abi stack))
                            (start-choose-mode)))))
             (begin 
               (send stack announce-spell/abi (car (get-field activated-abilities card)) owner state card)
               (notify 'announce-spell (get-field announced-spell/abi stack))
               (start-choose-mode)))))
     
     ; ( ability% player% -> )
     (define/public (put-ability-on-stack ability owner)
       (send stack put-ability-on-stack ability owner state))
     
     ; ( card% player% -> )
     (define/public (announce-spell spell player)
       (cond
         ((is-a? spell land%) (play-land spell player))
         ((is-a? spell instant%) (send stack announce-spell/abi spell player state)
                                 (notify 'announce-spell (get-field announced-spell/abi stack))
                                 (notify 'remove-card 'hand spell player)
                                 (start-choose-mode))
         (else (when (eq? player (get-player (+ priority 1)))
                 (send stack announce-spell/abi spell player state)
                 (notify 'announce-spell (get-field announced-spell/abi stack))
                 (notify 'remove-card 'hand spell player)
                 (start-choose-mode)))))
     
     ; ( -> )
     (define/public (cancel-announced-spell)
       (let ((announced-spell/abi (announced-spell/abi)))
         (send stack remove-announced)
         (notify 'update-all-mana (get-field owner announced-spell/abi))
         (notify 'add-card 'hand (get-field spell/abi announced-spell/abi) (get-field owner announced-spell/abi))
         (set! state (get-field state-played announced-spell/abi))))
     
     ; ( -> )
     (define/public (start-choose-mode)
       (if (or (is-a? (get-field spell/abi (announced-spell/abi)) ability%) 
               (is-a? (get-field spell/abi (announced-spell/abi)) non-permanent%))           
           (if (null? (get-field modes (get-field spell/abi (announced-spell/abi))))
               (start-choose-target)
               (begin (start-choose-target)));(set! state choose-mode) ;(notify 'choose-mode))))
           (start-choose-target)))
     
     ; ( -> )
     (define/public (start-choose-target)
       (if (or (is-a? (get-field spell/abi (announced-spell/abi)) ability%) 
               (is-a? (get-field spell/abi (announced-spell/abi)) non-permanent%)
               (is-a? (get-field spell/abi (announced-spell/abi)) aura%))
           (if (send (announced-spell/abi) targets-done?)
               (determine-total-cost)
               (begin (set! state choosing-spell/abi-target)
                      (notify 'update-announced-spell-state 'choose-target)))
           (determine-total-cost)))
     
     ; ( card% player% -> )
     (define/public (select-target target owner)
       (when (send (announced-spell/abi) check-target target owner)
         (send (announced-spell/abi) select-target target)
         (when (send (announced-spell/abi) targets-done?)
           (determine-total-cost))))
     
     ; ( -> )
     (define/public (determine-total-cost)
       (let ((mana-cost (get-field mc (announced-spell/abi))))
         (cond
           ((get-field x? mana-cost) (let loop ((x? (send view get-user-input "Choose a value for X")))
                                       (if x?
                                           (send (announced-spell/abi) set-x (string->number x?))
                                           (loop (send view get-user-input "Choose a value for X")))))) 
         (start-pay-mana)))
     
     ; ( -> )
     (define/public (start-pay-mana)
       (begin (set! state spending-mana)
              (notify 'update-announced-spell-state 'pay-mana)))
     
     ; ( land% player% -> )
     (define/public (play-land land player)
       (when (eq? player (active-player))
         (cond
           ((and (is-a? land land%) (not (get-field played-land? player)) (send stack empty?))
            (send player play-permanent land)
            (notify 'remove-card 'hand land player)
            (notify 'add-card 'lands land player)
            (for-each-player (lambda (p) (apply-static-abilities p))))
           ((and (is-a? land land%) (get-field played-land? player))
            (send view display-message "You already played a land this turn.")))))
     
     ; ( symbol player% -> )
     (define/private (update-mana color player)
       (send (get-field mana-pool player) dec-mana 1 color)
       (notify 'update-mana color player))
     
     ; ( symbol player% -> )
     (define/public (spend-mana color player)
       (when (and (eq? (get-field owner (announced-spell/abi)) player)
                  (not (= (send (get-field mana-pool player) get-mana color) 0)))
         (send (announced-spell/abi) pay-mana color)
         (update-mana color player)
         (notify 'update-announced-spell-mana)
         (when (send (announced-spell/abi) mana-payed?)
           (play-announced-spell))))
     
     ; ( -> )
     (define/public (play-announced-spell)
       (let ((spell/abi (get-field spell/abi (announced-spell/abi))))
         (cond
           ((eq? (get-field color spell/abi) 'green) (send manager notify-observers 'green-spell-played 'green-spell-played)))
         (notify 'add-stack-spell (announced-spell/abi))
         (notify 'announced-spell-done)
         (send stack lock-announced)
         (set! state able-to-play-spells/abi)))
     
     ; ( -> )
     (define/public (resolve-top-of-stack)
       (let* ((stack-spell/abi (send stack resolve-top)) ; this is a stack-spell/abi%
              (spell/abi (get-field spell/abi stack-spell/abi))
              (owner (get-field owner stack-spell/abi)))
         (if (is-a? spell/abi card%)
             (cond
               ((is-a? spell/abi permanent%)
                (resolve-permanent stack-spell/abi))
               ((is-a? spell/abi non-permanent%)
                (resolve-spell stack-spell/abi)))
             (resolve-ability stack-spell/abi))
         (set! state (get-field state-played stack-spell/abi))))
     
     ; ( stack-spell/abi% -> )
     (define/private (resolve-permanent stack-spell)
       (let ((spell (get-field spell/abi stack-spell))
             (owner (get-field owner stack-spell)))
         (if (is-a? spell aura%)
             (send spell set-target (car (get-field targets stack-spell))))
         (send owner play-permanent spell)
         (notify 'remove-card 'hand spell owner)
         (notify 'add-card 'nonland-permanents spell owner)                
         (notify 'remove-top-of-stack)
         (let ((cip (send spell has-cip-ability?))) ; comes into play triggered-ability?
           (if cip
               ((get-field effect cip) this owner 'CIP)))
         (for-each-player (lambda (p) (apply-static-abilities p)))
         (send spell initialize manager)))
     
     ; ( stack-spell/abi% -> )
     (define/private (resolve-spell stack-spell)
       (let ((spell (get-field spell/abi stack-spell))
             (owner (get-field owner stack-spell))
             (targets (get-field targets stack-spell)))
         (send owner play-spell spell)
         ((get-field effect spell) this owner spell (if (= (length targets) 1)
                                                        (car targets)
                                                        targets))
         (notify 'remove-top-of-stack)
         (notify 'update-graveyard-size owner)))
     
     ; ( stack-spell/abi% -> )
     (define/private(resolve-ability stack-ability)
       (let ((ability (get-field spell/abi stack-ability))
             (owner (get-field owner stack-ability))
             (targets (get-field targets stack-ability)))
         ((get-field effect ability) this owner (get-field card stack-ability) (if (= (length targets) 1)
                                                                                   (car targets)
                                                                                   targets))
         (notify 'remove-top-of-stack)))

     ; ************** CARD EFFECTS ***************
     ; *******************************************
     
     ; ( number/'all ( card% -> bool ) ( mtg% player% card% -> ) -> )
     (define/public (select-card-from-library player top-x condition end-action)
       (let loop ((choice (send view choice-from-library player top-x)))
         (if (condition choice)
             (end-action this player choice)
             (loop (send view choice-from-library player top-x)))))
     
     ; ( player% -> )
     (define/public (apply-static-abilities player)
       (send player apply-static-abilities this))
           
     
     ; ********* COMBAT PHASE METHODS ************
     ; *******************************************
     
     ; ( -> )
     (define/private (skip-combat-phase)
       (do ()
         ((eq? 'Main-phase2 (get-field name (send turn current-phase))))
         (next)))
     
     ; ( -> )
     (define/private (start-combat-phase)
       (next))
     
     ; ( -> )
     (define/public (start-combat-phase?)
       (if (= 2 (send view get-user-answer "Skip combat?" "Would you like to skip combat this turn?" "No" "Skip" #f))
           (skip-combat-phase)
           (start-combat-phase)))
     
     ; ( creature% player% -> )
     (define/public (declare-attacker creature owner)
       (when (eq? owner (active-player))
         (unless (or (get-field summoning-sickness? creature)
                     (get-field tapped? creature))
           (send red-zone add-attacking creature)
           (unless (send creature has-keyword-ability? 'vigilance)
             (notify 'tap-permanent creature owner)
             (send creature tap))
           (notify 'set-attacking creature owner))))
     
     (define blocker 0)
     
     ; ( creature% player% -> )
     (define/public (declare-blocker creature owner)
       (when (and (eq? owner (nonactive-player)) (not (get-field tapped? creature)))
         (set! blocker creature)
         (set! state declare-blocker-target-state)))
     
     ; ( creature% player% -> )
     (define/public (declare-blocker-target creature owner)
       (when (eq? owner (active-player))
         (send red-zone add-blocking creature blocker)
         (notify 'set-blocking blocker (nonactive-player))))
         
     ; ( -> )
     (define/public (end-declare-attackers)
       (set! state able-to-play-spells/abi))
     
     ; ( -> )
     (define/public (end-declare-blockers)
       (set! state able-to-play-spells/abi))
     
     (define assign-damage-from 0)
     
     ; ( creature% player% -> )
     (define/public (assign-damage creature owner)
       (set! assign-damage-from (list creature owner))
       (set! state assigning-damage-to))
     
     ; ( creature% player% -> )
     (define/public (assign-damage-to creature owner)
       (unless (eq? owner (cadr assign-damage-from))
         (let* ((amt (send view get-user-input "Amount of damage"))
                (amount (if amt (string->number amt) #f)))
           (when amount
             (if (> amount (send (car assign-damage-from) power))
                 (send view display-message "Amount is too high")
                 (if (eq? owner (active-player))
                     (if (send red-zone assign-damage-to-attacker (car assign-damage-from) creature amount)
                         (set! state assigning-damage)
                         (send view display-message "Wrong target"))
                     (if (send red-zone assign-damage-to-blocker (car assign-damage-from) creature amount)
                         (set! state assigning-damage)
                         (send view display-message "Wrong target"))))))))
     
     ; ( -> )
     (define/public (end-assign-damage)
       (set! state able-to-play-spells/abi))
     
     ; ( -> )
     (define/public (resolve-damage)
       (let ((unblocked-damage (send red-zone unblocked-damage)))
         (send (nonactive-player) dec-life unblocked-damage)
         (notify 'update-life (nonactive-player))
         (check-sbe)))
     
     ; ( -> )
     (define/public (end-combat)
       (send red-zone for-each-attacker
             (lambda (c) (notify 'remove-attacking c (active-player))))
       (send red-zone for-each-blocker
             (lambda (c) (notify 'remove-blocking c (nonactive-player))))
       (send red-zone reset))
     
     ;**********************************************
     
     ; ( -> )
     (define/public (do-nothing)
       'nothing)
      
     ; ( symbol pair -> )
     (define/public (get-update update-name . args)
       (case update-name
         ((select-nonlandpermanent) (send state select-nonlandpermanent (s:car args) (s:cadr args)))
         ((select-land) (send state select-land (s:car args) (s:cadr args)))
         ((select-graveyard-card) (send state select-graveyard-card (s:car args) (s:cadr args)))
         ((select-rfg-card) (send state select-rfg-card (s:car args) (s:cadr args)))
         ((select-hand-card) (send state select-hand-card (s:car args) (s:cadr args)))
         ((select-mana) (send state select-mana (s:car args) (s:cadr args)))
         ((select-player) (send state select-player (s:car args)))
         ((pass-priority) (send state pass-priority))))
         
     ; ( symbol -> )
     (define/public (set-state! s)
       (case s
         ((able-to-play-spells/abi) (set! state able-to-play-spells/abi))
         ((not-able-to-play-spells/abi) (set! state not-able-to-play-spells/abi))
         ((main-phase) (set! state main-phase))
         ((declaring-attackers) (set! state declaring-attackers))
         ((declaring-blockers) (set! state declaring-blockers))
         ((assigning-damage) (set! state assigning-damage))
         (else (error "MTG -- set-state! unknown state" state)))))))
