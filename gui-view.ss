#|
File : graphical view
Author : Wouter Van Rossem 
|#

#!r6rs

(library
 (gui-view)
 (export gui-view%)
 (import (rnrs base (6))
         (rnrs io simple)
         (rnrs control (6))
         (scheme class)
         (scheme gui base)
         (rnrs mutable-pairs)
         (prefix (only (scheme base) list cons null? null car cdr list-ref) s:)
         (magicthegathering gui)
         (magicthegathering controller)
         (magicthegathering observer-pattern))
 
 ; Interface name: view     
 ; Methods:
 ;  - public: 
 ;     * display-message: 
 ;         arguments: message
 ;         output : none
 ;         description: display a message to the user
 ;     * get-user-input: 
 ;         arguments: label
 ;         output : none
 ;         description: ask for input for a certain question (label)
 ;     * get-user-choice: 
 ;         arguments: label, choice
 ;         output: unspecified
 ;         description: get a choice from a list of choices from the user
 ;     * get-user-yes/no: 
 ;         arguments: title, message
 ;         output : 'yes/'no
 ;         description: get a user response to a certain question
 ;     * get-user-answer: 
 ;         arguments: title, message, button1, button2, button3
 ;         output : none
 ;         description: get a choice between maximum 3 options from the user
 ;     * choice-from-library: 
 ;         arguments: player, topx
 ;         output : card%
 ;         description: get a choice from the top x cards of target player's library
 ;     * display-error: 
 ;         arguments: error-message
 ;         output : none
 ;         description: display an error-message to the user
 ;  - private: none
 
 (define view<%> (interface () display-message get-user-input get-user-choice get-user-yes/no display-error
                   get-user-answer choice-from-library))
 
 ; Class name: gui-view
 ; Superclass: subject%
 ; Description: Implements the view interface + handles model updates
 ; Attributes: - public: none
 ;             - private: view, controller
 ; Methods:
 ;  - public: 
 ;     * start-mtg: 
 ;         arguments: player1, player2, turn, mtg
 ;         output : none
 ;         description: start the gui view for mtg
 ;     * initialize-hand: 
 ;         arguments: player
 ;         output : none
 ;         description: initialize the hand-panel of a player
 ;     * clear-hand: 
 ;         arguments: player
 ;         output: none
 ;         description: clears the hand-panel of a player
 ;     * update-library/gy/rfg-size 
 ;         arguments: player
 ;         output : none
 ;         description: update the size of a cardstack-panel
 ;     * update-life: 
 ;         arguments: player
 ;         output : none
 ;         description: update the life-panel of a player
 ;     * update-phase/step: 
 ;         arguments: none
 ;         output : none
 ;         description: update the phase or step panel
 ;     * update-priority/active-player/nr-of-turns: 
 ;         arguments: none
 ;         output : none
 ;         description: update the "who has priority"/"active-player"/"nr-of-turns" message
 ;     * add-card: 
 ;         arguments: zone, card, player
 ;         output : none
 ;         description: add a card-canvas to a zone of a player
 ;     * remove-card: 
 ;         arguments: zone, card, player
 ;         output : none
 ;         description: remove a card-canvas from a zone of a player
 ;     * tap-permanent: 
 ;         arguments: permanent, player
 ;         output : none
 ;         description: tap a cardcanvas of a player
 ;     * untap-permanents: 
 ;         arguments: player
 ;         output : none
 ;         description: untap all card-canvases a player
 ;     * update-mana: 
 ;         arguments: color, player
 ;         output : none
 ;         description: update the mana of a color of a player
 ;     * update-all-mana: 
 ;         arguments: player
 ;         output : none
 ;         description: update the whole mana-pool panel of a player
 ;     * announce-spell: 
 ;         arguments: zone, card, player
 ;         output : none
 ;         description: send the announce spell frame to start
 ;     * update-announced-spell-mana: 
 ;         arguments: none
 ;         output : none
 ;         description: update the mana to spend from the announce spell frame
 ;     * update-announced-spell-state: 
 ;         arguments: state
 ;         output : none
 ;         description: change the state of the announce spell frame
 ;     * announced-spell-done: 
 ;         arguments: none
 ;         output : none
 ;         description: send the announce spell frame it is done
 ;     * add-stack-spell: 
 ;         arguments: stack-spell
 ;         output : none
 ;         description: add a new spell to the stack frame
 ;     * remove-top-of-stack: 
 ;         arguments: none
 ;         output : none
 ;         description: remove the top of the stack frame
 ;     * set-attacking/blocking: 
 ;         arguments: creature, owner
 ;         output : none
 ;         description: set a cardcanvas to attack or block
 ;     * remove-attacking/blocking: 
 ;         arguments: creature, owner
 ;         output : none
 ;         description: set a cardcanvas to stop attacking or blocking
 ;  - private: none
 
 (define gui-view%
   (class* subject% (view<%>)
     (super-new (name 'gui-view))
     (inherit attach detach notify)
     (define view (new frame% (label "Main") (width 500) (height 200)))
     (define controller 0)
     
     (send view show #t)
     
     ; ( player% player% turn% mtg% -> )
     (define/public (start-mtg player1 player2 turn mtg)
       (send view show #f)
       (set! controller
             (new controller% (model mtg) (view this)))
       (set! view 
             (new mtg-frame% (player1 player1) (player2 player2) (turn turn) (controller controller) (mtg mtg)))       
       (send view show #t))
     
     ; ( string -> )
     (define/public (display-message message)
       (message-box "" message))
     
     ; ( string -> string )
     (define/public (get-user-input label)
       (get-text-from-user label label view))
     
     ; ( string pair -> unspecified)
     (define/public (get-user-choice label choices)
       (let ((choice (get-choices-from-user label label choices view)))
         (if choice 
             (s:list-ref choices (s:car choice))
             #f)))
     
     ; ( string string -> symbol )
     (define/public (get-user-yes/no title message)
       (message-box title message view (s:list 'yes-no)))
     
     ; ( string string string string string -> symbol )
     (define/public (get-user-answer title message button1 button2 button3)
       (message-box/custom title message button1 button2 button3 view))
     
     ; ( string -> )
     (define/public (display-error error-message)
       (bell)
       (message-box "Error" error-message))
     
     ; ( player% number -> card% )
     (define/public (choice-from-library player top-x)
       (send view choice-from-library player top-x))
     
     ; ********** MODEL UPDATES ***************
     
     ; ( player% -> )
     (define/public (initialize-hand player)
       (send view initialize-hand player))
     
     ; ( player% -> )
     (define/public (clear-hand player)
       (send view clear-hand player))
     
     ; ( player% -> )
     (define/public (update-library-size player)
       (send view update-library-size player))
     
     ; ( player% -> )
     (define/public (update-graveyard-size player)
       (send view update-graveyard-size player))
     
     ; ( player% -> )
     (define/public (update-rfg-size player)
       (send view update-rfg-size player))
     
     ; ( player% -> )
     (define/public (update-life player)
       (send view update-life player))
     
     ; ( -> )
     (define/public (update-phase)
       (send view update-phase))
     
     ; ( -> )
     (define/public (update-step)
       (send view update-step))
     
     ; ( -> )
     (define/public (update-priority)
       (send view update-priority))
     
     ; ( -> )
     (define/public (update-active-player)
       (send view update-active-player))
     
     ; ( -> )
     (define/public (update-nr-of-turns)
       (send view update-nr-of-turns))
     
     ; ( symbol card% player% -> )
     (define/public (add-card zone card player)
       (send view add-card zone card player))
     
     ; ( symbol card% player% -> )
     (define/public (remove-card zone card player)
       (send view remove-card zone card player))
     
     ; ( permanent% player% -> )
     (define/public (tap-permanent permanent player)
       (send view tap-permanent permanent player))
     
     ; ( player% -> )
     (define/public (untap-permanents player)
       (send view untap-permanents player))
     
     ; ( symbol player% -> )
     (define/public (update-mana color player)
       (send view update-mana color player))
     
     ; ( player% -> )
     (define/public (update-all-mana player)
       (send view update-all-mana player))
     
     ; ( stack-spell/abi% -> )
     (define/public (announce-spell stack-spell)
       (send view announce-spell stack-spell))
     
     ; ( -> )
     (define/public (update-announced-spell-mana)
       (send view update-announced-spell-mana))
     
     ; ( symbol -> )
     (define/public (update-announced-spell-state state)
       (send view update-announced-spell-state state))
   
     ; ( -> )
     (define/public (announced-spell-done)
       (send view announced-spell-done))
     
     ; ( stack-spell/abi% -> )
     (define/public (add-stack-spell stack-spell)
       (send view add-stack-spell stack-spell))
    
     ; ( -> )
     (define/public (remove-top-of-stack)
       (send view remove-top-of-stack))
     
     ; ( creature% player% -> )
     (define/public (set-attacking creature owner)
       (send view set-attacking creature owner))
     
     ; ( creature% player% -> )
     (define/public (remove-attacking creature owner)
       (send view remove-attacking creature owner))
     
     ; ( creature% player% -> )
     (define/public (set-blocking creature owner)
       (send view set-blocking creature owner))
     
     ; ( creature% player% -> )
     (define/public (remove-blocking creature owner)
       (send view remove-blocking creature owner)))))
