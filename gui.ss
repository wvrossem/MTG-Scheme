#|
File : gui
Author : Wouter Van Rossem 
|#

#!r6rs

(library
 (gui)
 (export  mtg-frame%)
 (import (rnrs base (6))
         (rnrs io simple)
         (rnrs control)
         (scheme class)
         (scheme gui base)         
         (prefix (scheme base) s:)
         (only (mzlib file) build-absolute-path)
         (only (scheme base) current-directory make-bytes for bytes-copy! in-range)
         (magicthegathering mana-cost)
         (magicthegathering permanents)
         (magicthegathering abilities)
         (prefix (magicthegathering datastructures positional-list adt) pl:))
 
 ; Source: ../plt/collects/games/cards/card-class
 ; Rotate a bitmap: if cw? = #t -> clockwise
 ;                         = #f -> counterclockwise
 (define (rotate-bm bm cw?)
    (let ([w (send bm get-width)]
          [h (send bm get-height)])
      (let ([bm2 (make-object bitmap% h w)]
            [s (make-bytes (* w h 4))]
            [s2 (make-bytes (* h w 4))])
        (send bm get-argb-pixels 0 0 w h s)
        (for ([i (in-range w)])
          (for ([j (in-range h)])
            (let ([src-pos (* (+ i (* j w)) 4)])
              (bytes-copy! s2 
                           (if cw?
                               (* (+ (- (- h j) 1) (* i h)) 4) 
                               (* (+ j (* (- (- w i) 1) h)) 4))
                           s src-pos (+ src-pos 4)))))
        (let ([dc (make-object bitmap-dc% bm2)])
          (send dc set-argb-pixels 0 0 h w s2)
          (send dc set-bitmap #f))
        bm2)))
 
 ; Class name: card-canvas
 ; Superclass: panel%
 ; Description: A card canvas to display a card. Has a private attribute image 
 ;    with the bitmap image of the card. This image is found by building a path tot the image.
 ; Attributes: - public: card, owner, zone, controller, display-panel, card-info-panel
 ;             - private: image, small-image, canvas        
 ; Methods:
 ;  - public: 
 ;     * (un)tap: 
 ;         arguments: none
 ;         output: none
 ;         description: rotate the card to represent it being (un)tapped
 ;     * set-attacking/blocking:  
 ;         arguments: none
 ;         output: bool
 ;         description: set the canvas background to represent attacking/blocking
 ;     * remove-attacking/blocking:  
 ;         arguments: none
 ;         output: bool
 ;         description: reset the canvas background
 
 (define card-canvas%
   (class panel%
     (init-field card)
     (init-field owner)
     (init-field zone)
     (init-field controller)    
     (init-field display-panel)
     (init-field card-info-panel)
     (define image (make-object bitmap% (build-absolute-path (current-directory) "pictures" (get-field expansion card)
                                                             (string-append (get-field name card) ".jpg"))))
     (define small-image (make-object bitmap% (build-absolute-path (current-directory) "pictures" (get-field expansion card)
                                                             "small" (string-append (get-field name card) ".jpg"))))
     
     (super-new)
     
     (define canvas (new (class canvas%
                           (super-new)
                           (define/override (on-event event)
                             (cond
                               ((send event entering?) (begin (send display-panel set-image! image)
                                                              (send card-info-panel display-card-info card)))
                               ((send event button-down?) (send controller update 'select-card card zone owner)))))
                         (parent this)
                         (paint-callback (lambda (c dc) (send dc draw-bitmap small-image 0 0)))
                         (min-width 100)
                         (min-height 100)))
     
     (send canvas refresh)
     
     ; ( -> )
     (define/public (tap)
       (unless (get-field tapped? card)
         (set! small-image (rotate-bm small-image #t))
         (send (send canvas get-dc) clear)
         (send (send canvas get-dc) draw-bitmap small-image 0 0)))
     
     ; ( -> )
     (define/public (untap)
       (when (get-field tapped? card)
         (set! small-image (rotate-bm small-image #f))
         (send (send canvas get-dc) clear)
         (send (send canvas get-dc) draw-bitmap small-image 0 0)))
     
     ; ( -> )
     (define/public (set-attacking)
       (send canvas set-canvas-background (make-object color% "Crimson"))
       (send canvas refresh))
     
     ; ( -> )
     (define/public (remove-attacking)
       (send canvas set-canvas-background (make-object color% "White"))
       (send canvas refresh))
     
     ; ( -> )
     (define/public (set-blocking)
       (send canvas set-canvas-background (make-object color% "SteelBlue"))
       (send canvas refresh))
     
     ; ( -> )
     (define/public (remove-blocking)
       (send canvas set-canvas-background (make-object color% "White"))
       (send canvas refresh))))
 
 ; Class name: mana-pool-panel
 ; Superclass: vertical-panel%
 ; Description: A mana-pool panel consists of different panels, one for each color.
 ;    Each panel has a picture and a message with the current mana of that color in the mana-pool
 ; Attributes: - public: mana-pool, owner, controller
 ;             - private: for each color color+image+canvas        
 ; Methods:
 ;  - public: 
 ;     * update: 
 ;         arguments: color
 ;         output: none
 ;         description: update the panel of a certain color, i.e. set the label of the message
 ;     * update-all:  
 ;         arguments: none
 ;         output: none
 ;         description: update all mana panels
 
 (define mana-pool-panel%
   (class vertical-panel%
     (init-field mana-pool)
     (init-field owner)
     (init-field controller)
     (super-new)
     ;(super-new (min-width 25) (min-height 125))
     (define panel (new vertical-panel% (parent this) (min-width 25) (min-height 125)))
     
     (define white (new pane% (parent panel) (min-width 25) (min-height 25)))
     (define white-image (make-object bitmap% (build-absolute-path (current-directory) "pictures" "mana-symbols" "ManaSymbols-white.gif")))
     (define white-canvas (new (class canvas% 
                                 (super-new (parent white) (min-width 25) (min-height 25)
                                            (paint-callback (lambda (c dc) (send dc set-background (make-object color% 0 0 0)) (send dc draw-bitmap white-image 0 0))))
                                 (define/override (on-event event)
                                   (cond
                                     ((send event button-down?) (send controller update 'select-mana 'white owner)))))))
     (define white-mana (new message% (parent white) (label (number->string (get-field white mana-pool))) (horiz-margin 10) (vert-margin 6)))
     
     (define blue (new pane% (parent panel) (min-width 25) (min-height 25)))
     (define blue-image (make-object bitmap% (build-absolute-path (current-directory) "pictures" "mana-symbols" "ManaSymbols-blue.gif")))
     (define blue-canvas (new (class canvas% 
                                 (super-new (parent blue) (min-width 25) (min-height 25)
                                            (paint-callback (lambda (c dc) (send dc set-background (make-object color% 0 0 0)) (send dc draw-bitmap blue-image 0 0))))
                                 (define/override (on-event event)
                                   (cond
                                     ((send event button-down?) (send controller update 'select-mana 'blue owner)))))))
     (define blue-mana (new message% (parent blue) (label (number->string (get-field blue mana-pool))) (horiz-margin 10) (vert-margin 6)))

     (define black (new pane% (parent panel) (min-width 25) (min-height 25)))
     (define black-image (make-object bitmap% (build-absolute-path (current-directory) "pictures" "mana-symbols" "ManaSymbols-black.gif")))
     (define black-canvas (new (class canvas% 
                                 (super-new (parent black) (min-width 25) (min-height 25)
                                            (paint-callback (lambda (c dc) (send dc set-background (make-object color% 0 0 0)) (send dc draw-bitmap black-image 0 0))))
                                 (define/override (on-event event)
                                   (cond
                                     ((send event button-down?) (send controller update 'select-mana 'black owner)))))))
     (define black-mana (new message% (parent black) (label (number->string (get-field black mana-pool))) (horiz-margin 10) (vert-margin 6)))
     
     (define red (new pane% (parent panel) (min-width 25) (min-height 25)))
     (define red-image (make-object bitmap% (build-absolute-path (current-directory) "pictures" "mana-symbols" "ManaSymbols-red.gif")))
     (define red-canvas (new (class canvas% 
                                 (super-new (parent red) (min-width 25) (min-height 25)
                                            (paint-callback (lambda (c dc) (send dc set-background (make-object color% 0 0 0)) (send dc draw-bitmap red-image 0 0))))
                                 (define/override (on-event event)
                                   (cond
                                     ((send event button-down?) (send controller update 'select-mana 'red owner)))))))
     (define red-mana (new message% (parent red) (label (number->string (get-field red mana-pool))) (horiz-margin 10) (vert-margin 6)))
     
     (define green (new pane% (parent panel) (min-width 25) (min-height 25)))
     (define green-image (make-object bitmap% (build-absolute-path (current-directory) "pictures" "mana-symbols" "ManaSymbols-green.gif")))
     (define green-canvas (new (class canvas% 
                                 (super-new (parent green) (min-width 25) (min-height 25)
                                            (paint-callback (lambda (c dc) (send dc set-background (make-object color% 0 0 0)) (send dc draw-bitmap green-image 0 0))))
                                 (define/override (on-event event)
                                   (cond
                                     ((send event button-down?) (send controller update 'select-mana 'green owner)))))))
     (define green-mana (new message% (parent green) (label (number->string (get-field green mana-pool))) (horiz-margin 10) (vert-margin 6)))
     
     (define colorless (new pane% (parent panel) (min-width 25) (min-height 25)))
     (define colorless-image (make-object bitmap% (build-absolute-path (current-directory) "pictures" "mana-symbols" "ManaSymbols-white.gif")))
     (define colorless-canvas (new (class canvas% 
                                 (super-new (parent colorless) (min-width 25) (min-height 25)
                                            (paint-callback (lambda (c dc) (send dc set-background (make-object color% 0 0 0)) (send dc draw-bitmap colorless-image 0 0))))
                                 (define/override (on-event event)
                                   (cond
                                     ((send event button-down?) (send controller update 'select-mana 'colorless owner)))))))
     (define colorless-mana (new message% (parent colorless) (label (number->string (get-field colorless mana-pool))) (horiz-margin 10) (vert-margin 6)))
     
     ; ( symbol -> )
     (define/public (update color)
       (case color
         ((white) (send white-mana set-label (number->string (get-field blue mana-pool))))
         ((blue) (send blue-mana set-label (number->string (get-field blue mana-pool))))
         ((black) (send black-mana set-label (number->string (get-field black mana-pool))))
         ((red) (send red-mana set-label (number->string (get-field red mana-pool))))
         ((green) (send green-mana set-label (number->string (get-field green mana-pool))))
         ((colorless) (send colorless-mana set-label (number->string (get-field colorless mana-pool))))))
     
     ; ( -> )
     (define/public (update-all)
       (begin
         (update 'white)
         (update 'blue)
         (update 'black)
         (update 'red)
         (update 'green)
         (update 'colorless)))
     
     (update-all)))
 
 ; Class name: life-panel
 ; Superclass: panel%
 ; Description: A life panel is a simple panel with an image and a message with the current life of the player
 ; Attributes: - public: player, controller 
 ;             - private: font, panel, life-picture, life-canvas, life        
 ; Methods:
 ;  - public: 
 ;     * update: 
 ;         arguments: none
 ;         output: none
 ;         description: update the life message

 (define life-panel%
   (class panel%
     (init-field player)
     (init-field controller)
     (super-new (alignment (s:list 'center 'center)))
     (define font (make-object font% 15 'decorative))
     (define panel (new panel% (parent this) (min-width 32) (min-height 32) (style (s:list 'border)) (border 1)))
     (define life-picture (make-object bitmap%
                            (build-absolute-path (current-directory) "pictures" "life.png")))          
     (define life-canvas (new (class canvas%
                                (super-new (parent panel)
                                           (min-width 32) (min-height 32)
                                           (paint-callback
                                            (lambda (c dc) (send dc draw-bitmap life-picture 0 0))))
                                (define/override (on-event event)
                                  (cond
                                    ((send event button-down?) (send controller update 'select-player player)))))))    
     (define life (new message% (parent this) (font font) (label (number->string (get-field life player))) (horiz-margin 12) (vert-margin 8)))
     
     ; ( -> )
     (define/public (update)
       (send life set-label (number->string (get-field life player))))))
 
 ; Class name: cardstack-panel
 ; Superclass: panel%
 ; Description: A cardstack panel is panel that displays the number of cards in the cardstack
 ;    When the canvas is pressed, the user can choose between the cards in the canvas
 ; Attributes: - public: cardstack, zone-name, owner, image, controller  
 ;             - private: canvas        
 ; Methods:
 ;  - public: 
 ;     * choice-from-library: 
 ;         arguments: topx
 ;         output: card%
 ;         description: open a selection window for all the cards or the top x cards of the cardstack
 ;     * update-size: 
 ;         arguments: none
 ;         output: none
 ;         description: update the size message
 ;  - private:
 ;     * select-window:
 ;         arguments: none
 ;         output: card%
 ;         description: open a selection window with all the cars in the cardstack
 
 (define cardstack-panel%
   (class panel%
     (init-field cardstack)
     (init-field zone-name)
     (init-field owner)
     (init-field image)
     (init-field controller)
     (super-new)
     (define canvas 
       (new (class canvas% 
              (super-new (paint-callback (lambda (c dc) (send dc draw-bitmap image 0 0)))
                         (min-width 72) (min-height 100))
              (define/override (on-event event)
                (cond
                  ((send event button-down?) (when (get-field searchable? cardstack)
                                               (send controller update 'select-card (select-window) zone-name owner))))))
            (parent this)))
     
     ; ( -> card% )
     (define/private (select-window)
       (let ((choices '())
             (cards '()))
         (begin (send cardstack for-each (lambda (c) (set! choices (s:cons (get-field name c) choices))
                                           (set! cards (cons c cards))))
                (set! choices (s:reverse choices))
                (set! cards (reverse cards))
                (let ((choice (get-choices-from-user (symbol->string zone-name) "" choices)))
                  (when choice
                    (list-ref cards (s:car choice)))))))
     
     ; ( number/'all -> card% )
     (define/public (choice-from-library top-x)
       (if (eq? top-x 'all)
           (select-window)
           (let ((choices '())
                 (cards '())
                 (counter 0))
             (send cardstack for-each (lambda (c) (when (<= counter top-x)
                                                    (set! choices (s:cons (get-field name c) choices))
                                                    (set! cards (cons c cards))
                                                    (set! counter (+ counter 1)))))
             (set! choices (s:reverse choices))
             (set! cards (reverse cards))
             (let ((choice (get-choices-from-user (symbol->string zone-name) "" choices)))
               (when choice
                 (list-ref cards (s:car choice)))))))
     
     ; ( -> )
     (define/public (update-size)
       (new message% (parent this) (label (number->string (send cardstack size))) (vert-margin 40) (horiz-margin 35)))
     
     ; Set the initial message
     (update-size)))
 
 ; Class name: hand-panel
 ; Superclass: panel%
 ; Description: A hand-panel is a panel that displays card-canvases, these are the cards in the player's hand
 ;    This also keeps track of its children for easy deletion.
 ; Attributes: - public: hand, owner, controller 
 ;             - private: children        
 ; Methods:
 ;  - public: 
 ;     * add-cardcanvas: 
 ;         arguments: card
 ;         output: none
 ;         description: add a new card-canvas to the hand-panel
 ;     * remove-cardcanvas: 
 ;         arguments: card
 ;         output: none
 ;         description: remove a cardcanvas from the hand-panel
 ;     * initialize: 
 ;         arguments: display-panel, card-info
 ;         output: none
 ;         description: make a new card-canvas for each card in the hand
 ;     * clear: 
 ;         arguments: none
 ;         output: none
 ;         description: remove a all the card-canvases from the hand-panel
 ;  - private: none
 
 (define hand-panel%
   (class horizontal-panel%
     (init-field hand)
     (init-field owner)
     (init-field controller)
     (super-new (alignment (s:list 'center 'center)) (style (s:list 'border)) (border 1))
     (define children (pl:new (lambda (c cc) (or (eq? c (get-field card cc)) (eq? c cc)))))
     
     ; ( card% card-preview-panel% card-info-panel% -> )
     (define/public (add-cardcanvas card display-panel card-info-panel)
       (pl:add-before! children
                       (new card-canvas% (card card) (parent this) (owner owner) (controller controller) 
                            (zone 'hand) (display-panel display-panel) (card-info-panel card-info-panel))))
     
     ; ( card% -> )
     (define/public (remove-cardcanvas card)
       (define pos (pl:find children card))
       (define child (if pos (pl:peek children pos) #f))
       (when child 
         (send this delete-child child)
         (pl:delete! children pos)))
     
     ; ( card-preview-panel% card-info-panel% -> )
     (define/public (initialize display-panel card-info-panel)
       (send hand for-each (lambda (c)
                             (add-cardcanvas c display-panel card-info-panel))))
     
     ; ( -> )
     (define/public (clear)
       (send hand for-each (lambda (c) (remove-cardcanvas c))))))
 
 ; Class name: zone-panel
 ; Superclass: panel%
 ; Description: A zone-panel is a panel that displays card-canvases, these are the cards in the zone
 ;    This also keeps track of its children for easy deletion.
 ; Attributes: - public: zone, zone-name, owner, controller 
 ;             - private: children        
 ; Methods:
 ;  - public: 
 ;     * add-cardcanvas: 
 ;         arguments: card
 ;         output: none
 ;         description: add a new card-canvas to the zone-panel
 ;     * remove-cardcanvas: 
 ;         arguments: card
 ;         output: none
 ;         description: remove a cardcanvas from the zone-panel
 ;     * initialize: 
 ;         arguments: display-panel, card-info
 ;         output: none
 ;         description: make a new card-canvas for each card in the zone
 ;     * clear: 
 ;         arguments: none
 ;         output: none
 ;         description: remove a all the card-canvases from the zone-panel
 ;     * tap-permanent: 
 ;         arguments: permanent
 ;         output: none
 ;         description: find a card-canvas and tap it
 ;     * untap-all: 
 ;         arguments: none
 ;         output: none
 ;         description: untap all card-canvases
 ;     * set-attacking/blocking: 
 ;         arguments: creature
 ;         output: none
 ;         description: find a card-canvas and set it to attacking/blocking
 ;     * remove-attacking/blocking: 
 ;         arguments: creature
 ;         output: none
 ;         description: find a card-canvas and remove attacking/blocking
 ;  - private: none
 
 (define zone-canvas%
   (class horizontal-panel%
     (init-field zone)
     (init-field zone-name)
     (init-field owner)
     (init-field controller)
     (super-new (min-width 800) (min-height 100) (style  (s:list 'border)) (border 1))
     (define children (pl:new (lambda (c cc) (or (eq? c (get-field card cc)) (eq? c cc)))))
     
     ; ( card% card-preview-panel% card-info-panel% -> )
     (define/public (add-cardcanvas card display-panel card-info-panel)
       (pl:add-before! children
                       (new card-canvas% (card card) (parent this) (owner owner) (controller controller) 
                            (zone zone-name) (display-panel display-panel) (card-info-panel card-info-panel))))
     
     ; ( card% -> )
     (define/public (remove-cardcanvas card)
       (define pos (pl:find children card))
       (define child (if pos (pl:peek children pos) #f))
       (when child 
         (send this delete-child child)
         (pl:delete! children pos)))
     
     ; ( card-preview-panel% card-info-panel% -> )
     (define/public (initialize display-panel card-info-panel)
       (send zone for-each (lambda (c)
                             (add-cardcanvas c display-panel card-info-panel))))
     
     ; ( -> )
     (define/public (clear)
       (send zone for-each (lambda (cc) 
                             (send this delete-child cc)
                             (pl:delete! children (pl:find children cc)))))
     
     ; ( permanent% -> )
     (define/public (tap-permanent permanent)
       (let* ((pos (pl:find children permanent))
              (cardcanvas (if pos (pl:peek children pos) #f)))
         (when cardcanvas
           (send cardcanvas tap))))
     
     ; ( creature% -> )
     (define/public (set-attacking creature)
       (let* ((pos (pl:find children creature))
              (cardcanvas (if pos (pl:peek children pos) #f)))
         (when cardcanvas
           (send cardcanvas set-attacking))))
     
     ; ( creature% -> )
     (define/public (remove-attacking creature)
       (let* ((pos (pl:find children creature))
              (cardcanvas (if pos (pl:peek children pos) #f)))
         (when cardcanvas
           (send cardcanvas remove-attacking))))
     
     ;; ( creature% -> )
     (define/public (set-blocking creature)
       (let* ((pos (pl:find children creature))
              (cardcanvas (if pos (pl:peek children pos) #f)))
         (when cardcanvas
           (send cardcanvas set-blocking))))
     
     ; ( creature% -> )
     (define/public (remove-blocking creature)
       (let* ((pos (pl:find children creature))
              (cardcanvas (if pos (pl:peek children pos) #f)))
         (when cardcanvas
           (send cardcanvas remove-blocking))))
     
     ; ( -> )
     (define/public (untap-all)
       (pl:for-each children (lambda (c) (send c untap))))))
 
 ; Class name: player-panel
 ; Superclass: horizontal-panel%
 ; Description: A player-panel is an encapsulation of the different graphical parts of a player
 ; Attributes: - public: player, controller, up-down
 ;             - private: font, left, right, player-info, name,
 ;                  life, library, graveyard, rfg, mana-pool
 ;                  hand, lands, nonland-permanents, 
 ; Methods:
 ;  - public: look at gui-view for more information about the methods
 ;  - private: none
 ; Extra-info: up-down is to know how the zones are placed
 
 (define player-panel%
   (class horizontal-panel%
     (init-field player)
     (init-field controller)
     (init-field (up-down #f))
     (super-new)
     
     (define font (make-object font% 15 'decorative))
           
     (define left (new horizontal-panel% (parent this) (style (s:list 'border)) (border 2)))
     (define right (new vertical-panel% (parent this) (style (s:list 'border)) (border 2)))
     ; left
     (define player-info (new vertical-panel% (parent left) (style (s:list 'border)) (border 2)))          
     (define name (new (class panel% 
                         (super-new (parent player-info))
                         (define canvas (new canvas% (parent this) (paint-callback 
                                                                    (lambda (c dc)
                                                                      (send dc set-background (make-object color% 74 112 139)))))))))
     (new message% (parent name) (label (get-field name player)) (font font))
     (define life (new life-panel% (parent player-info) (player player) (controller controller)))
     (define library (new cardstack-panel% 
                          (parent player-info) (cardstack (get-field library player)) (zone-name 'library) (owner player) (controller controller)
                          (image (make-object bitmap% (build-absolute-path (current-directory) "pictures" "card_back.jpg")))))
     (define graveyard (new cardstack-panel%
                            (parent player-info) (cardstack (get-field graveyard player)) (zone-name 'graveyard) (owner player) (controller controller)
                            (image (make-object bitmap% (build-absolute-path (current-directory) "pictures" "gy.jpg")))))
     (define rfg (new cardstack-panel%
                      (parent player-info) (cardstack (get-field rfg player)) (zone-name 'rfg) (owner player) (controller controller)
                      (image (make-object bitmap% (build-absolute-path (current-directory) "pictures" "rfg.jpg")))))
     (define mana-pool (new mana-pool-panel% (parent left) (mana-pool (get-field mana-pool player)) (owner player) (controller controller) (min-height 350)))
     ; right
     (define hand 0)
     (define lands 0)
     (define nonland-permanents 0)
     (if up-down
         (begin (set! hand (new hand-panel% (parent right) (hand (get-field hand player)) (owner player) (controller controller)))
                (set! lands (new zone-canvas% (parent right) (zone (get-field lands player)) (zone-name 'lands) (owner player) (controller controller)))
                (set! nonland-permanents (new zone-canvas% (parent right) (zone (get-field nonlands player)) (zone-name 'nonland-permanents) (owner player) (controller controller))))             
                
         (begin (set! nonland-permanents (new zone-canvas% (parent right) (zone (get-field nonlands player)) (zone-name 'nonland-permanents) (owner player) (controller controller)))
                (set! lands (new zone-canvas% (parent right) (zone (get-field lands player)) (zone-name 'lands) (owner player) (controller controller)))
                (set! hand (new hand-panel% (parent right) (hand (get-field hand player)) (owner player) (controller controller)))))
     
     ; ( card-preview-panel% card-info-panel% -> )
     (define/public (initialize-hand display-panel card-info-panel)
       (send hand initialize display-panel card-info-panel))
     
     ; ( -> )
     (define/public (clear-hand)
       (send hand clear))
     
     ; ( -> )
     (define/public (update-library-size)
       (send library update-size))
     
     ; ( -> )
     (define/public (update-graveyard-size)
       (send graveyard update-size))
     
     ; ( -> )
     (define/public (update-rfg-size)
       (send rfg update-size))
     
     ; ( -> )
     (define/public (update-life)
       (send life update))
     
     ; ( symbol card% card-preview-panel% card-info-panel% -> )
     (define/public (add-card zone card display-panel card-info-panel)
       (case zone
         ((hand) (send hand add-cardcanvas card display-panel card-info-panel))
         ((lands) (send lands add-cardcanvas card display-panel card-info-panel))
         ((nonland-permanents) (send nonland-permanents add-cardcanvas card display-panel card-info-panel))))
     
     ; ( symbol card% -> )
     (define/public (remove-card zone card)
       (case zone
         ((hand) (send hand remove-cardcanvas card))
         ((lands) (send lands remove-cardcanvas card))
         ((nonland-permanents) (send nonland-permanents remove-cardcanvas card))))
     
     ; ( permanent% -> )
     (define/public (tap-permanent permanent)
       (cond
         ((is-a? permanent land%) (send lands tap-permanent permanent))
         ((is-a? permanent permanent%) (send nonland-permanents tap-permanent permanent))))
     
     ; ( -> )
     (define/public (untap-permanents)
       (send lands untap-all)
       (send nonland-permanents untap-all))
     
     ; ( symbol -> )
     (define/public (update-mana color)
       (send mana-pool update color))
     
     ; ( -> )
     (define/public (update-all-mana)
       (send mana-pool update-all))
     
     ; ( creature% -> )
     (define/public (set-attacking creature)
       (send nonland-permanents set-attacking creature))
     
     ; ( creature% -> )
     (define/public (remove-attacking creature)
       (send nonland-permanents remove-attacking creature))
     
     ; ( creature% -> )
     (define/public (set-blocking creature)
       (send nonland-permanents set-blocking creature))
     
     ; ( creature% -> )
     (define/public (remove-blocking creature)
       (send nonland-permanents remove-blocking creature))
     
     ; ( number/'all -> )
     (define/public (choice-from-library top-x)
       (send library choice-from-library top-x))))
 
 ; Class name: step-panel
 ; Superclass: panel%
 ; Description: A step-panel is a panel with a message (name of the step)
 ;    Depending if the panel is active, the background of the canvas changes
 ; Attributes: - public: name 
 ;             - private: active-font, non-active-font, color-canvas                
 ; Methods:
 ;  - public: 
 ;     * activate/deactivate: 
 ;         arguments: none
 ;         output: none
 ;         description: change the background of the color-canvas
 ;  - private: none
 
 (define step-panel%
   (class panel%
     (init-field name)
     (super-new (horiz-margin 1))
     (define active-font (make-object font% 8 'decorative 'normal 'bold))
     (define non-active-font (make-object font% 7 'decorative 'normal 'light))
     (define color-canvas
       (new canvas% (parent this) (min-width 50) (min-height 20)))
     (new message% (parent this) (label (symbol->string name)) (font non-active-font))
     
     ; ( -> )
     (define/public (activate)
       (send color-canvas set-canvas-background (make-object color% "YellowGreen"))
       (send color-canvas refresh))
     
     ; ( -> )
     (define/public (deactivate)
       (send color-canvas set-canvas-background (make-object color% "White"))
       (send color-canvas refresh))))
 
 ; Class name: phase-panel
 ; Superclass: panel%
 ; Description: A phase panel is a panel that has several step-panels
 ; Attributes: - public: steps-names-lst, turn 
 ;             - private: steps, curr-step               
 ; Methods:
 ;  - public: 
 ;     * deactivate-curr-step: 
 ;         arguments: none
 ;         output: none
 ;         description: send the current step deactivate
 ;     * update-step: 
 ;         arguments: none
 ;         output: none
 ;         description: deactiavte the current step and activate the next step
 ;  - private: 
 ;     * make-steps-lst: 
 ;         arguments: step-names-lst
 ;         output: none
 ;         description: make a new step-panel for each element of the list
 
 (define phase-panel%
   (class horizontal-panel%
     (init-field step-names-lst)
     (init-field turn)
     (super-new (horiz-margin 4) (style (s:list 'border)) (border 1))
     (define steps (pl:new (lambda (s sp) (eq? (get-field name s) (get-field name sp)))))
     (define curr-step 0)

     ; ( pair -> )
     (define/private (make-steps-lst step-names-lst)
       (for-each (lambda (s) (pl:add-before! steps (new step-panel% (parent this) (name s)))) step-names-lst))
     (make-steps-lst step-names-lst)
     
     ; ( -> )
     (define/public (deactivate-curr-step)
       (if (not (eq? curr-step 0))
           (send curr-step deactivate)))
     
     ; ( -> )
     (define/public (update-step)
       (let* ((new-step (send turn current-step))
              (step-panel (pl:peek steps (pl:find steps new-step))))
         (begin 
           (deactivate-curr-step)
           (send step-panel activate)
           (set! curr-step step-panel))))))

 ; Class name: phases-panel
 ; Superclass: panel%
 ; Description: A phase panel is a panel that has several phase-panels
 ; Attributes: - public: turn, controller 
 ;             - private: phase-panel for each phase               
 ; Methods:
 ;  - public: 
 ;     * update-phase: 
 ;         arguments: none
 ;         output: none
 ;         description: deactivate the current-step of the current-phase
 ;            Set a new current-phase and activate the first step of this
 ;     * update-step: 
 ;         arguments: none
 ;         output: none
 ;         description: deactiavte the current step of the current phase
 ;            and activate the next step of the current phase
 ;  - private: none
 
 (define phases-panel%
   (class horizontal-panel%
     (init-field turn)
     (init-field controller)
     (super-new (style (s:list 'border)) (border 1))
     (define beginning-phase (new phase-panel% (parent this) (turn turn) (step-names-lst (list 'Untap-step 'Upkeep-step 'Draw-step))))
     (define main-phase1 (new phase-panel% (parent this) (turn turn) (step-names-lst (list 'Main-phase1))))
     (define combat-phase (new phase-panel% (parent this) (turn turn) (step-names-lst (list 'Begin-of-combat 'Declare-attackers 'Declare-blockers 'Combat-damage 'End-of-combat))))
     (define main-phase2 (new phase-panel% (parent this) (turn turn) (step-names-lst (list 'Main-phase2))))
     (define end-phase (new phase-panel% (parent this) (turn turn) (step-names-lst (list 'End-of-turn 'Cleanup))))
     (define curr-phase beginning-phase)
     
     (new button% (label "Pass Priority") (parent this) (callback (lambda (b e) (send controller update 'pass-priority))))
     
     ; ( -> )
     (define/public (update-phase)
       (begin 
         (send curr-phase deactivate-curr-step)
         (case (get-field name (send turn current-phase))
           ((Beginning-phase) (send beginning-phase update-step)
                              (set! curr-phase beginning-phase))
           ((Main-phase1) (send main-phase1 update-step)
                          (set! curr-phase main-phase1))
           ((Combat-phase) (send combat-phase update-step)
                           (set! curr-phase combat-phase))
           ((Main-phase2) (send main-phase2 update-step)
                          (set! curr-phase main-phase2))
           ((End-phase) (send end-phase update-step)
                        (set! curr-phase end-phase)))))
     
     ; ( -> )
     (define/public (update-step)
       (case (get-field name (send turn current-phase))
         ((Beginning-phase) (send beginning-phase update-step))
         ((Main-phase1) (send main-phase1 update-step))
         ((Combat-phase) (send combat-phase update-step))
         ((Main-phase2) (send main-phase2 update-step))
         ((End-phase) (send end-phase update-step))))))
 
 ; Class name: card-preview-panel
 ; Superclass: panel%
 ; Description: A card-preview-panel is a panel that displays an image of a card
 ; Attributes: - public: turn, controller 
 ;             - private: image, preview-canvas, dc               
 ; Methods:
 ;  - public: 
 ;     * set-image: 
 ;         arguments: image
 ;         output: none
 ;         description: draw the new image
 ;  - private: none
 
 (define card-preview-panel%
   (class panel%
     (super-new (style (s:list 'border)) (border 4))     
     (define image (make-object bitmap% (build-absolute-path (current-directory) "pictures" "tenth" "Craw Wurm.jpg")))     
     (define preview-canvas (new canvas% (parent this) (min-width 223) (min-height 310)))
     (define dc (send preview-canvas get-dc))
     
     ; ( bitmap% -> )
     (define/public (set-image! image)
       (send dc draw-bitmap image 0 0))))
 
 ; Class name: card-info-panel
 ; Superclass: vertical-panel%
 ; Description: A card-info-panel displays information of a card
 ; Attributes: - public: turn, controller 
 ;             - private: info-lines, font               
 ; Methods:
 ;  - public: 
 ;     * display-card-info: 
 ;         arguments: card
 ;         output: none
 ;         description: displays the info of the card
 ;  - private:
 ;     * display-creature/aura-info: 
 ;         arguments: card
 ;         output: none
 ;         description: displays the info of the card
 
 (define card-info-panel%
   (class vertical-panel%
     (super-new (style (s:list 'border)) (border 4))
     (inherit get-children delete-child)
     (define font (make-object font% 13 'decorative))
     (define info-line1 (new message% (parent this) (font font) (label "") (auto-resize #t)))
     (define info-line2 (new message% (parent this) (font font) (label "") (auto-resize #t)))
     (define info-line3 (new message% (parent this) (font font) (label "") (auto-resize #t)))
     (define info-line4 (new message% (parent this) (font font) (label "") (auto-resize #t)))
     (define info-line5 (new message% (parent this) (font font) (label "") (auto-resize #t)))
     
     ; ( creature% -> )
     (define/private (display-creature-info creature)
       (define (extract-keyword-abilities abilities)
         (if (null? abilities)
             ""
             (string-append (symbol->string (get-field keyword-name (car abilities))) " "(extract-keyword-abilities (cdr abilities)))))
       (send info-line1 set-label (string-append "Power: " (number->string (send creature power))))
       (send info-line2 set-label (string-append "Toughness: " (number->string (send creature toughness))))
       (send info-line3 set-label (string-append "Damage: " (number->string (get-field damage creature))))
       (send info-line4 set-label "Keyword abilities: ")
       (send info-line5 set-label (extract-keyword-abilities (get-field keyword-abilities creature))))
     
     ; ( aura% -> )
     (define/private (display-aura-info aura)
       (send info-line1 set-label (string-append "Target: " (if (eq? (get-field target aura) 0)
                                                              ""
                                                              (get-field name (get-field target aura)))))
       (send info-line2 set-label "")
       (send info-line3 set-label "")
       (send info-line4 set-label "")
       (send info-line5 set-label ""))
   
     ; ( card% -> )
     (define/public (display-card-info card)
       (cond
         ((is-a? card creature%) (display-creature-info card))
         ((is-a? card aura%) (display-aura-info card))
         (else (send info-line1 set-label "")
               (send info-line2 set-label "")
               (send info-line3 set-label "")
               (send info-line4 set-label "")
               (send info-line5 set-label ""))))))
 
 ; Class name: mtg-menu
 ; Superclass: menu-bar%
 ; Description: A menu for the mtg frame%
 
 (define mtg-menu%
   (class menu-bar%
     (init-field controller)
     (super-new)
     
     (define game-menu (new menu% (parent this) (label "Game")))
     (define new-game (new menu-item% (parent game-menu) (label "New Game")
                           (callback (lambda (x y) 'ok))))
                           ;(callback (lambda (i e) (send controller update 'new-game)))))
     (define options (new menu-item% (parent game-menu) (label "Options") (callback (lambda (x y) 'ok))))
     
     (define help-menu (new menu% (parent this) (label "Help")))
     (define help (new menu-item% (parent help-menu) (label "Help") 
                       (callback (lambda (i e) 
                                   (message-box "Help" "To learn more about Magic the Gathering go to \nhttp://www.wizards.com/magic/TCG/NewtoMagic.aspx")))))
     (define about (new menu-item% (parent help-menu) (label "About") 
                        (callback (lambda (i e) 
                                    (define dialog (new dialog% (label "About") (min-height 50)))
                                    (define panel (new horizontal-panel% (parent dialog)))
                                    (define left (new vertical-panel% (parent panel) (min-width 178)))
                                    (define right (new vertical-panel% (parent panel)))
                                    (define mtg-logo (make-object bitmap%
                                                       (build-absolute-path (current-directory) "pictures" "magic_logo.gif")))
                                    (new canvas%
                                         (parent left)
                                         (paint-callback (lambda (e dc) (send dc draw-bitmap mtg-logo 0 0))))                           
                                    (new message% (parent right) (label "Magic the Gathering"))
                                    (new message% (parent right) (label "Created by Wouter Van Rossem"))
                                    (send dialog show #t)))))))
 
 ; Class name: info-panel
 ; Superclass: vertical-panel%
 ; Description: An info-panel displays information of the game
 ; Attributes: - public: mtg, stack-frame
 ;             - private: messages for info               
 ; Methods:
 ;  - public: 
 ;     * update-******: 
 ;         arguments: none
 ;         output: none
 ;         description: update the message label
 ;  - private: none 
 
 (define info-panel%
   (class vertical-panel%
     (init-field mtg)
     (init-field stack-frame)
     (super-new (alignment (s:list 'center 'center)) (style (s:list 'border)) (border 2))      
     (define font (make-object font% 13 'decorative))
     
     (define standings
       (new message% (parent this) (font font) (auto-resize #t)
            (label (string-append "Standings: " (number->string (vector-ref (get-field standings mtg) 0)) " - " (number->string (vector-ref (get-field standings mtg) 0))))))
     (define turns
       (new message% (parent this) (font font) (auto-resize #t) (label (string-append "Number of turns: " (number->string (get-field turn-nr mtg))))))
     (define active-player
       (new message% (parent this) (font font) (auto-resize #t) (label (string-append "Active-player: " (get-field name (send mtg active-player))))))
     (define priority
       (new message% (parent this) (font font) (auto-resize #t) (label (string-append "Priority: " (get-field name (send mtg get-player (+ (get-field priority mtg) 1)))))))
     (define stack-button (new button% (parent this) (label "Show the stack")
                               (callback (lambda (b e) (send stack-frame show #t)))))
     
     ; ( -> )
     (define/public (update-standings)
       (send standings set-label (string-append "Standings: " (vector-ref (get-field standings 0)) " - " (vector-ref (get-field standings 1)))))
     (define/public (update-nr-of-turns)
       (send turns set-label (string-append "Number of turns: " (number->string (get-field turn-nr mtg)))))
     (define/public (update-active-player)
       (send active-player set-label (string-append "Active-player: " (get-field name (send mtg active-player)))))     
     (define/public (update-priority)
       (send priority set-label (string-append "Priority: " (get-field name (send mtg get-player (+ (get-field priority mtg) 1))))))
     
     ; ( -> )
     (define/public (update-all)
       (begin (update-standings)
              (update-nr-of-turns)
              (update-active-player)
              (update-priority)))))
 
 ; Class name: stack-frame
 ; Superclass: frame%
 ; Description: A stack-frame is a frame to show information about the stack-contents
 ; Attributes: - public: stack, controller 
 ;             - private: choices, buttons               
 ; Methods:
 ;  - public: 
 ;     * add-spell: 
 ;         arguments: stack-spell
 ;         output: none
 ;         description: add a new stack-spell to the stack-frame
 ;            i.e. add a new choice to the list-box
 ;     * remove-top: 
 ;         arguments: stack-spell
 ;         output: none
 ;         description: remove the top stack-spell/abi from the stack-frame
 ;            i.e. remove the top choice from the list-box
 ;  - private:
 ;     * display-creature/aura-info: 
 ;         arguments: card
 ;         output: none
 ;         description: displays the info of the card
 
 (define stack-frame%
   (class frame%
     (init-field controller)
     (init-field stack)
     (super-new (label "The Stack"))
     (define main (new vertical-panel% (parent this)))
     (define choices (new list-box% (label "The Stack") (parent main)
                          (choices '()) (min-width 200) (min-height 300)))
     (define buttons (new horizontal-panel% (parent main) (alignment (s:list 'center 'center))))
     (define top-of-stack -1)
     
     (define close-button (new button% (parent buttons) (label "Hide")
                                (callback (lambda (b e) (send this show #f)))))
     
     (define select-button (new button% (parent buttons) (label "Select")
                                (callback (lambda (b e) (let ((selection (send choices get-selections)))
                                                          (send controller update 'select-stack-item (send choices get-data (s:car selection))))))))
     ; ( stack-spell/abi% -> )
     (define/public (add-spell stack-spell)
       (if (is-a? (get-field spell/abi stack-spell) ability%)
           (begin
             (send choices append (get-field name (get-field card stack-spell)) stack-spell)
             (set! top-of-stack (+ top-of-stack 1)))
           (begin 
             (send choices append (get-field name (get-field spell/abi stack-spell)) stack-spell)
             (set! top-of-stack (+ top-of-stack 1)))))
     
     ; ( -> )
     (define/public (remove-top)
       (begin
         (send choices delete top-of-stack)
         (set! top-of-stack (- top-of-stack 1))))))
     
 ; Class name: announced-spell-frame
 ; Superclass: frame%
 ; Description: An announced spell frame is used to display information about the currenr
 ;    announced spell/abi
 ;    The frame can be in different states, depending on wich state of playing a spell it is in
 ; Attributes: - public: announced-spell/abi, controller 
 ;             - private: state               
 ; Methods:
 ;  - public: 
 ;     * update-mana: 
 ;         arguments: none
 ;         output: none
 ;         description: update the spent mana for the announced-spell
 ;     * udpate-state: 
 ;         arguments: state
 ;         output: none
 ;         description: change the state of playing a spell
 ;  - private: none
 
 (define announce-spell-frame%
   (class frame%
     (init-field announced-spell)
     (init-field controller)
     (super-new (label "Announce spell"))
     (define vertical (new vertical-panel% (parent this)))
     (define main (new horizontal-panel% (parent vertical) (style (s:list 'border)) (border 1)))
     (define buttons (new horizontal-panel% (parent vertical) (alignment (s:list 'center 'center))))
     (define spell (get-field spell/abi announced-spell))
     (define image (let ((spell (if (is-a? spell ability%)
                                    (get-field card announced-spell) spell)))
                     (make-object bitmap% (build-absolute-path (current-directory) "pictures" (get-field expansion spell)
                                                               (string-append (get-field name spell) ".jpg")))))
     (define image-canvas (new canvas% (parent main) (min-width 223) (min-height 310)
                               (paint-callback (lambda (c dc) (send dc draw-bitmap image 0 0)))))
     (define info (new panel% (parent main)))
     
     (define cancel-button (new button% (label "Cancel") (parent buttons) 
                                (callback (lambda (b e) (send this show #f) (send controller update 'cancel-announced-spell)))))
     
     
     ; Class name: pay-mana
     ; Superclass: vertical-panel% 
     ; Attributes: - public: none
     ;             - private: mc, total, message for each color unless amount = 0               
     ; Methods:
     ;  - public: 
     ;     * update: 
     ;         arguments: none
     ;         output: none
     ;         description: update color and gauge
     ;  - private: 
     ;     * update-color: 
     ;         arguments: color
     ;         output: none
     ;         description: update the payed mana for the color
     ;     * update-colors: 
     ;         arguments: none
     ;         output: none
     ;         description: update the payed mana for all colors
     ;     * udpate-gauge: 
     ;         arguments: state
     ;         output: none
     ;         description: update the total gauge
     
     (define pay-mana%
       (class vertical-panel%
         (super-new (parent info) (alignment (s:list 'center 'center)) (style (s:list 'border)) (border 1))
         (define mc (get-field mc announced-spell))
         (define total (send mc converted-mana-cost))
         (define white (if (= (get-field white mc) 0) 
                           0 (new message% (parent this) (auto-resize #t) (label (string-append "White mana: " (number->string (get-field white mc)))))))
         (define blue (if (= (get-field blue mc) 0) 
                          0 (new message% (parent this) (auto-resize #t) (label (string-append "Blue mana: " (number->string (get-field blue mc)))))))
         (define black (if (= (get-field black mc) 0) 
                           0 (new message% (parent this) (auto-resize #t) (label (string-append "Black mana: " (number->string (get-field black mc)))))))
         (define red (if (= (get-field red mc) 0)
                         0 (new message% (parent this) (auto-resize #t) (label (string-append "Red mana: " (number->string (get-field red mc)))))))
         (define green (if (= (get-field green mc) 0)
                           0 (new message% (parent this) (auto-resize #t) (label (string-append "Green mana: " (number->string (get-field green mc)))))))
         (define colorless (if (= (get-field colorless mc) 0)
                               0 (new message% (parent this) (auto-resize #t) (label (string-append "Any color: " (number->string (get-field colorless mc)))))))
         
         (define total-gauge (new gauge% (label "Total") (range total) (parent this)))
         
         ; ( symbol -> )
         (define/private (update-color color)
           (case color
             ((white) (unless (eq? white 0) (send white set-label (string-append "White mana: " (number->string (get-field white mc))))))
             ((blue) (unless (eq? blue 0) (send blue set-label (string-append "Blue mana: " (number->string (get-field blue mc))))))
             ((black) (unless (eq? black 0) (send black set-label (string-append "Black mana: " (number->string (get-field black mc))))))
             ((red) (unless (eq? red 0) (send red set-label (string-append "Red mana: " (number->string (get-field red mc))))))
             ((green) (unless (eq? green 0) (send green set-label (string-append "Green mana: " (number->string (get-field green mc))))))
             ((colorless) (unless (eq? colorless 0) (send colorless set-label (string-append "Any mana: " (number->string (get-field colorless mc))))))))
           
         ; ( -> )
         (define/private (update-colors)
           (begin
             (update-color 'white)
             (update-color 'blue)
             (update-color 'black)
             (update-color 'red)
             (update-color 'green)
             (update-color 'colorless)))
    
         ; ( -> )
         (define/private (update-gauge)
           (let ((old-value (send total-gauge get-value)))
             (send total-gauge set-value (+ old-value 1))))
         
         ; ( -> )
         (define/public (update)
           (update-colors)
           (update-gauge))))
     
     ; Class name: choose-mode
     ; Superclass: vertical-panel% 
     ; Attributes: - public: none
     ;             - private: none              
     ; Methods:
     ;  - public: none
     ;  - private: none
     
     (define choose-mode%
       (class vertical-panel%
         (super-new (parent info) (style (s:list 'border)) (border 1))))
     
     ; Class name: choose-target
     ; Superclass: vertical-panel% 
     ; Attributes: - public: none
     ;             - private: mc, total, message for each color unless amount = 0               
     ; Methods:
     ;  - public: 
     ;     * update: 
     ;         arguments: none
     ;         output: none
     ;         description: update the choose-target panel
     ;  - private: none
     
     (define choose-target%
       (class vertical-panel%
         (super-new (parent info) (alignment (s:list 'center 'center)) (style (s:list 'border)) (border 1))
         (new message% (parent this) (label "Choose target(s)"))
         (new message% (parent this) (label "Current target(s)"))
         
         (define/public (update)
           (let ((new-target (car (get-field targets announced-spell))))
             (new message% (parent this) (label (get-field name new-target)))))))
     
     (define state (new choose-mode%))
     
     ; ( -> )
     (define/public (update-mana)
       (when (is-a? state pay-mana%)
         (send state update)))
     
     ; ( symbol -> )
     (define/public (update-state s)
       (case s
         ((choose-target) (send info delete-child state)
                          (set! state (new choose-target%)))
         ((choose-mode) (send info delete-child state)
                        (set! state (new choose-mode%)))
         ((pay-mana) (send info delete-child state)
                     (set! state (new pay-mana%)))))))
 
 ; Class name: mtg-frame
 ; Superclass: frame%
 ; Description: This is an encapsulation of all the graphical parts of a mtg game
 ;    View sends its update message to this
 ; Attributes: - public: player1, player2, turn, controller, mtg 
 ;             - private: phase-panel, player-panels, menu
 ;                  preview-panel, info-panel, stack
 ; Methods:
 ;  - public: ...
 ;  - private:  
 ;     * get-player-panel: 
 ;         arguments: none
 ;         output: none
 ;         description: get the player-panel for a given player
 ; Extra info: Look at gui-view for more information about the methods
 (define mtg-frame%
   (class frame% 
     (init-field player1)
     (init-field player2)
     (init-field turn)
     (init-field controller)
     (init-field mtg)
     (inherit show)
     (super-new (label "Magic The Gathering")) ;(min-width 1000) (min-height 300))
     (define menu-bar (new mtg-menu% (parent this) (controller controller)))
     (define main-pane (new horizontal-pane% (parent this)))
     (define left-panel (new vertical-pane% (parent main-pane)))
     (define right-panel (new vertical-pane% (parent main-pane)))
     
     (define the-stack (new stack-frame% (controller controller) (stack (get-field stack mtg))))
     
     ; left
     (define player2-panel (new player-panel% (parent left-panel) (up-down #t) (player player2) (controller controller)))
     (define turn-panel (new phases-panel% (parent left-panel)  (turn turn) (controller controller)))
     (define player1-panel (new player-panel% (parent left-panel) (player player1) (controller controller)))
     ; right
     (define card-preview-panel (new card-preview-panel% (parent right-panel)))
     (define card-info-panel (new card-info-panel% (parent right-panel)))
     (define info-panel (new info-panel% (mtg mtg) (stack-frame the-stack) (parent right-panel)))     
     
     (define announced-spell-frame 0)
     
     ; ( player% -> )
     (define/private (get-player-panel player)
       (cond 
         ((eq? player player1) player1-panel)
         ((eq? player player2) player2-panel)))
     
     ; ( player% -> )
     (define/public (initialize-hand player)
       (let ((player-panel (get-player-panel player)))
         (send player-panel initialize-hand card-preview-panel card-info-panel)))
     
     ; ( player% -> )
     (define/public (clear-hand player)
       (let ((player-panel (get-player-panel player)))
         (send player-panel clear-hand)))
     
     ; ( player% -> )
     (define/public (update-library-size player)
       (let ((player-panel (get-player-panel player)))
         (send player-panel update-library-size)))
     
     ; ( player% -> )
     (define/public (update-graveyard-size player)
       (let ((player-panel (get-player-panel player)))
         (send player-panel update-graveyard-size)))
     
     ; ( player% -> )
     (define/public (update-rfg-size player)
       (let ((player-panel (get-player-panel player)))
         (send player-panel update-rfg-size)))
     
     ; ( player% -> )
     (define/public (update-life player)
       (let ((player-panel (get-player-panel player)))
         (send player-panel update-life)))
     
     ; ( -> )
     (define/public (update-phase)
       (send turn-panel update-phase))
     
     ; ( -> )
     (define/public (update-step)
       (send turn-panel update-step))
     
     ; ( -> )
     (define/public (update-priority)
       (send info-panel update-priority))
     
     ; ( -> )
     (define/public (update-active-player)
       (send info-panel update-active-player))
     
     ; ( -> )
     (define/public (update-nr-of-turns)
       (send info-panel update-nr-of-turns))
     
     ; ( symbol card% player% -> )
     (define/public (add-card zone card player)
       (let ((player-panel (get-player-panel player)))
         (send player-panel add-card zone card card-preview-panel card-info-panel)))
     
     ; ( symbol card% player% -> )
     (define/public (remove-card zone card player)
       (let ((player-panel (get-player-panel player)))
         (send player-panel remove-card zone card)))
     
     ; ( card% player% -> )
     (define/public (tap-permanent permanent player)
       (let ((player-panel (get-player-panel player)))
         (send player-panel tap-permanent permanent)))
     
     ; ( symbol player% -> )
     (define/public (untap-permanents player)
       (let ((player-panel (get-player-panel player)))
         (send player-panel untap-permanents)))
     
     ; ( symbol player% -> )
     (define/public (update-mana color player)
       (let ((player-panel (get-player-panel player)))
         (send player-panel update-mana color)))
     
     ; ( player% -> )
     (define/public (update-all-mana player)
       (let ((player-panel (get-player-panel player)))
         (send player-panel update-all-mana)))
     
     ; ( stack-spell/abi% -> )
     (define/public (announce-spell stack-spell)
       (set! announced-spell-frame (new announce-spell-frame% (announced-spell stack-spell) (controller controller)))
       (send announced-spell-frame show #t))
     
     ; ( -> )
     (define/public (update-announced-spell-mana)
       (send announced-spell-frame update-mana))
     
     ; ( symbol -> )
     (define/public (update-announced-spell-state state)
       (send announced-spell-frame update-state state))
     
     ; ( -> )
     (define/public (announced-spell-done)
       (send announced-spell-frame show #f))
     
     ; ( -> )
     (define/public (add-stack-spell stack-spell)
       (send the-stack add-spell stack-spell))
     
     ; ( -> )
     (define/public (remove-top-of-stack)
       (send the-stack remove-top))
     
     ; ( creature% player% -> )
     (define/public (set-attacking creature owner)
       (let ((player-panel (get-player-panel owner)))
         (send player-panel set-attacking creature)))
     
     ; ( creature% player% -> )
     (define/public (remove-attacking creature owner)
       (let ((player-panel (get-player-panel owner)))
         (send player-panel remove-attacking creature)))
     
     ; ( creature% player% -> )
     (define/public (set-blocking creature owner)
       (let ((player-panel (get-player-panel owner)))
         (send player-panel set-blocking creature)))
     
     ; ( creature% player% -> )
     (define/public (remove-blocking creature owner)
       (let ((player-panel (get-player-panel owner)))
         (send player-panel remove-blocking creature)))
     
     ; ( player% number/'all-> )
     (define/public (choice-from-library player top-x)
       (let ((player-panel (get-player-panel player)))
         (send player-panel choice-from-library top-x))))))