#|
File : mana-pool
Author : Wouter Van Rossem 
|#

#!r6rs

(library
 (mana-pool)
 (export mana-pool%)
 (import (rnrs base (6))
         (rnrs io simple)
         (scheme class))
 
 ; Class name: mana-pool
 ; Superclass: object%  
 ; Attributes: - public: white, blue, black, red, green, colorless
 ;             - private: none
 ; Methods:
 ;  - public:
 ;     * inc-mana: 
 ;         arguments: amount, color
 ;         output: none
 ;         description: increase the mana in the mana-pool of a certain color with a certain amount.
 ;                      Amount cannot be negative
 ;     * dec-mana: 
 ;         arguments: amount, color
 ;         output: none
 ;         description: decrease the mana in the mana pool of a certain color with a certain amount.
 ;                      Amount cannot be negative. If amount is larger than the amount in the mana pool, 
 ;                      it will be set to 0. This way we can't get any negative values for mana.
 ;     * reset: 
 ;         arguments: none
 ;         output: none
 ;         description: Reset the mana pool and return the mana burn, i.e. the amount of unspent mana                  
 ;  - private: none
 
 (define mana-pool%
   (class object%
     (super-new)
     (field (white 0))
     (field (blue 0))
     (field (black 0))
     (field (red 0))
     (field (green 0))
     (field (colorless 0))
     
     ; ( number symbol -> )
     (define/public (inc-mana amount color)
       (if (>= amount 0)
           (case color
             ((white w) (set! white (+ white amount)))
             ((blue u) (set! blue (+ blue amount)))
             ((black b) (set! black (+ black amount)))
             ((red r) (set! red (+ red amount)))
             ((green g) (set! green (+ green amount)))
             ((colorless c) (set! colorless (+ colorless amount)))
             (else (display "MANA-POOL -- inc-mana : unknown mana color" color)))
           (error "MANA-POOL -- inc-mana : amount must be >= 0" amount)))
     
     ; ( number symbol -> )
     (define/public (dec-mana amount color)
       (if (>= amount 0)
           (case color
             ((white w) (if (<= amount white) (set! white (- white amount)) (set! white 0)))
             ((blue u) (if (<= amount blue) (set! blue (- blue amount)) (set! blue 0)))
             ((black b) (if (<= amount black) (set! black (- black amount)) (set! black 0)))
             ((red r) (if (<= amount red) (set! red (- red amount)) (set! red 0)))
             ((green g) (if (<= amount green) (set! green (- green amount)) (set! green 0)))
             ((colorless c) (if (<= amount colorless) (set! colorless (- colorless amount)) (set! colorless 0)))
             (else (error "MANA-POOL -- dec-mana : unknown mana color" color)
           (error "MANA-POOL -- dec-mana : amount must be >= 0" amount)))))
     
     ; ( symbol -> number )
     (define/public (get-mana color)
       (case color
         ((white w) white)
         ((blue u) blue)
         ((black b) black)
         ((red r) red)
         ((green g) green)
         ((colorless c) colorless)
         (else (error "MANA-POOL -- dec-mana : unknown mana color" color))))
     
     ; ( -> )
     (define/public (reset)
       (let (; Unspent mana causes mana burn at end of phases.
             (mana-burn 0))
         (begin 
           (set! mana-burn (+ mana-burn white blue black red green colorless))
           (set! white 0)
           (set! blue 0)
           (set! black 0)
           (set! red 0)
           (set! green 0)
           (set! colorless 0)
           mana-burn))))))
        