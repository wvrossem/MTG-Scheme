#|
File : mana-cost
Author : Wouter Van Rossem 
|#

#!r6rs

(library
 (card)
 (export mana-cost%)
 (import (rnrs base (6))
         (rnrs io simple)
         (scheme class)
         (only (rnrs lists (6)) filter))
 
 ; Class name: mana-cost
 ; Superclass: object%
 ; Attributes: - public: white, blue, black, red, green, colorless, x?
 ;             - private: none       
 ; Methods:
 ;  - public: 
 ;     * derive-color: 
 ;         arguments: none
 ;         output : symbol
 ;         description: derive the color from the mana-cost
 ;     * converted-mana-cost: 
 ;         arguments: none
 ;         output: number
 ;         description: calculate the converted mana-cost, X=0 here
 ;     * set-x: 
 ;         arguments: x
 ;         output : none
 ;         description: set x on a value
 ;     * pay-mana: 
 ;         arguments: color, amount
 ;         output : none
 ;         description: reduce a color of mana with amount
 ;     * mana-payed?: 
 ;         arguments: none
 ;         output : bool
 ;         description: has all the mana of this mana-cost been payed?
 ;     * reset: 
 ;         arguments: none
 ;         output : none
 ;         description: reset this mana-cost, i.e. set all values on their original values
 ;  - private: none
 
 (define mana-cost%
   (class object%
     (init-field (white 0))
     (init-field (blue 0))
     (init-field (black 0))
     (init-field (red 0))
     (init-field (green 0))
     (init-field (colorless 0))
     (init-field (x? #f))
     (super-new)
     (define init-white white)
     (define init-blue blue)
     (define init-black black)
     (define init-red red)
     (define init-green green)
     (define init-colorless colorless)
     
     ; ( -> symbol )
     ; Put all color in a list. Then we filter this list so that each color > 0.
     ; If the length of this list >= 2, then it is a muliticolored manacost.
     ; Otherwise it is monocolored.
     (define/public (derive-color)
       (let ((color-lst (filter (lambda (x) (not (= x 0)))
                                (list white blue black red green))))
         (cond
           ((>= (length color-lst) 2) 'multi-color)
           ((= (length color-lst) 1) 
            (cond ((not (= white 0)) 'white)
                  ((not (= blue 0)) 'blue)
                  ((not (= black 0)) 'black)
                  ((not (= red 0)) 'red)
                  ((not (= green 0)) 'green)))
           (else 'colorless))))
          
     ; ( -> number )
     (define/public (converted-mana-cost)
       (+ white blue black red green colorless))
     
     ; ( number -> )
     (define/public (set-x x)
       (set! x? x)
       (set! colorless (+ colorless x)))
     
     ; ( symbol number -> )
     (define/public (pay-mana color amount)
       (case color
         ((white) (set! white (- white amount)))
         ((blue) (set! blue (- blue amount)))
         ((black) (set! black (- black amount)))
         ((red) (set! red (- red amount)))
         ((green) (set! green (- green amount)))
         ((colorless) (set! colorless (- colorless amount)))))
     
     ; ( -> bool )
     (define/public (mana-payed?)
       (= white blue black red green colorless 0))
     
     ; ( -> )
     (define/public (reset)
       (set! white init-white)
       (set! blue init-blue)
       (set! black init-black)
       (set! red init-red)
       (set! green init-green)
       (set! colorless init-colorless)))))