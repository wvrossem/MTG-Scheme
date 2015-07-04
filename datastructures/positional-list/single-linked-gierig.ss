;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*                                                                 *-*-
;-*-*                    Linked Positional Lists                      *-*-
;-*-*                                                                 *-*-
;-*-*                       Wolfgang De Meuter                        *-*-
;-*-*                 2008 Programming Technology Lab                 *-*-
;-*-*                   Vrije Universiteit Brussel                    *-*-
;-*-*                                                                 *-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-

#lang r6rs

(library 
 (linked-positional-list)
 (export new positional-list-tag equality
         attach-first! attach-last! attach-middle!
         detach-first! detach-last! detach-middle!
         length empty? full? update! peek
         first last has-next? has-previous? next previous
         shuffle peek-pos)
 (import (except (rnrs base (6)) length)
         (rnrs mutable-pairs)
         (only (scheme base) random null)
         (prefix (only (rnrs base (6)) length) s:))
 
 (define (make-list-node val next)
   (cons val next))
 (define (list-node-val node)
   (car node))
 (define (list-node-val! node val)
   (set-car! node val))
 (define (list-node-next node)
   (cdr node))
 (define (list-node-next! node next)
   (set-cdr! node next))
 
 (define positional-list-tag 'single-linked-list)
 (define (make head ==?)
   (list positional-list-tag head ==?))
 (define (head plst)
   (cadr plst))
 (define (head! plst node)
   (set-car! (cdr plst) node))
 (define (equality plst)
   (caddr plst))
 (define (new ==?)
   (make '() ==?))
 
 (define (iter-from-head plist stop?)
   (define first (head plist))
   (define (chasing-pointers prev next)
     (if (stop? next)
       prev
       (chasing-pointers next (list-node-next next))))
   (chasing-pointers '() first))
 
 (define (attach-first! plst val)
   (define first (head plst))
   (define node (make-list-node val first))
   (head! plst node))
 
 (define (attach-middle! plst val pos)
   (define next (list-node-next pos))
   (define node (make-list-node val next))
   (list-node-next! pos node))
 
 (define (attach-last! plst val)
   (define first (head plst))
   (define last (iter-from-head plst null?))
   (define node (make-list-node val '()))
   (if (null? first)
     (head! plst node) ; last is also first
     (list-node-next! last node)))

 (define (detach-first! plst)
   (define first (head plst))
   (define second (list-node-next first))
   (head! plst second))
 
 (define (detach-middle! plst pos)
   (define next (list-node-next pos))
   (define prev (iter-from-head plst (lambda (node) (eq? pos node))))
   (list-node-next! prev next))
 
 (define (detach-last! plst pos)
   (define first (head plst))
   (if (null? (list-node-next first)) ; last is also first
     (head! plst '())
     (list-node-next! (iter-from-head plst (lambda (last) (not (has-next? plst last)))) '())))
 
 (define (length plst)
   (let length-iter
     ((curr (head plst))
      (size 0))
     (if (null? curr)
       size
       (length-iter (list-node-next curr) (+ size 1)))))
 
 (define (full? plst)
   #f)
 
 (define (empty? plst)
   (null? (head plst)))
 
 (define (first plst)
   (if (null? (head plst))
     (error "list empty (first)" plst)
     (head plst)))
 
 (define (last plst)
   (if (null? (head plst))
     (error "list empty (last)" plst)
     (iter-from-head plst null?)))
 
 (define (has-next? plst pos)
   (not (null? (list-node-next pos))))
 
 (define (has-previous? plst pos)
   (not (eq? pos (head plst))))
 
 (define (next plst pos)
   (if (not (has-next? plst pos))
     (error "list has no next (next)" plst)
     (list-node-next pos)))
 
 (define (previous plst pos)
   (if (not (has-previous? plst pos))
     (error "list has no previous (previous)" plst)
     (iter-from-head plst (lambda (node) (eq? pos node)))))
 
 (define (update! plst pos val)
   (list-node-val! pos val)
   plst)
 
 (define (peek plst pos)
   (list-node-val pos))
 
 (define (peek-pos plst pos)
   (if (> pos (length plst))
       (error "pos out of range" plst)
       (let loop ((l (head plst))
                  (i 0))
         (if (= i pos)
             (list-node-val l)
             (loop (list-node-next l) (+ i 1))))))
 
 ; Bron: Drscheme utils.ss
 ; ; De lijst wordt opgesplits in 2 delen, daarna weer samengevoegd door de bovenste kaart van een helften (willekeurig gekozen) te nemen. 7 zou de beste counter zijn.
 (define (shuffle plst)
   (define l (head plst))
   (let shuffle-list ((l l) (c 7))
     (if (zero? c) ; als de counter 0 is word de lijst niet geshufflet
         (head! plst l)
         (let-values ([(a b) ; (let-values (((variable ···) expr) ···) body-expr ···1), vb (let-values ([(x y) (values 5 8)]) y) -> 8
                       (let ([half (floor (/ (s:length l) 2))]) ;lengte van lijst delen door 2 en naar beneden afronden (floor <-> ceiling), 
                         (values ;bind de lokale variabelen a en b
                          (let loop ([l l][n half]) ; named let, bind a, recursieve proc, de lijst wordt doorlopen tot half (alle elementen tot car worden geconst
                            (if (zero? n)
                                null
                                (cons (car l) (loop (cdr l) (- n 1))))) ; (sub1 x) geeft x - 1
                          (list-tail l half)))]) ;= b, geeft de lijst van elementen na posistie half
           (shuffle-list
            (let loop ([a a][b b][l null]) ; a en b zijn gebonden door let-value, null = lege lijst
              (cond
                [(null? a) (append (reverse b) l)] ; als een van de 2 lijsten leeg is word de ander gereverset en aan l geäppend
                [(null? b) (append (reverse a) l)]
                [(zero? (random 2)) ; random geeft 1 of 0 
                 (loop (cdr a) b (cons (car a) l))] ; als het 0 is neemt proc de top van de eerste helft
                [else
                 (loop a (cdr b) (cons (car b) l))])) ; als het 1 is neemt proc top van de andere helft
            (- c 1))))))) ; counter - 1, procedure wordt dus c keer herhaalt