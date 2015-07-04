;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*                                                                 *-*-
;-*-*                  Positional List code shared                    *-*-
;-*-*                   among all implementations                     *-*-
;-*-*                       Wolfgang De Meuter                        *-*-
;-*-*                 2008 Programming Technology Lab                 *-*-
;-*-*                   Vrije Universiteit Brussel                    *-*-
;-*-*                                                                 *-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-

#lang r6rs

(library 
 (positional-list adt)
 (export new from-scheme-list next previous 
         map for-each
         find delete! peek update! add-before! add-after!
         first last has-next? has-previous?
         length empty? full? shuffle peek-pos)
 (import (except (rnrs base (6)) length list? map for-each)
         ;(a-d positional-list with-sentinel))
         (magicthegathering datastructures positional-list without-sentinel))
 
 (define (positional-list? any)
   (and (pair? any)
        (eq? (car any) positional-list-tag)))
 
 (define (from-scheme-list slst ==?)
   (define result (new ==?))
   (if (null? slst)
     result
     (let for-all
       ((orig (cdr slst))
        (curr (first (add-after! result (car slst)))))
       (cond
         ((not (null? orig))
          (add-after! result (car orig) curr)
          (for-all (cdr orig) (next result curr)))
         (else
          result)))))
 
 (define (map plst f ==?)
   (define result (new ==?))
   (if (empty? plst)
     result
     (let for-all
       ((orig (first plst))
        (curr (first 
               (add-after! result (f (peek plst (first plst)))))))
       (if (has-next? plst orig)
         (for-all (next plst orig) 
                  (next (add-after! result
                                    (f (peek plst (next plst orig))) 
                                    curr)
                        curr))
         result))))
 
 (define (for-each plst f)
   (if (not (empty? plst))
     (let for-all
       ((curr (first plst)))
       (f (peek plst curr))
       (if (has-next? plst curr)
         (for-all (next plst curr)))))
   plst)
 
; (define (foldl plst oper zero)
;   (if (empty? plst)
;     zero
;     (let for-all
;       ((curr (first plst))
;        (acc zero))
;       (if (not (has-next? plst curr))
;         (oper acc (peek plst curr))
;         (for-all (next plst curr)
;                  (oper acc (peek plst curr)))))))
; 
; (define (foldr plst oper zero)
;   (if (empty? plst)
;     zero
;     (let for-all
;       ((curr (first plst)))
;       (oper (peek plst curr)
;             (if (not (has-next? plst curr))
;               zero
;               (for-all (next plst curr)))))))
 
 (define (add-before! plst val . pos)
   (cond 
     ((and (empty? plst)
           (not (null? pos)))
      (error "illegal position (add-before!)" plst))
     ((or (null? pos) 
          (eq? (car pos) (first plst)))
      (attach-first! plst val))
     (else
      (attach-middle! plst val (previous plst (car pos)))))
   plst)
 
 (define (add-after! plst val . pos)
   (cond
     ((and (empty? plst)
           (not (null? pos)))
      (error "illegal position (add-after!)" plst))
     ((null? pos)
      (attach-last! plst val))
     (else
      (attach-middle! plst val (car pos))))
   plst)
 
 (define (delete! plst pos)
   (cond 
     ((eq? pos (first plst))
      (detach-first! plst))
     ((not (has-next? plst pos))
      (detach-last! plst pos))
     (else
      (detach-middle! plst pos)))
   plst))