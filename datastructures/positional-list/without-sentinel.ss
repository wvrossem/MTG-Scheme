;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*                                                                 *-*-
;-*-*                Positional List Without Sentinel                 *-*-
;-*-*                                                                 *-*-
;-*-*                       Wolfgang De Meuter                        *-*-
;-*-*                 2008 Programming Technology Lab                 *-*-
;-*-*                   Vrije Universiteit Brussel                    *-*-
;-*-*                                                                 *-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-

#lang r6rs

(library 
 (positional-list-without-sentinel)
 (export new positional-list-tag find
         attach-first! attach-last! attach-middle!
         detach-first! detach-last! detach-middle!
         length empty? full? update! peek
         first last has-next? has-previous? next previous
         shuffle peek-pos)
 (import (except (rnrs base (6)) length list? map for-each)
         ;(a-d positional-list single-linked))
         (magicthegathering datastructures positional-list single-linked-gierig))
         ;(a-d positional-list double-linked-gierig))
         ;(magicthegathering datastructures positional-list vectorial))
         ;(a-d positional-list double-linked))
         ;(a-d positional-list augmented-double-linked))

 (define (find plst key)
   (define ==? (equality plst))
   (if (empty? plst)
     #f
     (let sequential-search
       ((curr (first plst)))
       (cond
         ((==? key (peek plst curr)) 
          curr)
         ((not (has-next? plst curr))
          #f)
         (else 
          (sequential-search (next plst curr))))))))
