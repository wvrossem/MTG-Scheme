;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*                                                                 *-*-
;-*-*                 Stacks (Linked Implementation)                  *-*-
;-*-*                                                                 *-*-
;-*-*                       Wolfgang De Meuter                        *-*-
;-*-*                 2008 Programming Technology Lab                 *-*-
;-*-*                   Vrije Universitent Brussel                    *-*-
;-*-*                                                                 *-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-

#lang r6rs

(library
 (stack)
 (export new size stack? push! pop! top empty? full? top-to-bottom shuffle find for-each delete!)
 (import (except (rnrs base (6)) map list length for-each)
         (rnrs io simple)
         (rnrs mutable-pairs)
         (prefix (magicthegathering datastructures positional-list adt) plist:))
 
 (define stack-tag 'linked-stack)
 (define (make)
   (cons stack-tag (plist:new eq?)))
 (define (plist stack)
   (cdr stack))
 
 (define (new)
   (make))
 
 (define (stack? any)
   (and (pair? any)
        (eq? (car any) stack-tag)))
 
 (define (size stack)
   (define plst (plist stack))
   (plist:length plst))
 
 (define (push! stack val)
   (plist:add-before! (plist stack) val)
   stack)
 
 (define (top stack)
   (define plst (plist stack))
   (if (= (plist:length plst) 0)
     (error "stack empty (top)"stack))
   (plist:peek plst (plist:first plst)))
 
 (define (pop! stack)
   (define plst (plist stack))
   (define first-position (plist:first plst))
   (if (= (plist:length plst) 0)
     (error "stack empty (pop)" stack))
   (let ((val (plist:peek plst first-position)))
     (plist:delete! plst first-position)
     val))
 
 (define (empty? stack)
   (define plst (plist stack))
   (plist:empty? plst))
 
 (define (full? stck)
   (define plst (plist stck))
   (plist:full? plst))
 
 (define (top-to-bottom stack)
   (define plst (plist stack))
   (define pos (plist:first plst))
   (define el (plist:peek plst pos))
   (plist:delete! plst pos)
   (plist:add-after! plst el))
 
 (define (shuffle stack)
   (define plst (plist stack))
   (plist:shuffle plst))
 
 (define (find stack el)
   (define plst (plist stack))
   (plist:find plst el))
 
 (define (for-each stack func)
   (define plst (plist stack))
   (plist:for-each plst func))
 
 (define (delete! stack pos)
   (define plst (plist stack))
   (begin
     (plist:delete! plst pos)
     (plist:peek plst pos))))
   