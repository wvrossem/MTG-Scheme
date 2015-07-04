#!r6rs

(import 
 (rnrs base (6))
 (scheme class)
 (magicthegathering mtg)
 (magicthegathering gui-view)
 (magicthegathering controller))

(define view (new gui-view%))

(define mtg (new mtg% (view view)))

(send mtg set-up)

