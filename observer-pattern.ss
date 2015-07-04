#!r6rs

(library
 (observer-pattern)
 (export subject% observer% manager%)
 (import (rnrs base (6))
         (rnrs control (6))
         (scheme class)
         (only (scheme list) flatten)
         (prefix (scheme base) s:)
         (prefix (magicthegathering datastructures positional-list adt) pl:))
 
 ; Class name: observer
 ; Superclass: object%  
 ; Description: An observer, observes a subject
 ; Attributes: - public: none
 ;             - private: none
 ; Methods:
 ;  - public:
 ;     * update: 
 ;         arguments: none
 ;         output: /
 ;         description: subclasses should override this function
 ;  - private: none
 
 (define observer%
   (class object%
     (super-new)
     
     ; ( -> )
     (define/public (update . update-args)
       (error "override this method - update" this))))
 
 ; Class name: subject
 ; Superclass: object%
 ; Description: Subjects notify their observers when an update happens
 ; Attributes: - public: name
 ;             - private: observers = positional list
 ; Methods:
 ;  - public:
 ;     * attach: 
 ;         arguments: observer
 ;         output: none
 ;         description: add a new observer to the obsrevers list
 ;     * detach: 
 ;         arguments: observer
 ;         output: none
 ;         description: remove an observer from the obsrevers list
 ;     * notify: 
 ;         arguments: update-name, optionally arguments
 ;         output: none
 ;         description: send a message to each observer to call its update method                     
 ;  - private: none
 
 (define subject%
   (class object%
     (init-field name)
     (super-new)
     (field (observers (pl:new eq?)))
     
     ; ( observer% -> )
     (define/public (attach observer)
       (pl:add-before! observers observer))
     
     ; ( observer% -> )
     (define/public (detach observer)
       (let ((pos (pl:find observers observer)))
         (pl:delete! observers pos)))
     
     ; ( -> )
     (define/public (notify update-name . update-args)
       (if (null? update-args)
           (pl:for-each observers (lambda (o) (send o update update-name)))
           (pl:for-each observers (lambda (o) (send o update update-name (flatten update-args))))))))
 
 ; Class name: manager 
 ; Superclass: object%  
 ; Description: A manager, manages subjects
 ;    Arguments 'subject' can also be symbols here (= subject name)
 ; Attributes: - public: none
 ;             - private: subjects = positional list
 ; Methods:
 ;  - public:
 ;     * find-subject: 
 ;         arguments: subject
 ;         output: subject or #f
 ;         description: find a subject in the subjects list
 ;     * add-subject: 
 ;         arguments: subject
 ;         output: none
 ;         description: add a new subject to the subjects list 
 ;     * remove-subject: 
 ;         arguments: subject
 ;         output: none
 ;         description: remove a subject from the subjects list      
 ;     * add-observer: 
 ;         arguments: observer, subject
 ;         output: none
 ;         description: attach an observer to a subject. If the subject is not yet in the list,
 ;            it will be added and the observer will be attached
 ;     * notify-subject: 
 ;         arguments: subject
 ;         output: none
 ;         description: notify all the observers of a certain subject 
 ;  - private: none
 
 (define manager%
   (class object%
     (super-new)
     ; A list of subjects
     (define subjects (pl:new (lambda (s1 s2) (or (eq? s1 (get-field name s2)) (eq? s1 s2)))))
     
     ; ( subject% -> subject%/#f)
     (define/public (find-subject subject)
       (let ((pos (pl:find subjects subject)))
         (if pos
             (pl:peek subjects pos)
             #f)))
     
     ; ( subject% -> )
     (define/public (add-subject subject)
       (unless (find-subject subject)
         (if (symbol? subject)
             (pl:add-before! subject (new subject% (name subject)))
             (pl:add-before! subjects subject))))
     
     ; ( subject% -> )
     (define/public (remove-subject subject)
       (let ((pos (pl:find subjects subject)))
         (if pos
             (pl:delete! subjects pos))))
     
     ; ( observer% subject% -> )
     (define/public (add-observer observer subject)
       (let ((subj (find-subject subject)))
         (if subj
             (send subj attach observer)
             (begin (add-subject subject)
                    (add-observer observer subject)))))
     
     ; ( subject% -> )
     (define/public (notify-observers subject update-name . args)
       (let ((subj (find-subject subject)))
         (if subj
             (if (null? args)
                 (send subject notify update-name)
                 (send subject notify update-name args))))))))
 
 