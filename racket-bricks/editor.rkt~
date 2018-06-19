#lang racket

(provide game-editor)

(require "./renderer.rkt")
(require "./model.rkt")


;TODO:
;
;  * After editing finished, inject into program (into snip?)
;  * Put instructions on page
;
;  * Clean up and polish for release.  Add to BB.



;Make videos.  Don't lose track of the big picture.  Sell.  Grow.  Evangelize.


(require game-engine
         game-engine-demos-common)

(define (on-circ i)
    (overlay
     i
     (circle 20
             "solid"
             "red")))

(define (game-editor d1)

  (define default-cursor (on-circ (rotate 0 (star 20 "solid" "black"))))
  
 
  (define clipboard #f)
  (define (get-clipboard)
    clipboard)
  (define (set-clipboard! thing)
    (set! clipboard thing)
    (displayln clipboard)
    (set! current-cursor (beside default-cursor
                                 (scale 0.25 ((render-bricks) thing)))))

  (define current-cursor default-cursor)

  (define WIDTH  1000)
  (define HEIGHT 1000)

  (define bg-entity
    (sprite->entity (space-bg-sprite WIDTH HEIGHT 100)
                    #:name     "bg"
                    #:position (posn 0 0)
                    #:components (static)))

  (define (edit-code f d)
    (lambda (g e)
      (define new-d (f d1 ))
      (set! d1 new-d)
      ((change-sprite (new-sprite (code-to-anim new-d))) g e)))

  (define (store-deletion-into-undo-buffer g e)
    (set-clipboard! (code-before-cursor d1))
    e)

  

  (define (code-to-anim d)
    ((render-bricks #:cursor current-cursor) d))


  (define (toggle-edit-mode g e)
    (if (code-edit-mode? e)
        (add-components
         (remove-all-button-behaviours g e)
         typing-mode-behaviours)
        (add-components
         (remove-all-button-behaviours g e)
         code-button-behaviours)))

  ;Poorly named.  "Edit mode" is when you are moving in and out of containers,
  ;  not when you are editing text.  Technically these are both different kinds of
  ;  edit modes.
  (define (enter-edit-mode g e)
    (if (code-edit-mode? e)
        e
        (add-components
         (remove-all-button-behaviours g e)
         code-button-behaviours)))
  
  (define up-arrow-behaviour
    (on-key 'up (edit-code move-cursor-up d1)))

  (define down-arrow-behaviour
    (on-key 'down (edit-code move-cursor-down d1)))

  (define right-arrow-behaviour
    (on-key 'right (lambda (g e)
                     (if (cursor-before-token? d1)
                         ((do-many
                           (edit-code move-cursor-in d1)
                           toggle-edit-mode) g e)
                         ((edit-code move-cursor-in d1) g e)))))

  (define left-arrow-behaviour
    (on-key 'left (edit-code move-cursor-out d1)))

  (define enter-arrow-behaviour
    (on-key 'enter (do-many
                    toggle-edit-mode
                    (edit-code move-cursor-out d1))))

  (define add-container-action
    (edit-code
     (curryr insert-before-cursor '())
     d1))

  (define (paste-cliboard-action g e)
    (define new-d (insert-before-cursor d1 (get-clipboard)))
    (set! d1 new-d)
    ((change-sprite (new-sprite (code-to-anim new-d))) g e))
  
  (define c-button-behaviour     
    (on-key 'c add-container-action))

  (define p-button-behaviour     
    (on-key 'p paste-cliboard-action))

  (define add-token-action
    (edit-code
     (curryr insert-before-cursor 'token)
     d1))
  
  (define t-button-behaviour
                    (on-key 't add-token-action))

  (define d-button-behaviour
    (on-key 'backspace (do-many
                        store-deletion-into-undo-buffer
                        (edit-code
                         delete-after-cursor
                         d1))))

  (define wheel-up-behaviour
    (on-key '|[| (lambda (g e)
                        (match-define (posn x y) (get-component e posn?))
                        (update-entity e posn? (posn x (- y 50))))))

  (define wheel-down-behaviour
    (on-key '|]| (lambda (g e)
                          (match-define (posn x y) (get-component e posn?))
                          (update-entity e posn? (posn x (+ y 50))))))

  (define (remove-all-button-behaviours g e) ;Removes everything except the e button behaviour
    (remove-component e (lambda (c)
                          (and
                           (on-key? c)
                           (not (eq? enter-button-behaviour c))))))

  (define (code-edit-mode? e)
    (define ret (get-component e up-arrow-behaviour))
    (displayln ret)
    ret) ;If it's responding to up arrow events, it's in edit mode


  
  (define enter-button-behaviour
    (on-key 'enter (do-many
                     (edit-code move-cursor-out d1)
                     enter-edit-mode)))

  (define code-button-behaviours
    (list
     up-arrow-behaviour
     down-arrow-behaviour
     left-arrow-behaviour
     right-arrow-behaviour
     c-button-behaviour
     t-button-behaviour
     d-button-behaviour
     p-button-behaviour
     wheel-up-behaviour
     wheel-down-behaviour))

  (define (add-letter-action letter)
    (on-key
     (string->symbol letter)
     (edit-code
      (curryr add-letter-before-cursor letter)
      d1)))

  
  (define letters
    (map (lambda (l)
           (format "~a" l))
           '(a b c d e f g h i j k l m n o p q r s t u v w x y z
             A B C D E F G H I J K L M N O P Q R S T U V W X Y Z
             0 1 2 3 4 5 6 7 8 9
             * - / + = > < \
             |"| |#| |,| |.| : | |
             right left
             backspace)))
  
  (define typing-mode-behaviours
    (map add-letter-action letters)) 
  
  (define (code-entity d1)
    (sprite->entity (code-to-anim d1)
                    #:name       "code"
                    #:position   (posn 200 300)
                    #:components
                    code-button-behaviours
                    enter-button-behaviour))


    
  (start-game (code-entity d1)
              bg-entity)

  (extract-cursor-completely d1)) ;Return the code after editing...

(module+ test
  (game-editor
   '(CURSOR i am code)))


