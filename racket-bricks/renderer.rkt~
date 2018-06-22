#lang racket

(provide render-bricks
         render-text)

(require 2htdp/image)
(require "./model.rkt")

;Not tail recursive... rewrite?
(define (map-with-index f l (i 0))
  (if (empty? l)
      '()
      (cons (f (first l) i)
            (map-with-index f (rest l) (add1 i)))))

(define (render-bricks  ( PADDING 20)
                        ( SLOT-COLOR (color 220 220 220))
                        ;( BRICK-COLOR (color 169 233 95))
                        ( CONTAINER-COLOR (color 255 135 104))
                        ( BRICK-OUTLINE (pen "black" 1 "solid" "butt" "bevel"))
                        ( FUNCTION-DEF-COLOR (color 223 91 142))
                        ( NUMBER-SLOT-COLOR  (color 96 160 163))
                        ( STRING-SLOT-COLOR  (color 121 205 126))
                        ( BOOLEAN-SLOT-COLOR (color 255 199 150))
                        ( CURSOR-COLOR "red")
                        ( CONTAINER rectangle)
                        #:container-padding (CONTAINER-PADDING PADDING)
                        #:container-padding-left (CONTAINER-PADDING-LEFT PADDING)
                        #:cursor ( CURSOR (star PADDING "solid" CURSOR-COLOR))
                        #:show-parens? (SHOW-PARENS? #f))

  (define (text s size color)
    (text/font s size color
               "modern" 'swiss 'normal 'bold #f))
  
  ;Used to color an entire container based on the first token (e.g. (define ...))
  (define (token->container-color t)
    (cond [(eq? t 'define) FUNCTION-DEF-COLOR]
          [(eq? t 'if) FUNCTION-DEF-COLOR]
          [else CONTAINER-COLOR]))

  ;Used to color a slot based on its type
  (define (token->slot-color t)
    (cond [(number? t) NUMBER-SLOT-COLOR]
          [(string? t) STRING-SLOT-COLOR]
          [(boolean? t) BOOLEAN-SLOT-COLOR]
          [else SLOT-COLOR]))


  ;END CONFIG

  (define (outline i)
    (overlay
     i
     (rectangle (image-width i)
                (image-height i)
                "outline"
                BRICK-OUTLINE)))

  (define LEFT-PAREN  (text "(" 20 "brown"))
  (define RIGHT-PAREN (text ")" 20 "brown"))
  (define (ending-parens d)
    (if (= 0 d) empty-image
        (img-if SHOW-PARENS?
                (apply (make-safe beside)
                       (map (thunk* RIGHT-PAREN)
                            (range d))))))

  (define (img-if b i)
    (if b
        i
        empty-image))
 
  (define (brick-container color name-image inner-image depth breadth total)

    
    (define top (outline (rectangle (+ CONTAINER-PADDING-LEFT  (image-width name-image))
                                    PADDING 
                                    "solid"
                                    color)))

    (define bottom (outline (rectangle (/ (image-width inner-image) 2)
                                       CONTAINER-PADDING
                                       "solid"
                                       color)
                             
                             ))

    (define column (outline (overlay/align "right" "top"
                             (img-if SHOW-PARENS? LEFT-PAREN
                                     #;(beside (text (format "~a/~a" breadth total) 20 "black") LEFT-PAREN))
                             (rectangle CONTAINER-PADDING-LEFT
                                       (+ (image-height name-image)
                                          (image-height inner-image))
                                       "solid"
                                       color))))

    (define main-image (above/align "left"
                                    top
                                    (beside column (above/align "left" name-image inner-image))
                                    bottom))

    (define fixer (circle (/ CONTAINER-PADDING 2) "solid" color))
  
    (define top-fixed (overlay/xy
                       fixer
                       0 (+ (/ (image-height fixer) 2)
                            (- (image-height top)))
                       (beside/align "bottom"
                                     main-image
                                     (img-if SHOW-PARENS? empty-image
                                             #;(ending-parens depth)))))

    (define final (overlay/xy
                   fixer
                   0 (+ (- (image-height column)) (/ (image-height fixer) 2)
                        (- (image-height top)))
                   top-fixed))

    final)

  (define (boxify color outline i (border? #t))
    (underlay
     (CONTAINER (+ PADDING (image-width i))
                (+ PADDING (image-height i))
                "solid" color)
     (CONTAINER (+ PADDING (image-width i))
                (+ PADDING (image-height i))
                "outline" outline)
     i))


  (define (cursor? d)
    (eq? d 'CURSOR))

  (define (token? d)
    (not (list? d)))

  (define (leaf? d)
    (if  (list? d)
         (not (ormap list? d))
         #f))

  (define blank-image (circle 1 "solid" "transparent"))


  (define (token->color t)
    (cond [(number? t) "darkgreen"]
          [(string? t) "darkgreen"]
          [(boolean? t) "darkgreen"]
          [else "darkblue"]))
  
  (define (token->text t)
    (if (token-contains-cursor? t)
        (let [(split-by-cursor (split-cursor (format "~a" t)))]
          (beside
           (if (string? t) (text "\"" 15 "black") blank-image)
           (text (first split-by-cursor) 15 "black")
           (scale 0.5 CURSOR)
           (text (second split-by-cursor) 15 "black")
           (if (string? t) (text "\"" 15 "black") blank-image)))
        (text (format "~s" t) 15 (token->color t))))

  (define (token->slot t (color (token->slot-color t))
                         (border? #t))
    (if (cursor? t)
        CURSOR
        (boxify color BRICK-OUTLINE (token->text t)
                border?)))

  (define (leaf->bricks d depth breadth total)
    
    
    (define comb (make-safe
                  (if (member 'CURSOR d)
                      (curry above/align "left")
                      (curry beside/align "middle"))))
    
    (define ret
      (cond [(empty? d) (boxify CONTAINER-COLOR BRICK-OUTLINE
                              (circle PADDING "solid" "transparent"))]
          [(= 1 (length d)) (boxify (token->container-color (first d)) BRICK-OUTLINE
                                    (token->slot (first d) CONTAINER-COLOR #f))]
          [else (boxify (token->container-color (first d)) BRICK-OUTLINE
                        (comb (token->slot (first d) (token->container-color (first d)) #f)
                              (apply comb (map token->slot (rest d)))))]))
    
    (beside #;(text (format "~a/~a" breadth total) 20 "black")
            (img-if SHOW-PARENS? LEFT-PAREN)
            ret
            (img-if SHOW-PARENS? RIGHT-PAREN))
    )


  (define (make-safe f)
    (lambda (arg . args)
      (if (empty? args)
          arg
          (apply f (cons arg args)))))

  (define (data->bricks d (depth 0) (breadth 0) (parent #f))
    (define total (if parent (length parent) 1))
    (define (recurse child b)
      (data->bricks child
                    (add1 depth)
                    (add1 b) ;We add one here because we recurse over (rest ...), so we account for the skipped (first ...)
                    d))
    
    (define ret
      (cond [(cursor? d) CURSOR]
          [(leaf? d) (leaf->bricks d depth breadth total)]
          [(token? d) (token->slot d)]
          [(token? (first d))
           (let [(color (token->container-color (first d)))]
             (brick-container color
                              (token->slot (first d) color #f)
                              (apply (make-safe (curry  above/align "left"))
                                     (map-with-index recurse (rest d)))
                              depth
                              breadth
                              total))]
          [(list? d)
           (brick-container CONTAINER-COLOR
                            (rectangle PADDING 1 "solid" "transparent")
                            (apply (make-safe (curry  above/align "left"))
                                   (map-with-index recurse d))
                            depth
                            breadth
                            total)]
          [else (apply (make-safe (curry  above/align "left")) (map-with-index recurse d))]))

    (beside/align "bottom"
                  ret
                  (img-if (and  SHOW-PARENS?
                                parent
                               (= (add1 breadth)
                                   total))
                          (above RIGHT-PAREN
                                 (rectangle 1 (- PADDING 3) "solid" "transparent")))))

  data->bricks)





(define (render-text)
  (define clear (color 0 0 0 0))
  (render-bricks 10
                 clear
                 clear
                 clear
                 
                 clear ;(pen clear 1 "solid" "butt" "bevel")
                 clear
                 clear
                 clear
                 clear
                 ;( CURSOR-COLOR "red")
                 ;( CONTAINER rectangle)
                 #:container-padding 0
                 #:container-padding-left 20
                 #:cursor (circle 5 "solid" "red")
                 #:show-parens? #t
                 ))


(module+ test
  (define d1 '(beside
               CURSOR
               (triangle 20 "solid" "green")
               (above (circle 20 "solid" "red")
                      (circle 20 "solid" "blue"))))

  (define d2 '(circle 20 "solid" "red"))

  (define d3 `(define my-thing
                ,d1))


  (define d4 '(define bg-entity
                (sprite->entity (space-bg-sprite WIDTH HEIGHT 100)
                                #:name
                                "bg"
                                #:position
                                (posn 0 0)
                                #:components
                                (static))))

  (define d5
    '(beside
      (rotate 45
              (above (circle 30 "solid" "red")
                     (circle 30 "solid" "red")))
      (circle 30 "solid" "red")
      (above (circle 30 "solid" "red")
             (circle 30 "solid" "red"))))


  ((render-bricks) (insert-before-cursor d3 'test))
  ((render-bricks) (move-cursor-up
                    (insert-before-cursor d3 'test)))
  ((render-bricks) (move-cursor-in
                    (move-cursor-down
                     (insert-before-cursor d3 'test))))
  ((render-bricks) '(test ()))
  ((render-bricks) '(test (())))
  ((render-bricks) '(test ((()))))

  ((render-bricks) (move-cursor-in
                    (move-cursor-up
                     (insert-before-cursor d3 'test))))
  
  ((render-bricks) (add-letter-before-cursor
                    (move-cursor-in
                     (move-cursor-up
                      (insert-before-cursor d3 'test)))
                    "a"))

  ((render-bricks) d3)
  
  ((render-text) d3)
  ((render-text) d5)
  )


