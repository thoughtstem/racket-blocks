#lang racket

(provide render-bricks)

(require 2htdp/image)
(require "./model.rkt")

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
                        #:cursor ( CURSOR (star PADDING "solid" CURSOR-COLOR)))


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

 
  (define (brick-container color name-image inner-image)
    (define top (outline (rectangle (+ PADDING (image-width name-image))
                                    PADDING #;(+ PADDING (image-height name-image))
                                    "solid"
                                    color)))

    (define bottom (outline (rectangle (/ (image-width inner-image) 2)
                                       PADDING
                                       "solid"
                                       color)))

    (define column (outline (rectangle PADDING
                                       (+ (image-height name-image)
                                          (image-height inner-image))
                                       "solid"
                                       color)))

    (define main-image (above/align "left"
                                    top
                                    (beside column (above/align "left" name-image inner-image))
                                    bottom))

    (define fixer (circle (/ PADDING 2) "solid" color))
  
    (define top-fixed (overlay/xy
                       fixer
                       0 (+ (/ (image-height fixer) 2)
                            (- (image-height top)))
                       main-image))

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
  
  (define (token->text t)
    (if (token-contains-cursor? t)
        (let [(split-by-cursor (split-cursor (format "~a" t)))]
          (beside
           (if (string? t) (text "\"" 15 "black") blank-image)
           (text (first split-by-cursor) 15 "black")
           (scale 0.5 CURSOR)
           (text (second split-by-cursor) 15 "black")
           (if (string? t) (text "\"" 15 "black") blank-image)))
        (text (format "~s" t) 15 "black")))

  (define (token->slot t (color (token->slot-color t))
                         (border? #t))
    (if (cursor? t)
        CURSOR
        (boxify color BRICK-OUTLINE (token->text t)
                border?)))

  (define (leaf->bricks d)
    (define comb (make-safe
                  (if (member 'CURSOR d)
                      (curry above/align "left")
                      (curry beside/align "middle"))))
    (cond [(empty? d) (boxify CONTAINER-COLOR BRICK-OUTLINE
                              (circle PADDING "solid" "transparent"))]
          [(= 1 (length d)) (boxify (token->container-color (first d)) BRICK-OUTLINE
                                    (token->slot (first d) CONTAINER-COLOR #f))]
          [else (boxify (token->container-color (first d)) BRICK-OUTLINE
                        (comb (token->slot (first d) (token->container-color (first d)) #f)
                              (apply comb (map token->slot (rest d)))))]))


  (define (make-safe f)
    (lambda (arg . args)
      (if (empty? args)
          arg
          (apply f (cons arg args)))))

  (define (data->bricks d)
    (cond [(cursor? d) CURSOR]
          [(leaf? d) (leaf->bricks d)]
          [(token? d) (token->slot d)]
          [(token? (first d))
           (let [(color (token->container-color (first d)))]
             (brick-container color
                              (token->slot (first d) color #f)
                              (apply (make-safe (curry  above/align "left"))
                                     (map data->bricks (rest d)))))]
          [(list? d)
           (brick-container CONTAINER-COLOR
                            (rectangle PADDING 1 "solid" "transparent")
                            (apply (make-safe (curry  above/align "left"))
                                   (map data->bricks d)))]
          [else (apply (make-safe (curry  above/align "left")) (map data->bricks d))]))

  data->bricks)

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

  ((render-bricks) d3))
