#lang racket/base


(require racket/class
         racket/snip
         racket/format)

(require (only-in racket/draw make-bitmap bitmap-dc%)
         (only-in 2htdp/image image-width image-height)
         (only-in mrlib/image-core render-image))

(require racket-bricks/renderer
         racket-bricks/editor)

(provide brick-snip%
         (rename-out [brick-snip-class snip-class]))
   
(define brick-snip%
  (class snip%
    (inherit set-snipclass
             get-flags set-flags
             get-admin)
    (init-field [size 20.0])
    (init-field [extra 42.0])

    (init-field [code-s "(i am code)"])

    (init-field [code (read (open-input-string code-s))])
      
    (super-new)
    (set-snipclass brick-snip-class)
    (send (get-the-snip-class-list) add brick-snip-class)
    (set-flags (cons 'handles-events (get-flags)))
      
    (define/override (get-extent dc x y                                
                                 [w #f]
                                 [h #f]
                                 [descent #f]
                                 [space #f]
                                 [lspace #f]
                                 [rspace #f])
      (define img ((render-bricks) code))
      (define (maybe-set-box! b v) (when b (set-box! b v)))
      (maybe-set-box! w (+ 2.0 (image-width img)))
      (maybe-set-box! h (+ 2.0 (image-height img)))
      (maybe-set-box! descent 1.0)
      (maybe-set-box! space 1.0)
      (maybe-set-box! lspace 1.0)
      (maybe-set-box! rspace 1.0))



    (define (image->bitmap image dc x y)
      (let* ([width (image-width image)]
             [height (image-height image)]
             [bm (make-bitmap width height)])
        (render-image image dc x y)))
      
    (define/override (draw dc x y left top right bottom dx dy draw-caret)
      (define img ((render-bricks) code))
      ;(send dc draw-rectangle (+ x 1.0) (+ y 1.0) size size)
      (image->bitmap img dc x y))
      
    (define/override (copy)
      (new brick-snip%
           [size size]
           [extra extra]
           [code-s code-s]))

      
    (define/override (write f)
      
        
      (send f put size)
      (send f put extra)
      (send f put (string->bytes/utf-8 code-s)))

 
      
    (define/override (on-event dc x y editorx editory e)
      (when (send e button-down?)
        (thread
         (λ ()
           (set! code
                 (game-editor (cons 'CURSOR code)))
           (set! code-s (format "~S" code))
           (define admin (get-admin))
           (when admin
             (send admin resized this #t))))
        
        (define admin (get-admin))
        (when admin
          (send admin resized this #t))))))
   
(define brick-snip-class%
  (class snip-class%
    (inherit set-classname)
      
    (super-new)
    (set-classname (~s '((lib "main.rkt" "brick-snip")
                         (lib "wxme-brick-snip.rkt" "brick-snip"))))
      
    (define/override (read f)
       
        
      (define size-b (box 0.0))
      (send f get size-b)
      (define extra-b (box 0.0))
      (send f get extra-b)


      (define code-b (send f get-bytes))

      (define code-s (bytes->string/utf-8 code-b))
        
      (new brick-snip%
           [size (unbox size-b)]
           [extra (unbox extra-b)]
           [code-s code-s]))))
   
(define brick-snip-class (new brick-snip-class%))
