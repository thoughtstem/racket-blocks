#lang racket/base
(require racket/class
         racket/format
         wxme
         pict)
   
(provide reader)
   
(define brick-reader%
  (class* object% (snip-reader<%>)
    (define/public (read-header version stream) (void))
    (define/public (read-snip text-only? version stream)
      (define size (send stream read-inexact "brick-snip"))
      (define extra (send stream read-inexact "brick-snip"))
      (define code-s (bytes->string/utf-8
                      (send stream read-bytes "msg")))

      (cond
        [text-only?
         (string->bytes/utf-8 (~s `(list ,size ,extra ,code-s)))]
        [else
         (new brick-readable
              [size size]
              [extra extra]
              [code-s code-s])]))
    (super-new)))
   
(define brick-readable
  (class* object% (readable<%>)
    (init-field size)
    (init-field extra)
    (init-field code-s)
    
    (define/public (read-special source line column position)
      (datum->syntax #f
                     (list size extra code-s)
                     (vector source line column position 1)
                     #f))
    (super-new)))
   
(define reader (new brick-reader%))
