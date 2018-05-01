#lang racket

(module reader syntax/module-reader
  racket-bricks/racket-bricks-module
  #:wrapper1 (lambda (t)
               (define exp-t (t))

               (define (brick-snip? b)
                 (and
                  (object? b)
                  (member 'code-s (field-names b))))

               (define (replace-easter-eggs2 syn)
                 (define thing (if (syntax? syn)
                                   (syntax->datum syn)
                                   syn))

                 ;is-a? doesn't work?  For some reason the class is returned as #f from
                 ;   (object-info thing)

                 (cond [(brick-snip? thing)
                        (datum->syntax (list-ref exp-t 0)
                                    (read (open-input-string (get-field code-s thing))))]
                       [(list? thing) (map replace-easter-eggs2 thing)]
                       [else syn]))
               
               (map replace-easter-eggs2 exp-t))
   
  (require 2htdp/image
           racket/class))
