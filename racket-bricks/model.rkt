#lang racket

(provide (all-defined-out))

(define (token-contains-cursor? t)
  (define ts (format "~a" t))
  (and (not (list? t))
       (string-contains? ts ":CURSOR:")
       (split-cursor ts)))

(define (code-before-cursor p1)
  (cond [(not (list? p1)) #f]
        [(index-of p1 'CURSOR)
         (let [(i (index-of p1 'CURSOR))]
         (if (= 0 i)
             #f
             (list-ref p1 (sub1 i))))]
        [else (ormap code-before-cursor p1)]))

(module+ test
  (check-equal? 'a (code-before-cursor '(a CURSOR c)))
  (check-equal? '(a b) (code-before-cursor '((a b) CURSOR c)))
  (check-equal? #f (code-before-cursor '(CURSOR a b c))))


(define (insert-before-cursor p1 p2)
  (cond [(not (list? p1)) p1]
        [(member 'CURSOR p1)
         (append (take p1 (index-of p1 'CURSOR))
                 (list p2)
                 (drop p1 (index-of p1 'CURSOR)))]
        [else (map (curryr insert-before-cursor p2) p1)]))

(define (swap p1 p2 li)
  (define f (min p1 p2))
  (define l (max p1 p2))
  (if (= f l) li
      (append (take li f)
              (list (list-ref li l))
              (list (list-ref li f))
              (drop li (add1 l)))))

(module+ test
  (check-equal? '(a c b) (swap 1 2 '(a b c)))
  (check-equal? '(a b c) (swap 0 0 '(a b c)))
  (check-equal? '(b a c) (swap 1 0 '(a b c))))


(define (move-in p1 li)
  (cond [(>= p1 (- (length li) 1)) li] ;If last item in list, do nothing
        [else (let* [(target (list-ref li (add1 p1)))
                     (source (list-ref li p1))
                     (first-part (if (= 0 p1)
                                     '()
                                     (take li p1)))
                     (last-part (if (= p1 (- (length li) 2))
                                    '()
                                    (drop li (+ 2 p1))))]
                (append first-part
                        (if (list? target)
                            (list (cons source target))
                            (list (insert-cursor target (format "~a" source))))
                        last-part))]))


(module+ test
  (require rackunit)
  (check-equal? '(above (CURSOR circle 10 solid red)
                        (circle 10 solid red))
                (move-in 1 '(above CURSOR
                                   (circle 10 solid red)
                                   (circle 10 solid red))))
  (check-equal? '((a b c)) (move-in 0 '(a (b c))))
  (check-equal? '(a (b c)) (move-in 1 '(a b (c))))
  (check-equal? '(b:a: c)  (move-in 0 '(a b c)))
  (check-equal? '(a b c)   (move-in 2 '(a b c)))
  
  (check-equal? '(b:CURSOR: c) (move-in 0 '(CURSOR b c)))

  )

(define (insert-at l p t)
  (append (take l p)
          (list t)
          (drop l p)))



(define (move-out p1 li)
  (define target (list-ref li p1))
  (define new-target (filter (lambda (x) (not (eq? 'CURSOR x))) target))
  (insert-at
   (list-set li p1 new-target)
   p1
   'CURSOR))


(module+ test
  (check-equal? '(a CURSOR (c)) (move-out 1 '(a (CURSOR c))))
  (check-equal? '(CURSOR (b c)) (move-out 0 '((CURSOR b c)))))

(define (delete-before l p)
  (if (= p 0)
      l
      (append (take l (sub1 p))
              (drop l p))))

(module+ test
  (check-equal? '(b c)       (delete-before '(a b c) 1 ))
  (check-equal? '(a (b c) d) (delete-before '(a (b c) d) 0 ))
  (check-equal? '(a d)       (delete-before '(a (b c) d) 2 )))

(define (remove-last-letter s)
  (if (<= (string-length s) 1)
      ""
      (substring s
                 0
                 (- (string-length s) 1))))

(define (string->symbol-handle-empty s)
  (if (string=? s "")
      ""
      (string->symbol s)))

(define (up-to-last-letter s)
  (substring s 0 (sub1 (string-length s))))

(define (last-letter s)
  (substring s
             (sub1 (string-length s))
             (string-length s)))

(define (all-but-first-letter s)
  (substring s 1 (string-length s)))

(define (first-letter s)
  (substring s 0 1))

(define (letter-move-right two-strings)
  (define f (first two-strings))
  (define s (second two-strings))
  (if (= 0 (string-length f))
      two-strings
      (list (up-to-last-letter f)
            (string-append (last-letter f) s))))

(define (letter-move-left two-strings)
  (define f (first two-strings))
  (define s (second two-strings))
  (if (= 0 (string-length s))
      two-strings
      (list (string-append f
                           (first-letter s))
            (all-but-first-letter s))))

(define (delete-letter-left two-strings)
  (define f (first two-strings))
  (define s (second two-strings))
  (if (= 0 (string-length f))
      two-strings
      (list
       (up-to-last-letter f)
       s)))

(define (remove-cursor s)
  (string-replace s ":CURSOR:" ""))

(define (split-cursor s)
  (cond [(string-prefix? s ":CURSOR:") (list "" (remove-cursor s))]
        [(string-suffix? s ":CURSOR:") (list (remove-cursor s) "")]
        [else (string-split s ":CURSOR:")]))

(define (cursor-action f s)
  (define ss (f (split-cursor s)))
  (string-append
     (first ss)
     ":CURSOR:"
     (second ss)))

(define (add-letter-before-string token-with-cursor letter)
  (match letter
         ["right" (cursor-action letter-move-left token-with-cursor)]
         ["left" (cursor-action letter-move-right token-with-cursor)]
         ["backspace" (cursor-action delete-letter-left token-with-cursor)]
         [else (string-replace token-with-cursor ":CURSOR:" (string-append letter ":CURSOR:"))]))

(define (add-letter-before-symbol token-with-cursor letter)
  (string->symbol
   (add-letter-before-string (format "~a" token-with-cursor) letter)))

(define (add-letter-before token-with-cursor letter)
  (cond [(and
          (string? token-with-cursor)
          (string=? letter "\""))
         (string->symbol token-with-cursor)]
        [(and
          (symbol? token-with-cursor)
          (string=? letter "\""))
         (format "~a" token-with-cursor)]
        [else
         (if (string? token-with-cursor)
             (add-letter-before-string token-with-cursor letter)
             (add-letter-before-symbol token-with-cursor letter))]))



(define (add-letter-before-cursor p1 letter)
  (cond [(and
          (not (list? p1))
          (token-contains-cursor? p1))
         (add-letter-before p1 letter)]
        [(not (list? p1)) p1]
        [else (map (curryr add-letter-before-cursor letter) p1)]))

(module+ test
  (check-equal? 'ab:CURSOR:c       (add-letter-before 'a:CURSOR:c "b"))
  (check-equal? 'abc:CURSOR:       (add-letter-before 'ab:CURSOR: "c"))
  (check-equal? 'a:CURSOR:bc       (add-letter-before ':CURSOR:bc "a"))
  (check-equal? '11:CURSOR:0       (add-letter-before '1:CURSOR:0 "1"))
  
  (check-equal? ':CURSOR:bc        (add-letter-before ':CURSOR:bc "backspace"))
  (check-equal? 'a:CURSOR:bc       (add-letter-before ':CURSOR:abc "right"))
  (check-equal? 'a:CURSOR:bc       (add-letter-before 'ab:CURSOR:c "left"))
  (check-equal? ':CURSOR:abc       (add-letter-before ':CURSOR:abc "left"))
  (check-equal? 'abc:CURSOR:       (add-letter-before 'abc:CURSOR: "right"))

  (check-equal? "a:CURSOR:bc"     (add-letter-before ":CURSOR:abc" "right"))
  (check-equal? "a:CURSOR:bc"     (add-letter-before "ab:CURSOR:c" "left"))
  (check-equal? ":CURSOR:abc"     (add-letter-before ":CURSOR:abc" "left"))
  (check-equal? "abc:CURSOR:"     (add-letter-before "abc:CURSOR:" "right"))

  (check-equal? 'abc:CURSOR:       (add-letter-before "abc:CURSOR:" "\""))
  (check-equal? "abc:CURSOR:"     (add-letter-before 'abc:CURSOR: "\"")))


(define (symbol-numeric? s)
  (number? (string->number (symbol->string s))))

(define (symbol-booleanic? s)
  (define clean (string-replace (symbol->string s) "|" "" #:all? #t ))
  (or (string=? clean "#f")
      (string=? clean "#t")))

(define (symbol->number s)
  (string->number (string-replace (symbol->string s) "|" "" #:all? #t )))

(define (symbol->boolean s)
  (define clean (string-replace (symbol->string s) "|" "" #:all? #t ))
  (if (string=? "#t" clean)
      #t
      #f))


(define (extract-cursor token)
  (define new-token
    (cond [(string? token) (string-replace token ":CURSOR:" "")]
          [(number? token) token]
          [(boolean? token) token]
          [else
           (string->symbol
            (string-replace (symbol->string token) ":CURSOR:" ""))]))
  
  (cond [(and (symbol? new-token)
              (symbol-numeric? new-token))
         (symbol->number new-token)]
        [(and (symbol? new-token)
              (symbol-booleanic? new-token))
         (symbol->boolean new-token)]
        [else new-token]))


(module+ test
  (check-equal? 'ac       (extract-cursor 'a:CURSOR:c))
  (check-equal? 'ab       (extract-cursor 'ab:CURSOR:))

  (check-equal? 10       (extract-cursor '10:CURSOR:))
  (check-equal? 100       (extract-cursor '10:CURSOR:0))

  (check-equal? "100"       (extract-cursor "10:CURSOR:0"))
  (check-equal? "abc"       (extract-cursor "ab:CURSOR:c"))

  (check-equal? #t       (extract-cursor '|#:CURSOR:t|))
  (check-equal? #t       (extract-cursor '|#t:CURSOR:|))

  (check-equal? #t       (extract-cursor ':CURSOR:#t))
  
  (check-equal? #t       (extract-cursor #t))
  (check-equal? 10       (extract-cursor 10)))

(define (remove-all-cursors p1)
  (if (not (list? p1))
      p1
      (map remove-all-cursors
           (filter (λ (x) (not (eq? 'CURSOR x))) p1))))

(define (extract-cursor-completely p1)
  (remove-all-cursors
   (if (not (list? p1))
      (extract-cursor p1)
      (map extract-cursor-completely p1))))

(module+ test
  (check-equal? 'ac       (extract-cursor-completely 'a:CURSOR:c))

  (check-equal? '(ac)       (extract-cursor-completely '(a:CURSOR:c)))

  (check-equal? '((a b c dd) ac)
                (extract-cursor-completely '(CURSOR (a b c CURSOR d:CURSOR:d) a:CURSOR:c)))
  )


(define (insert-cursor token (cursor "CURSOR"))
  (define string-with-cursor (string-append (format "~a" token)
                                            (string-append ":" cursor ":")))
  (cond [(string? token) string-with-cursor]
        [(eq? token 'token) ':CURSOR:]
        [else (string->symbol string-with-cursor)]))

(module+ test
  (check-equal? 'ac:CURSOR:       (insert-cursor 'ac))  
  (check-equal? '10:CURSOR:       (insert-cursor 10))
  (check-equal? "100:CURSOR:"    (insert-cursor "100"))
  (check-equal? ':CURSOR:    (insert-cursor 'token)))


(define (move-cursor-up p1)
  (cond [(not (list? p1)) p1]
        [(member 'CURSOR p1)
         (let* [(pos1 (index-of p1 'CURSOR))
                (pos2 (max 0 (- pos1 1)))]
           (swap pos1 pos2 p1))]
        [else (map move-cursor-up p1)]))

(define (move-cursor-down p1)
  (cond [(not (list? p1)) p1]
        [(member 'CURSOR p1)
         (let* [(pos1 (index-of p1 'CURSOR))
                (pos2 (min (- (length p1) 1) (+ pos1 1)))]
           (swap pos1 pos2 p1))]
        [else (map move-cursor-down p1)]))


(define (move-cursor-in p1)
  (cond [(not (list? p1)) p1]
        [(member 'CURSOR p1)
         (let* [(pos1 (index-of p1 'CURSOR))]
           (move-in pos1 p1))]
        [else (map move-cursor-in p1)]))

(define (cursor-before-token? p1)
  (cond [(not (list? p1)) #f]
        [(member 'CURSOR p1)
         (let* [(pos1 (index-of p1 'CURSOR))]
           (if (= pos1 (sub1 (length p1)))
               #f
               (not (list? (list-ref p1 (add1 pos1))))))]
        [else (ormap cursor-before-token? p1)]))


(define (child-i-with-member thing li)
  (index-where li (lambda (x)
                    (and (list? x)
                         (member thing x)))))

(define (child-i-with-embedded-cursor li)
  (index-where li token-contains-cursor?))

(define (move-cursor-out p1)
  (cond [(not (list? p1)) p1]
        [(child-i-with-embedded-cursor p1)
         (insert-at
          (list-update p1
                      (child-i-with-embedded-cursor p1)
                      extract-cursor)
          (child-i-with-embedded-cursor p1)
          'CURSOR)]
        [(child-i-with-member 'CURSOR p1)
         (move-out (child-i-with-member 'CURSOR p1) p1)]
        [else (map move-cursor-out p1)]))

(define (delete-after-cursor p1)
  (cond [(not (list? p1)) p1]
        [(member 'CURSOR p1)
         (let* [(pos1 (index-of p1 'CURSOR))]
           (delete-before p1 pos1))]
        [else (map delete-after-cursor p1)]))



