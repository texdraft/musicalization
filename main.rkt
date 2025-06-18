#lang scribble/lp2

@chunk[<*>
       (require (submod "lisp15.rkt" LISP15))
       (define (main)
         (call-with-input-file "lispset"
           (λ (in)
             (call-with-output-file "musicalized-lispset.ly"
               #:exists 'replace
               (λ (out)
                 (lily (musicalize-file in tonal-pitcher)
                       out
                       #t))))))

       (main)]