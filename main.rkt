#lang scribble/lp2

@chunk[<*>
       (require (submod "lisp15.rkt" LISP15))
       (define (do-file in out)
         (call-with-input-file in
           (λ (in)
             (call-with-output-file out
               #:exists 'replace
               (λ (out)
                 (lily (musicalize-file in pentatonic-pitcher)
                       out
                       #t))))))

       (do-file "lispset" "musicalized-lispset.ly")
       (do-file "reduce" "musicalized-reduce.ly")]