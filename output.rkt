#lang scribble/lp2

@require[scribble/manual]

@require[@for-label[racket
                    racket/match
                    syntax/parse
                    racket/pretty]]

@title[#:style manual-doc-style]{Lilypond output}

@chunk[<*>
       (module Output racket
         (provide <provisions/output>)
         (require <requirements/output>)
         <output>)]

@chunk[<output>
       (define (output-music m [out (current-output-port)])
         (define (go m indentation start-of-line? space? meter)
           (define (easy m)
             (go m indentation start-of-line? space? meter))
           (define (indent)
             (for ([_ (in-range indentation)])
               (display #\Space out)))
           <output-pitch>
           <output-value>
           <output-post-events>
           (define (output c)
             (cond [(char=? c #\Newline)
                    (if start-of-line?
                        (void)
                        (begin (set! start-of-line? #t)
                               (display c out)
                               (indent)))]
                   [(char=? c #\Space)
                    (if space?
                        (void)
                        (begin (set! space? #t)
                               (display c out)))]
                   [else
                    (display c out)
                    (set! start-of-line? #f)
                    (set! space? #f)]))
           (define (output-string s)
             (map output (string->list s)))
           (match m
             [(list ms ...)
              (when (> (length ms) 5)
                (output-string "\\allowBreak "))
              (output #\newline)
              (map (λ (m)
                     (easy m))
                   ms)]
             <measure-cases>
             <note-cases>
             <tuplet-cases>
             [(Polyphony voice-1 voice-2)
              (output-string "\n<<\n{\n")
              (go voice-1 (+ indentation 4) #f #f meter)
              (output-string " }\n\\\\ { \n")
              (indent)
              (go voice-2 (+ indentation 4) #f #f meter)
              (output-string "}\n>>")]))
         (go m 2 #f #f #f))]

We simplify pointless tuplets.

@chunk[<tuplet-cases>
       [(Tuplet 1/1 music)
        (easy music)]
       [(Tuplet ratio music)
        (output-string (format "\\tuplet ~A {\n" ratio))
        (indent)
        (go music (+ indentation 2) #f #f meter)
        (displayln "}" out)]]

@chunk[<measure-cases>
       [(Measure 0 0 m label)
        (output-string (format "\\mark \"~A\"\n" label))
        (cond [(not meter)
               (easy m)
               (output-string "\\bar \"||\"")]
              [else
               (set! meter #f)
               (output-string "\\cadenzaOn\n")
               (easy m)])]
       [(Measure n d m label)
        (output-string (format "\\mark \"~A\"\n" label))
        (when (not meter)
          (output-string "\\cadenzaOff\n"))
        (output-string (format "\\time ~A/~A~%" n d))
        (easy m)]]

@chunk[<note-cases>
       [(Note '() log dots post-events)
        (output #\r)
        (output-value log dots)
        (output-post-events post-events)]
       [(Note (list pitches ...) log dots post-events)
        (define pruned (remove-duplicates pitches = #:key Pitch-number))
        (define l (length pruned))
        (cond [(null? (rest pruned))
               (output-pitch (first pruned))]
              [else
               (output #\<)
               (map (λ (p)
                      (output-pitch p)
                      (set! l (- l 1))
                      (unless (<= l 1)
                        (output #\Space)))
                    pruned)
               (output #\>)])
        (output-value log dots)
        (output-post-events post-events)
        (output #\Space)]]

@chunk[<requirements/output>
       (submod "music.rkt" Music)]

@chunk[<provisions/output>
       output-music]

@chunk[<output-pitch>
       (define (output-pitch pitch)
         (output-string (symbol->string (pitch-class-of pitch)))
         (let* ([octave (- (octave-of pitch) 3)]
                [c (if (< octave 0)
                       #\,
                       #\')])
           (for ([_ (in-range octave)])
             (output c))))]

@chunk[<output-value>
       (define (output-value log dots)
         (output-string (~v log))
         (for ([_ (in-range dots)])
           (output #\.)))]

@chunk[<output-post-events>
       (define (output-post-events post-events)
         (map (λ (e)
                (display (case e
                           [(staccato) "-."]
                           [(marcato) "-^"]
                           [(slur-begin) "-("]
                           [(slur-end) "-)"]
                           [(accent) "->"]
                           [(crescendo-begin) "\\<"]
                           [(crescendo-end) "\\!"]
                           [(diminuendo-begin) "\\>"]
                           [(diminuendo-end) "\\!"]
                           [(sustain-begin) "\\sustainOn"]
                           [(sustain-end) "\\sustainOff"]
                           [(beam-begin) "-["]
                           [(beam-end) "-]"]
                           [(pppp
                             ppp
                             pp
                             p
                             mp
                             mf
                             f
                             ff
                             fff
                             ffff
                             fz
                             rfz
                             sf)
                            (format "\\~A" e)]
                           [else
                            (display e)
                            (error "what?")])
                         out))
              post-events)
         (display #\Space out))]
