#lang scribble/lp2

@require[scribble/manual]

@require[@for-label[racket
                    racket/match
                    syntax/parse
                    racket/pretty]]

@title[#:style manual-doc-style]{LISP 1.5 musicalization}

@chunk[<*>
       (module LISP15 racket
         (provide <provisions/l15>)
         (require <requirements/l15>)
         <l15>)]

@chunk[<l15>
       (define (musicalize-file in pitcher)
         (parameterize ([read-case-sensitive #f])
           (let loop ([forms '()])
             (let* ([operator (read in)]
                    [operands (read in)])
               (if (eof-object? operator)
                   (musicalize-top-level (reverse forms) pitcher)
                   (loop (cons (cons operator operands)
                               forms)))))))]

In order to generate variety in the musical output, the
musicalizer passes around some state.

@chunk[<l15>
       (struct State
         (log
          dots
          octave
          dynamic
          setq-parity
          tonic
          mode
          pitcher))

       (define (default-state pitcher)
         (State 8 0 4 (box 'mf) (box #f) (box 'a) (box 'major) pitcher))

       (define depth (make-parameter 0))]

@chunk[<l15>
       (define (character-code c)
         (string-find "0123456789=\"+abcdefghi.)-jklmnopqr$* /stuvwxyz,("
                      (make-string 1 c)))
       
       (define (atonal-pitcher text state)
         (map (λ (c)
                (add-interval (make-pitch (unbox (State-tonic state))
                                          (State-octave state))
                              (modulo (character-code c)
                                      31)))
              (string->list text)))

       (define major-intervals '(0 5 10 13 18 23 28))
       (define minor-intervals '(0 5 8 13 15 20 26))
       (define harmonic-minor-intervals '(0 2 3 5 7 8 11))

       (define (tonal-pitcher text state)
         (map (λ (c)
                (define code (character-code c))
                (add-interval (make-pitch (unbox (State-tonic state))
                                          (+ (State-octave state)
                                             (cond [(<= 0 code 24)
                                                    -1]
                                                   [(<= 24 code 42)
                                                    0]
                                                   [else
                                                    +1])))
                              (list-ref (case (unbox (State-mode state))
                                          ((major) major-intervals)
                                          ((minor) minor-intervals)
                                          ((harmonic-minor) harmonic-minor-intervals))
                                        (modulo code 7))))
              (string->list text)))]

@chunk[<provisions/l15>
       atonal-pitcher
       tonal-pitcher]

@chunk[<l15>
       (define (musicalize-top-level forms pitcher)
         (let ([state (default-state pitcher)])
           (map (λ (form)
                  (match form
                    [`(define (,things ...))
                     (map (λ (thing)
                            (match thing
                              [`(,name ,e)
                               (Measure 0 0 (musicalize-expression e (make-state state)) (symbol->string name))]))
                          things)]
                    [_
                     (Measure 0 0 (musicalize-expression form state)
                              (~a form))]))
                forms)))]

@chunk[<l15>
       (define (make-state state
                           #:log [log (State-log state)]
                           #:dots [dots (State-dots state)]
                           #:octave [octave (State-octave state)]
                           #:dynamic [dynamic (State-dynamic state)]
                           #:setq-parity [setq-parity (State-setq-parity state)]
                           #:tonic [tonic (State-tonic state)]
                           #:mode [mode (State-mode state)]
                           #:pitcher [pitcher (State-pitcher state)])
         (State log dots octave dynamic setq-parity tonic mode pitcher))
       (define-syntax-rule (with-state e (field ...) body ...)
         (match e
           [(State field ...)
            body ...]))
       
       (define (musicalize-expression e state)
         (with-state state (log dots octave dynamic setq-parity tonic mode pitcher)
           (match e
             [(? symbol?)
              (musicalize-symbol e state)]
             [(? number?)
              (musicalize-constant e state)]
             ['()
              (Note '() log dots '())]
             [`(quote ,thing)
              (musicalize-constant thing state)]
             [`(go ,tag)
              (set-box! tonic (pitch-class-of (add-interval (make-pitch (unbox tonic) 0) 5)))
              (add-event-first! (musicalize-symbol tag state)
                                (increase-dynamic (unbox dynamic)))]
             [`(not ,e)
              (musicalize-expression e (make-state state #:mode (box 'minor)))]
             [`(prog (,vs ...) ,body ...)
              (let* ([count (length vs)]
                     [ratio (if (>= count 3)
                                (/ count (- count 1))
                                3/2)])
                (Tuplet ratio (map (λ (item)
                                     (if (symbol? item)
                                         (add-event-first! (musicalize-symbol item (make-state state #:log 8))
                                                           'accent)
                                         (musicalize-expression item state)))
                                   body)))]
             [`(return ,e)
              (musicalize-expression e (make-state state
                                                   #:log (/ log 2)
                                                   #:tonic (box 'a)))]
             [`(function ,e)
              (add-event-first-last! (musicalize-expression e state)
                                     'sustain-begin
                                     'sustain-end)]
             [`(lambda (,vs ...) ,e)
              (let* ([count (length vs)]
                     [ratio (cond [(= count 1)
                                   1]
                                  [(odd? count)
                                   (/ count (nearest-power-of-two count))]
                                  [else
                                   1])])
                (parameterize ([depth 0])
                  (cond [(= count 1)
                         (musicalize-expression e state)]
                        [(even? count)
                         (musicalize-expression e state)]
                        [else
                         (Tuplet ratio
                                 (musicalize-expression e state))])))]
             [`(setq ,v ,e)
              (define text (symbol->string v))
              (define size (string-length text))
              (set-box! (State-setq-parity state) (not (unbox setq-parity)))
              (let loop ([d (unbox dynamic)]
                         [i size])
                (if (= i 0)
                    (list (musicalize-symbol v state)
                          (add-event-first-last! (musicalize-expression e state)
                                                 (if (unbox setq-parity)
                                                     'crescendo-begin
                                                     'diminuendo-begin)
                                                 d))
                    (loop ((if (unbox setq-parity)
                               increase-dynamic
                               decrease-dynamic)
                           d)
                          (- i 1))))]
             <musicalize-cond>
             <musicalize-application>
             [`(,h . ,t)
              (musicalize-constant e state)])))]

@chunk[<musicalize-application>
       [`(,operator ,operands ...)
        (parameterize ([depth (+ (depth) 1)])
          (let* ([state (make-state state #:log (cond [(zero? (modulo (depth) 5))
                                                       (* log 2)]
                                                      [(and (zero? (modulo (depth) 9))
                                                            (>= log 32))
                                                       (/ log 2)]
                                                      [(zero? (modulo (depth) 7))
                                                       (/ log 2)]
                                                      [else
                                                       log]))]
                 [music (append (map (λ (o)
                                       (musicalize-expression o state))
                                     operands)
                                (list (musicalize-expression operator state)))])
            (cond [(zero? (modulo (depth) 7))
                   (Tuplet (/ (depth) (nearest-power-of-two (depth)))
                           music)]
                  [else
                   music])))]]

@chunk[<musicalize-cond>
       [`(cond ((,test ,result)))
        (list (add-event-first! (musicalize-expression test (make-state state #:dynamic (increase-dynamic (unbox dynamic))))
                                (increase-dynamic dynamic))
              (musicalize-expression result state))]
       [`(cond ((,tests ,results) ...))
        (define count (length tests))
        (define (clause c)
          (list (musicalize-expression (first c) (make-state state
                                                             #:octave (+ octave 1)
                                                             #:tonic (box (pitch-class-of (add-interval (make-pitch (unbox tonic) 0)
                                                                                                        7)))))
                (musicalize-expression (second c) state)))
        (define (go clauses count)
          (define-values (upper lower) (split-at clauses (/ count 2)))
          (Polyphony (map clause upper)
                     (transpose (map clause lower) -62)))
        (if (odd? count)
            (list (clause (first tests)
                          (first results))
                  (go (map list (rest tests) (rest results)) (- count 1)))
            (go (map list tests results) count))]]

@chunk[<l15>
       (define (musicalize-constant c state)
         (with-state state (log dots octave dynamic setq-parity tonic mode pitcher)
           (match c
             [(? exact-integer?)
              (Note (list (make-pitch (integer->pitch-class (modulo c 31)) 4))
                    log
                    dots
                    '())]
             [(? number?)
              (Note (list (make-pitch (integer->pitch-class (modulo (exact-floor c) 31)) 4))
                    log
                    (+ dots 1)
                    '())]
             [(? symbol?)
              (musicalize-symbol c (make-state state #:octave 4))]
             [(? null?)
              (Note '() log dots '())]
             [`(,h . ,t)
              (list (musicalize-constant h state)
                    (musicalize-constant t state))])))]

@chunk[<l15>
       (define (musicalize-symbol s state)
         (with-state state (log dots octave dynamic setq-parity tonic mode pitcher)
           (if (eqv? s 'nil)
               (Note '() log dots '())
               (let* ([text (symbol->string s)]
                      [l (string-length text)]
                      [pitches (pitcher text state)])
                 (if (and (>= l 1)
                          (<= (State-log state) 8)
                          (<= l 5))
                     (list (Note pitches log dots '()))
                     (list (let ([i 0])
                             (map (λ (p)
                                    (set! i (+ i 1))
                                    (Note (list p)
                                          (* log 2)
                                          dots
                                          (cond [(and (> l 1) (= i 1))
                                                 '(beam-begin)]
                                                [(and (> l 1) (= i l))
                                                 '(beam-end)]
                                                [else
                                                 '()])))
                                  pitches))))))))]

@chunk[<requirements/l15>
       (submod "music.rkt" Music)]

@chunk[<l15>
       (define (lily measures out [score? #t])
         (fprintf out "\\version \"2.25.26\"

#(set-default-paper-size \"a1\")

#(define (naturalize-pitch p)
   (let ((o (ly:pitch-octave p))
         (a (* 4 (ly:pitch-alteration p)))
         (n (ly:pitch-notename p)))
     (cond
      ((and (> a 1) (or (eqv? n 6) (eqv? n 2)))
       (set! a (- a 2))
       (set! n (+ n 1)))
      ((and (< a -1) (or (eqv? n 0) (eqv? n 3)))
       (set! a (+ a 2))
       (set! n (- n 1))))
     (cond
      ((> a 2) (set! a (- a 4)) (set! n (+ n 1)))
      ((< a -2) (set! a (+ a 4)) (set! n (- n 1))))
     (if (< n 0) (begin (set! o (- o 1)) (set! n (+ n 7))))
     (if (> n 6) (begin (set! o (+ o 1)) (set! n (- n 7))))
     (ly:make-pitch o n (/ a 4))))

#(define (naturalize music)
   (let ((es (ly:music-property music 'elements))
         (e (ly:music-property music 'element))
         (p (ly:music-property music 'pitch)))
     (if (pair? es)
         (ly:music-set-property!
          music 'elements
          (map naturalize es)))
     (if (ly:music? e)
         (ly:music-set-property!
          music 'element
          (naturalize e)))
     (if (ly:pitch? p)
         (begin
           (set! p (naturalize-pitch p))
           (ly:music-set-property! music 'pitch p)))
     music))

naturalizeMusic =
#(define-music-function (m)
   (ly:music?)
   (naturalize m))

music = \\naturalizeMusic {
~A
}

\\score {
  ~A
  <<
    \\new PianoStaff \\autoChange {
        \\tempo 4 = 60
        \\cadenzaOn
        \\music
    }
  >>
  
  \\midi { }
}
"
                  (let ([out (open-output-string)])
                    (output-music measures out)
                    (get-output-string out))
                  (if score?
                      "  \\layout {
    \\set Score.measureBarType = \"\"
    \\accidentalStyle Staff.dodecaphonic
    \\context {
      \\Voice
      \\remove Forbid_line_break_engraver
      \\override TupletBracket.bracket-visibility = ##t
      \\override TupletNumber.text = #tuplet-number::calc-fraction-text
      \\override Beam.breakable = ##t
   }
    \\context {
      \\Score
      \\override SpacingSpanner.uniform-stretching = ##t
      %proportionalNotationDuration = #1/8
    }
  }"
                      "")))]

@chunk[<requirements/l15>
       (submod "output.rkt" Output)]

@chunk[<provisions/l15>
       lily
       musicalize-file]