#lang scribble/lp2

@require[scribble/manual]

@require[@for-label[racket
                    racket/match
                    syntax/parse
                    racket/pretty]]

@title[#:style manual-doc-style]{Data structures for music}

@chunk[<*>
       (module Music racket
         (provide <provisions/music>)
         (require #;<requirements/music>)
         <music>)]

As the input is musicalized, the result is built up as a
tree structure. The simplest kinds of nodes are notes and
rests.

I have based the representation of notes on Lilypond, to
make output easier.

A note is a combination of pitches (single notes are chords
with one pitch), their duration, and any “post-events” (this
is Lilypond terminology), which are usually articulations
and dynamics. Elements of @code{post-events} are symbols,
such as @tt{'staccato} (Lilypond @tt{-.}) or @tt{'mf}
(Lilypond @tt{\mf}). We will discuss pitches later.

The duration of a note is conveyed by a “log” and a number
of dots (usually 0). The log corresponds to typical note
value terminology, so that @code{16} means a sixteenth note,
@code{2} a half note. Whole notes have a log of @code{1},
and longer durations can be had by setting @code{log} to
@code{1/2}, etc. The dots function as augmentation dots do
in conventional notation: a @code{dots} of 1 adds half of
the duration according to the @code{log}; a @code{dots} of 2
adds half of the duration, plus half of that half, for 1.75
the duration according to the @code{log}; a @code{dots} of 3
effectively multiplies the duration by 1.875, and so on.

Rests are notes that don't have pitches.

@chunk[<music>
       (struct Note
         (pitches
          log
          dots
          post-events)
         #:mutable)]

@chunk[<provisions/music>
       (struct-out Note)]

In a music tree, a sequence of notes is a list of
@code{Note} or @code{Rest} objects. Nested lists will be
flattened in the output.

Durations other than those based on powers of two have to be
expressed using tuplets.

@chunk[<music>
       (struct Tuplet
         (ratio
          music))]

Music should be divided into measures. There need not be a
meter; use 0 for the numerator and denominator in that case.

@chunk[<music>
       (struct Measure
         (numerator
          denominator
          music))]

We support two voices.

@chunk[<music>
       (struct Polyphony
         (voice-1
          voice-2))]

@chunk[<provisions/music>
       (struct-out Tuplet)
       (struct-out Polyphony)
       (struct-out Measure)]

We will often need to traverse music.

@chunk[<music>
       (define (map-notes f music)
         (match music
           [(list ms ...)
            (flatten (map (λ (m)
                            (map-notes f m))
                          ms))]
           [(Note _ _ _ _)
            (f music)]
           [(Tuplet ratio inner)
            (Tuplet ratio (map-notes f inner))]
           [(Polyphony voice-1 voice-2)
            (Polyphony (map-notes f voice-1)
                       (map-notes f voice-2))]))

       (define (extract-notes music)
         (match music
           [(list ms ...)
            (flatten (map extract-notes ms))]
           [(Note _ _ _ _)
            (list music)]
           [(Tuplet _ m)
            (extract-notes m)]
           [(Polyphony voice-1 voice-2)
            (append (extract-notes voice-1)
                    (extract-notes voice-2))]))]

@chunk[<provisions/music>
       map-notes]

@chunk[<music>
       (define (add-post-event! note event)
         (set-Note-post-events! note (cons event (Note-post-events note))))

       (define (add-pitch! note pitch)
         (set-Note-pitches! note (cons pitch (Note-pitches note))))]

@chunk[<provisions/music>
       add-post-event!
       add-pitch!]

@section{Pitches}

Pitches are combinations of integers and an alteration. The
integer represents the basic pitch, with 0 meaning C0 (@tt{c,,,}
in Lilypond). For fun, we use a base-31 system, so that some
enharmonic pitches are available. To add an octave, add 31.

The alteration is supposed to be used for microtonal stuff.

@chunk[<music>
       (struct Pitch
         (number
          alteration))
       (define pitch-classes '(c deses cis des cisis
                               d eeses dis es disis
                               e fes eis
                               f eisis fis ges fisis
                               g aeses gis aes gisis
                               a beses ais bes aisis
                               b ces bis))

       (define pitch-vector (apply vector pitch-classes))
       (define pitch-hash (make-hash (let loop ([ps pitch-classes]
                                                [i 0])
                                       (if (null? ps)
                                           '()
                                           (cons (cons (first ps) i)
                                                 (loop (rest ps) (+ i 1)))))))

       (define (pitch-class->integer pitch-class)
         (hash-ref pitch-hash pitch-class))

       (define (integer->pitch-class integer)
         (vector-ref pitch-vector integer))

       (define (pitch-class-of pitch)
         (vector-ref pitch-vector (modulo (Pitch-number pitch) 31)))

       (define (octave-of pitch)
         (quotient (Pitch-number pitch) 31))

       (define (make-pitch pitch-class octave [alteration 0])
         (Pitch (+ (hash-ref pitch-hash pitch-class) (* 31 octave))
                alteration))]

@chunk[<provisions/music>
       (struct-out Pitch)
       pitch-class->integer
       integer->pitch-class
       pitch-class-of
       octave-of
       make-pitch]

The major advantage of integers over a symbolic
representation is that operations on pitches are easier.
Enharmonics make this slightly more annoying than if we used
a plain twelve-tone system, however.

@chunk[<music>
       (define (add-interval pitch semitones)
         (let ([number (Pitch-number pitch)])
           (define (subtract-interval pitch semitones)
             (cond [(= semitones 1)
                    (let ([pitch-class (pitch-class-of pitch)])
                      (Pitch (- (Pitch-number pitch) (if (memq pitch-class '(f c))
                                                         3
                                                         2))
                             (Pitch-alteration pitch)))]
                   [else
                    (subtract-interval (subtract-interval pitch 1) (- semitones 1))]))
           (cond [(= semitones 0)
                  pitch]
                 [(< semitones 0)
                  (subtract-interval pitch (abs semitones))]
                 [(= semitones 1)
                  (let ([pitch-class (pitch-class-of pitch)])
                    (Pitch (+ number (if (memq pitch-class '(e b))
                                         3
                                         2))
                           (Pitch-alteration pitch)))]
                 [else
                  (add-interval (add-interval pitch 1) (- semitones 1))])))]

@chunk[<music>
       (define (pitch< p1 p2)
         (match* (p1 p2)
           [((Pitch n1 _) (Pitch n2 _))
            (< n1 n2)]))

       (define (pitch= p1 p2)
         (match* (p1 p2)
           [((Pitch n1 a1) (Pitch n2 a2))
            (and (= n1 n2)
                 (= a1 a2))]))]

We do our own transposition, so that we can clamp the
results into range before outputting Lilypond.

@chunk[<music>
       (define (transpose music amount)
         (map-notes (λ (note)
                      (match note
                        [(Note pitches log dots post-events)
                         (Note (map (λ (p)
                                      (add-interval p amount))
                                    pitches)
                               log
                               dots
                               post-events)]))
                    music))]

And speaking of clamping:

@chunk[<music>
       (define (clamp music lower upper)
         (map-notes (λ (note)
                      (match note
                        [(Note pitches log dots post-events)
                         (Note (map (λ (p)
                                      (cond [(pitch< p lower)]))
                                    pitches)
                               log
                               dots
                               post-events)]))))]

This function adds a post-event to the first and last note
in some music, unless the music consists of only one note.

@chunk[<music>
       (define (add-event-first-last! m e1 e2)
         (let ([l (extract-notes m)])
           (add-post-event! (first l) e1)
           (add-post-event! (last l) e2))
         m)

       (define (add-event-first! m e)
         (let ([l (extract-notes m)])
           (add-post-event! (first l) e))
         m)]

@chunk[<provisions/music>
       add-interval
       clamp
       transpose
       add-event-first-last!
       add-event-first!]

Here are some utility functions for generating music.

@chunk[<music>
       (define (increase-dynamic dynamic)
         (if (eqv? dynamic 'ffff)
             'ffff
             (cadr (member dynamic '(pppp ppp pp p mp mf f ff fff ffff)))))

       (define (decrease-dynamic dynamic)
         (if (eqv? dynamic 'pppp)
             'pppp
             (cadr (member dynamic '(ffff fff ff f mf mp p pp ppp pppp)))))

       (define (nearest-power-of-two n)
         (let loop ([i 0])
           (if (>= (expt 2 i) n)
               (expt 2 (- i 1))
               (loop (+ i 1)))))]

@chunk[<provisions/music>
       increase-dynamic
       decrease-dynamic
       nearest-power-of-two]