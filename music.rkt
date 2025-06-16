#lang scribble/lp2

@require[@for-label[scribble/manual
                    racket/control
                    racket/match
                    syntax/parse
                    racket/pretty
                    (only-in racket/base #%module-begin)]]

@title{Data structures for music}

@chunk[<Syntax>
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
such as @code{'staccato} (Lilypond @code{-.}) or @code{'mf}
(Lilypond @code{\mf}). We will discuss pitches later.

The duration of a note is conveyed by a “log” and a number
of dots (usually 0). The log corresponds to typical note
value terminology, so that @code{16} means a sixteenth note,
@code{2} a half note. Whole notes have a log of @code[1],
and longer durations can be had by setting @code{log} to
@code{1/2}, etc. The dots function as augmentation dots do
in conventional notation: a @code{dots} of 1 adds half of
the duration according to the @code{log}; a @code{dots} of 2
adds half of the duration, plus half of that half, for 1.75
the duration according to the @code{log}; a @code{dots} of 3
effectively multiplies the duration by 1.875, and so on.

Rests are like notes but don't have pitches.

@chunk[<music>
       (struct Note
         (pitches
          log
          dots
          post-events))

       (struct Rest
         (log
          dots
          post-events))]

@chunk[<provisions/music>
       (struct-out Note)
       (struct-out Rest)]

In a music tree, a sequence of notes is a list of
@code{Note} or @code{Rest} objects. Nested lists will be
flattened in the output.

Tuplets are represented by @code{Tuplet}.

@chunk[<music>
       (struct Tuplet
         (ratio
          music))]

@section{Pitches}

Pitches are simply integers, with 0 meaning C0 (@code{c,,,}
in Lilypond). For fun, we use a base-31 system, so that some
enharmonic pitches are available. To add an octave, add 31.

@chunk[<music>
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
         (vector-ref pitch-vector (modulo pitch 31)))

       (define (octave-of pitch)
         (quotient pitch 31))

       (define (make-pitch pitch-class octave)
         (+ (hash-ref pitch-hash pitch-class) (* 31 octave)))]

@chunk[<provisions/music>
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
         (define (subtract-interval pitch semitones)
           (cond [(= semitones 1)
                  (let ([pitch-class (pitch-class-of pitch)])
                    (- pitch (if (memq pitch '(f c))
                                 3
                                 2)))]))
         (cond [(= semitones 0)
                pitch]
               [(< semitones 0)
                (subtract-interval pitch (abs semitones))]
               [(= semitones 1)
                (let ([pitch-class (pitch-class-of pitch)])
                  (+ pitch (if (memq pitch '(e b))
                               3
                               2)))]
               [else
                (add-interval (add-interval pitch 1) (- semitones 1))]))]

@chunk[<provisions/music>
       add-interval]
