#lang scribble/lp2

#lang scribble/lp2

@require[scribble/manual]

@require[@for-label[racket
                    racket/match
                    syntax/parse
                    racket/pretty]]

@title[#:style manual-doc-style]{Lilypond output}

@chunk[<*>
       (module Output racket
         (provide #;<provisions/output>)
         (require #;<requirements/output>)
         <output>)]

@chunk[<output>
       4]