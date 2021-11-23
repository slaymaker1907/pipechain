#lang info
(define collection "pipechain")
(define deps '("base"))
(define build-deps '("scribble-lib" "racket-doc" "rackunit-lib"))
(define scribblings '(("scribblings/pipechain.scrbl" ())))
(define pkg-desc "Provides a quick and easy way to reduce nested function calls without creating
many superfluous temporary names. Naming things is hard, but so is reading
nested function calls")
(define version "1.0")
(define pkg-authors '(Dyllon Gagnier))
