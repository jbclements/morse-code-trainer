#lang racket

;; Copyright 2013 John Clements (clements@racket-lang.org)
;; Code licensed under the Mozilla Public License 2.0

;; play a bunch of words

(require "morse-code-sounds.rkt"
         levenshtein
         (only-in rsound play)
         racket/runtime-path)

(define-runtime-path common-words-list "./Lemmatized-NGSL-ezi1.txt")

;; read all words matching the given regexp from the ridyhew wordlist
(define (regexp->wordlist rx)
  (apply
   append
   (for/list ([line (in-lines 
                     (open-input-file common-words-list))])
     (filter (lambda (w) 
               (and (not (string=? (string-trim w) ""))
                    (regexp-match rx (string-trim w))))
             (regexp-split #px"\t" line)))))

(define wordlist (regexp->wordlist #px"^[aeitn]*$"))

(define rand-words
  (for/list ([i 10]) (list-ref wordlist (random (length wordlist)))))

(define words rand-words)

(define text (apply string-append (add-between words " ")))

(define the-sound (word-list->sound words 20 20))

(play the-sound)

(define user-input
  (read-line))

(define result
  (apply
   string
   (for/list ([i (in-string user-input)]
              [j (in-string text)])
     (cond [(eq? i j) #\space]
           [else #\X]))))
(list
 user-input
 text
 result
 (string-levenshtein user-input text)
 )