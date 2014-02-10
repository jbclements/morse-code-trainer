#lang racket

;; play a bunch of words

(require "morse-code-sounds.rkt"
         levenshtein
         (only-in rsound play))

;; read all words matching the given regexp from the ridyhew wordlist
(define (regexp->wordlist rx)
  (for/list ([word (in-lines 
                    (open-input-file
                     "/Users/clements/clements/racket-scraps/ridyhew/MASTER"))]
             #:when (regexp-match rx word))
    word))

(define wordlist (regexp->wordlist #px"^[aetn]*$"))

(define rand-words
  (for/list ([i 10]) (list-ref wordlist (random (length wordlist)))))

(define words rand-words)

(define text (apply string-append (add-between words " ")))

(define the-sound (word-list->sound words))

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



;(play )