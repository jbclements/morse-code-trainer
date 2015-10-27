#lang racket

;; Copyright 2014 John Clements (clements@racket-lang.org)
;; Code licensed under the Mozilla Public License 2.0


;; play a set of ten code groups, read keyboard input and report a score.

(require levenshtein
         math/distributions
         racket/runtime-path
         "morse-code-sounds.rkt")

(require (only-in rsound play))

(define-runtime-path HERE ".")

(define CHAR-WPM 22)
(define EFFECTIVE-WPM 15)
(define LETTERS-IN-GROUP 5)
(define GROUPS 10)

;; DONE AT 15 WPM (with 20 WPM char speed):
;; ET AET AENT AEINT AEINOT AEINORT AEINORST ADEINORST ADEHINORST
(define OLDCHARS (string->list "adehinorst"))
(define NEWCHARS (string->list "l"))

(define ALLCHARS (append OLDCHARS NEWCHARS))

(define letter-distribution
  (discrete-dist ALLCHARS
                 (vector-append
                  (make-vector (length OLDCHARS) 0)
                  (make-vector (length NEWCHARS) 80))))

(define USE-MARKOV-LIKELIHOOD 0.5)

(define markov-chain (file->value (build-path HERE "dickens-markov-chain.rktd")))

(define (transpose lol)
  (apply map list lol))

;; reduce a distribution to only a subset of elements.
;; return "no-chars-in-distribution" if empty
(define (reduce-distribution char dist)
  (define remaining-elements
    (for/list ([v (second (hash-ref markov-chain char))]
               [p (third (hash-ref markov-chain char))]
               #:when (member v ALLCHARS))
      (list v p)))
  (cond [(empty? remaining-elements) "no-chars-in-distribution"]
        [else (apply discrete-dist (transpose remaining-elements))]))

;; a hash mapping characters to the distributions of
;; their following letters RESTRICTED to elements of ALLCHARS.
(define follow-hash
  (for/hash ([(char dist) (in-hash markov-chain)])
    (values char (reduce-distribution char dist))))

;; generate a random code group; use the markov chain 
;; distribution with likelihood USE-MARKOV-LIKELIHOOD
(define (random-code-group-3)
  (list->string
   (let loop ([i LETTERS-IN-GROUP]
              [prev-char #\space])
     (cond [(= i 0) empty]
           [else (define distribution 
                   (cond [(< (random) USE-MARKOV-LIKELIHOOD)
                          (hash-ref follow-hash prev-char)]
                         [else letter-distribution]))
                 (define next-char (sample distribution))
                 (cons next-char (loop (sub1 i) next-char))]))))

;; generate a random code group using the letter-distribution
(define (random-code-group-2)
  (list->string
   (let loop ([i LETTERS-IN-GROUP]
             [ch (sample letter-distribution)])
    (cond [(= i 0) empty]
          [else (cons ch (loop (sub1 i) (sample (hash-ref follow-hash ch))))]))))


;; generate a sequence of a given length chosen from the letters in the charset
(define (random-code-group)
  (list->string (for/list ([i LETTERS-IN-GROUP]) 
                  (sample letter-distribution))))

;; generate the desired number of groups
(define rand-code-groups (for/list ([i GROUPS]) (random-code-group-3)))

;; play the sound
(play (word-list->sound rand-code-groups CHAR-WPM EFFECTIVE-WPM))

;; generate the text that the student should type in:
(define correct-text (apply string-append (add-between rand-code-groups " ")))

(define user-input (read-line))

;; a string showing which characters were wrong:
(define error-chars
  (apply
   string
   (for/list ([i (in-string user-input)]
              [j (in-string correct-text)])
     (cond [(eq? i j) #\space]
           [else #\X]))))

(printf "
typed:   ~s
correct: ~s
errors:  ~s
levenshtein (edit) distance: ~s "
        user-input
        correct-text
        error-chars
        (string-levenshtein user-input correct-text))
