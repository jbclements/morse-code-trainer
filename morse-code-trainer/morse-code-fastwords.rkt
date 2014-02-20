#lang typed/racket

;; Copyright 2014 John Clements (clements@racket-lang.org)
;; Code licensed under the Mozilla Public License 2.0

;; play a bunch of words


(require/typed rsound 
               [#:opaque Sound rsound?]
               [play (Sound -> String)])

(require/typed "morse-code-sounds.rkt"
               [word-list->sound 
                ((Listof String) Positive-Real Positive-Real -> Sound)])

(require/typed levenshtein
               [string-levenshtein (String String -> Integer)])

(require racket/runtime-path)

(define-runtime-path common-words-list "./Lemmatized-NGSL-ezi1.txt")
(define-runtime-path word-frequency-list "./frequency.rktd")

(define HIGH-WPM 30)

(define-predicate freq-list? (Listof (List String Integer)))
(: allwords (Listof (List String Integer)))
(define allwords 
  ((lambda (v) (cond [(freq-list? v) v]
                     [else (error 'allwords "expected a frequency list")]))
   (file->value word-frequency-list)))

;; read all words matching the given regexp
(: regexp->wordlist (Regexp Index -> (Listof String)))
(define (regexp->wordlist rx maxlen)
  (filter (lambda: ([w : String]) (and (not (false? (regexp-match rx w)))
                                       (<= (string-length w) maxlen)))
          (map (ann first ((List String Integer) -> String)) allwords)))

(define MAX-WORD-LEN 2)
(define wordlist (regexp->wordlist #px"^[aeinot]*$" MAX-WORD-LEN))

(printf "~v words available matching the given regexp and length"
        (length wordlist))

;; here are the words we're going to repeat again and again.
(define NUM-WORDS-CHOSEN 3)
(define rand-words
  (for/list: : (Listof String) ([i NUM-WORDS-CHOSEN]) (list-ref wordlist (random (length wordlist)))))

;; how many actual words should we play?
(define NUM-WORDS-PLAYED 30)
(define word-seq (for/list: : (Listof String)
                   ([i : Integer (in-range NUM-WORDS-PLAYED)]) 
                   (list-ref rand-words (random (length rand-words)))))

(define words word-seq)

(define text (apply string-append (add-between words " ")))

(define the-sound (word-list->sound words HIGH-WPM HIGH-WPM))

(play the-sound)

(: user-input String)
(define user-input
  ((lambda (r) (cond [(string? r) r]
                     [else (error 'user-input "expected string")]))
   (read-line)))

(define result
  (apply
   string
   (for/list: : (Listof Char) 
     ([i (in-string user-input)]
      [j (in-string text)])
     (cond [(eq? i j) #\space]
           [else #\X]))))
(list
 user-input
 text
 result
 (string-levenshtein user-input text)
 )