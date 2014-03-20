#lang racket

;; Copyright 2014 John Clements (clements@racket-lang.org)
;; Code licensed under the Mozilla Public License 2.0


;; play a set of ten code groups, read keyboard input and report a score.

(require levenshtein
         "morse-code-sounds.rkt")
(require (only-in rsound play))

(define CHAR-WPM 20)
(define EFFECTIVE-WPM 15)
(define LETTERS-IN-GROUP 5)
(define GROUPS 10)

;; DONE AT 15 WPM (with 20 WPM char speed):
;; ET AT AE AET AENT AEINT AEINOT AEINORT AEIS
(define CHARSET (string->list "aeinorst") )

;; generate a sequence of a given length chosen from the letters in the charset
(define (random-code-group charset)
  (list->string (for/list ([i LETTERS-IN-GROUP]) (list-ref charset (random (length charset))))))

;; generate the desired number of groups
(define rand-code-groups (for/list ([i GROUPS]) (random-code-group CHARSET)))

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
