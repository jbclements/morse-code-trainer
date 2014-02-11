#lang racket

;; Copyright 2013 John Clements (clements@racket-lang.org)
;; Code licensed under the Mozilla Public License 2.0


;; play a set of ten code groups, read keyboard input and report a score.

(require levenshtein
         "morse-code-sounds.rkt")
(require (only-in rsound play))

;; DONE AT 15 WPM (with 20 WPM char speed):
;; ET AT AE AET NT EN AN AENT AI EI IN IT AEINT 
(define charset (list #\n #\o #\t) #;(list #\a #\e #\i #\n #\o #\t))

(define (random-code-group charset)
  (list->string (for/list ([i 5]) (list-ref charset (random (length charset))))))

(define rand-code-groups (for/list ([i 10]) (random-code-group charset)))

;; chose either rand-code-groups or rand-words:
(define words #;rand-words rand-code-groups)

(define text (apply string-append (add-between words " ")))

(define the-sound (word-list->sound words 20 15))

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