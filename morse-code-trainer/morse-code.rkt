#lang racket

(require rsound
         rackunit
         levenshtein
         "morse-code-table.rkt")

(define (s sec) (round (* sec 44100)))

;; The article SO YOU WANT TO LEARN MORSE CODE, by David G. Finley, suggests
;; characters at 20 WPM, with a bit of space-padding to 15 WPM overall.

(define WPM 20)
;; lengthen the gaps to simulate spaced-WPM
(define spaced-WPM 15)

;; morse code properties:
(define char-gap-units 3)
(define word-gap-units 7)
;; length in dits of the word "paris"
(define paris-length 50)

;; compute the right length for dits and spaces
(define seconds-per-word (/ 60 WPM))
(define seconds-per-spaced-word (/ 60 spaced-WPM))
(define space-required (- seconds-per-spaced-word seconds-per-word))
(define paris-space-length (+ (* 4 char-gap-units) word-gap-units))
(define space-required-per-dit (/ space-required paris-space-length))
(define unit-seconds (/ seconds-per-word paris-length))
(define space-unit-seconds (+ unit-seconds space-required-per-dit))
(define unit-frames (s unit-seconds))
(define space-unit-frames (s space-unit-seconds))
(define (u units) (* unit-frames units))
(define (su units) (* space-unit-frames units))

(define pitch 880)

(define (toney units) (make-tone pitch 0.2 (u units)))

(define dit (toney 1))
(define dah (toney 3))
;; NOTE: NO EXTRA SPACE HERE:
(define intra-gap (silence (u 1)))
;; making longer for now...
(define inter-gap (silence (su char-gap-units)))
;; ditto
(define word-gap (silence (su word-gap-units)))

;; map a string like "-." into a sound
(define (string->sound str)
  (rs-append* (add-between
               (map dot-or-dash->sound (string->list str))
               intra-gap)))

;; map a character to a sound
(define (dot-or-dash->sound ch)
  (match ch
    [#\. dit]
    [#\- dah]))

;; map a character to a dit-dah string
(define (char->dit-dah-string letter)
  (match (hash-ref char-table (char-downcase letter) #f)
    [#f (raise-argument-error 'letter-map "character in map"
                              0 letter)]
    [str str]))


(check-equal? (char->dit-dah-string #\A) ".-")


(check-equal? (string->sound ".-..")
              (rs-append* (list dit intra-gap 
                                dah intra-gap
                                dit intra-gap
                                dit)))

(define (word->sound word)
  (rs-append*
   (add-between (map string->sound (map char->dit-dah-string (string->list word)))
                inter-gap)))

;; check that the extra spacing worked right
(check-=
 (rs-frames (rs-append (word->sound "paris") word-gap))
 (* 60 44100 (/ 1 spaced-WPM))
 ;; we can tolerate 20 frames of error per word.
 20)

(define (word-list->sound word-list)
  (rs-append* (add-between (map word->sound word-list) word-gap)))



(define (random-code-group charset)
  (list->string (for/list ([i 5]) (list-ref charset (random (length charset))))))

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

;; DONE AT 15 WPM (with 20 WPM char speed):
;; ET AT AE AET NT EN AN AENT AI EI IN IT AEINT
(define charset (list ) #;(list #\a #\e #\i #\n #\t))

(define rand-code-groups (for/list ([i 10]) (random-code-group charset)))

;; chose either rand-code-groups or rand-words:
(define words #;rand-words rand-code-groups)

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