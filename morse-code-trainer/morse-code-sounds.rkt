#lang racket

;; Copyright 2013 John Clements (clements@racket-lang.org)
;; Code licensed under the Mozilla Public License 2.0

;; this file contains functions to convert text into sounds

(require rsound
         "morse-code-table.rkt")

(provide (all-defined-out))

(define (s sec) (round (* sec 44100)))

;; map a character to a dit-dah string
(define (char->dit-dah-string letter)
  (match (hash-ref char-table (char-downcase letter) #f)
    [#f (raise-argument-error 'letter-map "character in map"
                              0 letter)]
    [str str]))


(module+ test
  (require rackunit)
  (check-equal? (char->dit-dah-string #\A) ".-"))

;; The article SO YOU WANT TO LEARN MORSE CODE, by David G. Finley, suggests
;; characters at 20 WPM, with a bit of space-padding to 15 WPM overall.
;; a sound-bundle is (make-sound-bundle sound sound sound sound sound)
(struct sound-bundle (dit dah intra-gap inter-gap word-gap))

(define (make-sound-bundle WPM WPM-effective)
  
  ;; morse code properties:
  (define char-gap-units 3)
  (define word-gap-units 7)
  ;; length in dits of the word "paris"
  (define paris-length 50)
  
  ;; compute the right length for dits and spaces
  (define seconds-per-word (/ 60 WPM))
  (define seconds-per-spaced-word (/ 60 WPM-effective))
  (define space-required (- seconds-per-spaced-word seconds-per-word))
  (define paris-space-length (+ (* 4 char-gap-units) word-gap-units))
  (define space-required-per-dit (/ space-required paris-space-length))
  (define unit-seconds (/ seconds-per-word paris-length))
  (define space-unit-seconds (+ unit-seconds space-required-per-dit))
  (define unit-frames (s unit-seconds))
  (define space-unit-frames (s space-unit-seconds))
  (define (u units) (* unit-frames units))
  (define (su units) (* space-unit-frames units))
  
  (define pitch 770)
  
  (define (toney units) (make-tone pitch 0.15 (u units)))
  
  (define dit (toney 1))
  (define dah (toney 3))
  ;; NOTE: NO EXTRA SPACE HERE:
  (define intra-gap (silence (u 1)))
  ;; making longer for now...
  (define inter-gap (silence (su char-gap-units)))
  ;; ditto
  (define word-gap (silence (su word-gap-units)))
  (sound-bundle dit dah intra-gap inter-gap word-gap))

(define default-WPM-effective 15)
(define default-sound-bundle (make-sound-bundle 20 
                                                default-WPM-effective))

;; map a string like "-." into a sound
(define ((string->sound sound-bundle) str)
  (rs-append* (add-between
               (map (dot-or-dash->sound sound-bundle) (string->list str))
               (sound-bundle-intra-gap sound-bundle))))

;; map a character to a sound
(define ((dot-or-dash->sound sound-bundle) ch)
  (match ch
    [#\. (sound-bundle-dit sound-bundle)]
    [#\- (sound-bundle-dah sound-bundle)]))


(module+ test
  (check-equal? ((string->sound default-sound-bundle) ".-..")
                (rs-append* (list (sound-bundle-dit default-sound-bundle)
                                  (sound-bundle-intra-gap default-sound-bundle) 
                                  (sound-bundle-dah default-sound-bundle)
                                  (sound-bundle-intra-gap default-sound-bundle)
                                  (sound-bundle-dit default-sound-bundle) 
                                  (sound-bundle-intra-gap default-sound-bundle)
                                  (sound-bundle-dit  default-sound-bundle)))))

(define ((word->sound sound-bundle) word)
  (rs-append*
   (add-between (map (string->sound sound-bundle)
                     (map char->dit-dah-string (string->list word)))
                (sound-bundle-inter-gap sound-bundle))))

;; check that the extra spacing worked right
(module+ test
  (check-=
   (rs-frames (rs-append ((word->sound default-sound-bundle) "paris")
                         (sound-bundle-word-gap default-sound-bundle)))
   (* 60 44100 (/ 1 default-WPM-effective))
   ;; we can tolerate 20 frames of error per word.
   20))

(define (word-list->sound word-list WPM WPM-effective)
  (define sound-bundle (make-sound-bundle WPM WPM-effective))
  (rs-append* (add-between (map (word->sound sound-bundle) word-list)
                           (sound-bundle-word-gap sound-bundle))))


