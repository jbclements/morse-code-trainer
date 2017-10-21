#lang racket

;; this file uses a heuristic approach to approximately decide
;; which small sets of letters can be used to spell a large number
;; of words. Specifically, it starts with all words of five letters
;; or less and at each step removes the letter that shrinks the list
;; the least. It outputs the list of lists to a file.


;; given a list of words, return
;; (listof (list Char (Listof String)))
;; representing the easiest-to-remove character
;; and the words that are then removed from the list.
;; If you want to practice with five letters, then, you'd
;; take the last five letters and all of the words in
;; their corresponding lists
(define (pick-letters shortwords)
  (let loop ([wordlist shortwords]
             [letters-remaining (list->set
                                 (map integer->char
                                      (range (char->integer #\a)
                                             (add1 (char->integer #\z)))))])
    (cond
      [(or (= (length wordlist) 0)
           (= (set-count letters-remaining) 0))
       '()]
      [else
       (define possible-drops
         (for/list ([ch (in-set letters-remaining)])
           (list ch (filter (λ (str)
                              (not (member ch (string->list str))))
                            wordlist))))
       (define chosen
         (argmax (λ (ch-words) (length (second ch-words))) possible-drops))
       (printf "dropping ~v, wordlist now contains ~a words\n"
               (first chosen) (length (second chosen)))
       (cons (list (first chosen)
                   (remove* (second chosen) wordlist))
             (loop (second chosen)
                   (set-remove letters-remaining (first chosen))))])))

(module+ main
  (define allwords
    (file->lines "/Users/clements/clements/datasets/ridyhew/MASTER"))

  (define shortwords
    (filter (λ (w) (< (string-length w) 5)) allwords))

  (call-with-output-file "/tmp/shortwords.rktd"
    (λ (port) (pretty-write (pick-letters shortwords) port))
    #:exists 'truncate))

