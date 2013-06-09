(require 'srfi-69)
(require 'srfi-13)
(require 'srfi-1)

(define (tokenize input)
  (map string->symbol
       (string-tokenize input)))

(define (split-into-triplets result input)
  (if (= 2 (length input))
    result
    (split-into-triplets (append result (list (take-right input 3)))
                         (drop-right input 1))))

(define (triplet-split tokens)
    (reverse (split-into-triplets '() tokens)))

(define (token-group grp)
  (list (list (first grp) (second grp))
        (list (last grp))))

(define (trigram-key-structure triplets)
  (map token-group triplets))

(define (convert-to-trigram input)
  (trigram-key-structure input))

