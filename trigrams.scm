(require 'srfi-69)
(require 'srfi-13)
(require 'srfi-1)
(require 'utils)

(define (tokenize input)
  (map string->symbol
       (string-tokenize input)))

(define (split-into-triplets result input)
  (if (= 2 (length input))
    result
    (split-into-triplets (append result (list (take-right input 3)))
                         (drop-right input 1))))

(define (triplet-split sentances)
    (reverse (split-into-triplets '() (tokenize sentances))))

(define (token-group grp)
  (cons (list (first grp) (second grp))
        (list (last grp))))

(define (trigram-key-structure triplets)
  (map token-group triplets))

(define (alist-value-by-ref-or-default key alist)
  (let ((value (assoc key alist equal?)))
    (if (pair? value)
      (cdr value)
      '())))

(define (convert-word-groups-to-trigram alist word-groups)
  (if (null? word-groups)
    alist
    (let ((key (car (token-group (car word-groups))))
          (tokens (token-group (car word-groups))))
      (convert-word-groups-to-trigram 
        (cons 
          (cons key (flatten (list (alist-value-by-ref-or-default 
                                     (car tokens) 
                                     alist)
                             (cdr tokens))))
          alist)
        (cdr word-groups)))))
       

(define (random-value-from alist str)
  (let* ((last-pair (take-right (tokenize str) 2))
         (value (drop (assoc last-pair alist equal?) 1)))
    (symbol->string (list-ref value (random (length value))))))


;;
;; Main entry point
;;
;; str         - the result string (and a two word bootstrap)
;; trigram-str - the source to generate a trigram list from
;; len         - the length (in words) of the output
;;
(define (story str trigram-str len)
  (let ((trigrams (convert-word-groups-to-trigram 
                    '() 
                     (triplet-split trigram-str))))
    (if (equal? 2 len)
      str
      (story (string-append str " " (random-value-from trigrams str))
             trigram-str
             (- len 1)))))

