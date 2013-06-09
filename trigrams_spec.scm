(load "trigrams.scm")
 
(define (test-input)
  "The cat sat on the mat on the cat")

(define (test-input-as-list)
  '(The cat sat on the mat on the cat))

(define (test-table-output)
  '((The cat sat)
    (cat sat on)
    (sat on the)
    (on the mat)
    (the mat on)
    (mat on the)
    (on the cat)))

(define (grouped-trigrams)
  '(((The cat) (sat))
    ((cat sat) (on))
    ((sat on) (the))
    ((on the) (mat))
    ((the mat) (the))
    ((mat on) (cat))
    ((on the) (mat cat))))

(define (unique-token-data)
  '((The cat) (cat sat) (sat on) (on the) (the mat) (mat on)))

(require-extension missbehave missbehave-matchers)

(describe "Splitting a string into tokens"
  (it "returns a list of tokens"
    (expect (tokenize (test-input))
            (be (test-input-as-list)))))

(describe "grouping the tokens"
  (it "groups 3 tokens into key/value pairs"
    (expect (token-group '(The cat sat))
            (be '((The cat) (sat))))))

(describe "splitting the string into token groups"
  (it "has 3 tokens per group"
    (expect (triplet-split (test-input-as-list))
            (be (test-table-output))))

  (it "converts the triplets into an associative list of trigrams"
    (expect (convert-to-trigram (test-table-output))
            (be (grouped-trigrams)))))

