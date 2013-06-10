(load "trigrams.scm")
 
(define test-input
  "The cat sat on the mat on the cat")

(define test-table-output
  '((The cat sat)
    (cat sat on)
    (sat on the)
    (on the mat)
    (the mat on)
    (mat on the)
    (on the cat)))

(define grouped-trigrams
  '(((The cat) sat)
    ((cat sat) on)
    ((sat on) the)
    ((on the) mat)
    ((the mat) the)
    ((mat on) cat)
    ((on the) mat cat)))

(require-extension missbehave missbehave-matchers)

(describe "splitting the string into token groups"
  (it "has 3 tokens per group"
    (expect (triplet-split test-input)
            (be test-table-output))))

(describe "getting values from an alist"
  (let ((dummy-list '(((a b) c) ((c d) e) ((a d) r))))
    (it "finds a value"
      (expect (alist-value-by-ref-or-default '(a b) dummy-list)
              (be '(c))))
    (it "returns a default"
      (expect (alist-value-by-ref-or-default '(cats) dummy-list)
              (be '())))))

(describe "grouping the test table into a table of trigrams"
  (it "makes an association list"
    (expect (convert-word-groups-to-trigram '() test-table-output)
            (be a list)))
  (it "correctly groups multiple words"
    (expect (assoc 
              '(on the) 
              (convert-word-groups-to-trigram '() test-table-output))
            (be '((on the) mat cat)))))

