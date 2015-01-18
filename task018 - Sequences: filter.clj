
;; Sequences: filter

;; The filter function takes two arguments: a predicate function (f) and a sequence (s). 
;; Filter returns a new sequence consisting of all the items of s for which (f item) returns true.

(def result '(6 7))

;; Tests

(println (= result (filter #(> % 5) '(3 4 5 6 7))))