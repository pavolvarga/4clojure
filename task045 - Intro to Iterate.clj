
;; Intro to Iterate

;; The iterate function can be used to produce an infinite lazy sequence.

(def result '(1 4 7 10 13))

;; Tests

(println (= result (take 5 (iterate #(+ 3 %) 1))))