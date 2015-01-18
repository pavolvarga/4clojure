
;; Sequences: rest

;; The rest function will return all the items of a sequence except the first.

(def result '(20 30 40))

;; Tests

(println (= result (rest [10 20 30 40])))