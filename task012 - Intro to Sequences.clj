
;; Intro to Sequences

;; All Clojure collections support sequencing. You can operate on sequences with functions like first, second, and last.

(def result 3)

;; Tests

(println (= result (first '(3 2 1))))

(println (= result (second [2 3 4])))

(println (= result (last (list 1 2 3))))