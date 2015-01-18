
;; dot product

;; Create a function that computes the dot product of two sequences. You may assume that the vectors will have the same length.

(defn dp [v1 v2]
  (reduce + (map * v1 v2)))

;; Tests

(println (= 0 (dp [0 1 0] [1 0 0])))

(println (= 3 (dp [1 1 1] [1 1 1])))

(println (= 32 (dp [1 2 3] [4 5 6])))

(println (= 256 (dp [2 5 6] [100 10 1])))