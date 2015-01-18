
;; Replicate a Sequence

;; Write a function which replicates each element of a sequence a variable number of times.

(defn replicate-seq [s n]
  (apply concat (for [x s] (repeat n x))))

;; Tests

(println (= (replicate-seq [1 2 3] 2) '(1 1 2 2 3 3)))

(println (= (replicate-seq [:a :b] 4) '(:a :a :a :a :b :b :b :b)))

(println (= (replicate-seq [4 5 6] 1) '(4 5 6)))

(println (= (replicate-seq [[1 2] [3 4]] 2) '([1 2] [1 2] [3 4] [3 4])))

(println (= (replicate-seq [44 33] 2) [44 44 33 33]))