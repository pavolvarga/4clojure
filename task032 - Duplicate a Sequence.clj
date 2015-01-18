
;; Duplicate a Sequence

;; Write a function which duplicates each element of a sequence.

(defn duplicate [coll] (apply concat (for [x coll] (conj () x x))))

;; Tests

(println (= (duplicate [1 2 3]) '(1 1 2 2 3 3)))

(println (= (duplicate [:a :a :b :b]) '(:a :a :a :a :b :b :b :b)))

(println (= (duplicate [[1 2] [3 4]]) '([1 2] [1 2] [3 4] [3 4])))

(println (= (duplicate [[1 2] [3 4]]) '([1 2] [1 2] [3 4] [3 4])))