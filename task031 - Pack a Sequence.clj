
;; Pack a Sequence

;; Write a function which packs consecutive duplicates into sub-lists.

(defn pack-seq [s]
  (partition-by identity s))

;; Tests

(println (= (pack-seq [1 1 2 1 1 1 3 3]) '((1 1) (2) (1 1 1) (3 3))))

(println (= (pack-seq [:a :a :b :b :c]) '((:a :a) (:b :b) (:c))))

(println (= (pack-seq [[1 2] [1 2] [3 4]]) '(([1 2] [1 2]) ([3 4]))))