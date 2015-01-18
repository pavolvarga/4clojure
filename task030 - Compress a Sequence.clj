
;; Compress a Sequence

;; Write a function which removes consecutive duplicates from a sequence.

(defn comp-seq [s]
  (map first (partition-by identity s)))

;; Tests

(println (= (apply str (comp-seq "Leeeeeerrroyyy")) "Leroy"))

(println (= (comp-seq [1 1 2 3 3 2 2 3]) '(1 2 3 2 3)))

(println (= (comp-seq [[1 2] [1 2] [3 4] [1 2]]) '([1 2] [3 4] [1 2])))