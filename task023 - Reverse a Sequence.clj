
;; Reverse a Sequence

;; Write a function which reverses a sequence.

(defn rev [coll]
  ((fn [coll]
    (loop [coll2 coll, result []]
      (if (empty? coll2)
        result
        (recur (drop-last coll2) (conj result (last coll2))))))
  coll))

;; Tests

(println (= (rev [1 2 3 4 5]) [5 4 3 2 1]))

(println (= (rev (sorted-set 5 7 2 7)) '(7 5 2)))

(println (= (rev [[1 2][3 4][5 6]]) [[5 6][3 4][1 2]]))