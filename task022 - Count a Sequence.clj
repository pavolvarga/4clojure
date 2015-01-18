
;; Count a Sequence

;; Write a function which returns the total number of elements in a sequence.

(defn count-a-seq [coll]
  (loop [coll2 coll, result 0]
    (if (empty? coll2)
      result
      (recur (rest coll2) (inc result)))))

;; Tests

(println (= (count-a-seq '(1 2 3 3 1)) 5))

(println (= (count-a-seq "Hello World") 11))

(println (= (count-a-seq [[1 2] [3 4] [5 6]]) 3))

(println (= (count-a-seq '(13)) 1))

(println (= (count-a-seq '(:a :b :c)) 3))