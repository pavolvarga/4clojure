
;; Interleave Two Seqs

;; Write a function which takes two sequences and returns the first item from each, then the second item from each, then the third, etc.

(defn fc [seq1 seq2]
  (loop [x seq1, y seq2, result []]
    (if (or (empty? x) (empty? y))
      result
      (recur (rest x) (rest y) (conj result (first x) (first y)))
)))

;; Tests

(println (= (fc [1 2 3] [:a :b :c]) '(1 :a 2 :b 3 :c)))

(println (= (fc [1 2] [3 4 5 6]) '(1 3 2 4)))

(println (= (fc [1 2 3 4] [5]) [1 5]))

(println (= (fc [30 20] [25 15]) [30 25 20 15]))