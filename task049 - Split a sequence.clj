
;; Split a sequence

;; Write a function which will split a sequence into two parts.

(defn cs [x, coll]
  (let [partioned (partition-all x coll)] (conj [] (first partioned) (apply concat (rest partioned)))))

;; Tests

(println (= (cs 3 [1 2 3 4 5 6]) [[1 2 3] [4 5 6]]))

(println (= (cs 1 [:a :b :c :d]) [[:a] [:b :c :d]]))

(println (= (cs 2 [[1 2] [3 4] [5 6]]) [[[1 2] [3 4]] [[5 6]]]))