
;; Flatten a Sequence

;; Write a function which flattens a sequence.

(defn ft [coll]
  (let [f1 (fn [coll] (apply concat (map #(if (sequential? %) % (take 1 (repeat %))) coll)))] 
  (loop [x coll]
    (if (some sequential? x)
      (recur (f1 x))
      x))))      

;; Tests

(println (= (ft '((1 2) 3 [4 [5 6]])) '(1 2 3 4 5 6)))

(println (= (ft ["a" ["b"] "c"]) '("a" "b" "c")))

(println (= (ft '((((:a))))) '(:a)))