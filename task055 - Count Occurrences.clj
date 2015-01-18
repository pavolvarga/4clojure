
;; Count occurences

;; Write a function which returns a map containing the number of occurences of each distinct item in a sequence.

(defn occurences [s]
  (let [update-map (fn [m, k] (if (contains? m k) 
                                (update-in m [k] inc) 
                                (assoc-in m [k] 1)))]
    (loop [result {}, s2 s]
      (if (empty? s2)
        result
        (recur (update-map result (first s2)) (rest s2))
        ))))

;; Tests

(println (= (occurences [1 1 2 3 2 1 1]) {1 4, 2 2, 3 1}))

(println (= (occurences [:b :a :b :a :b]) {:a 2, :b 3}))

(println (= (occurences '([1 2] [1 3] [1 3])) {[1 2] 1, [1 3] 2}))