
;; Split by Type

;; Write a function which takes a sequence consisting of items with different types 
;; and splits them up into a set of homogeneous sub-sequences. 
;; The internal order of each sub-sequence should be maintained, 
;; but the sub-sequences themselves can be returned in any order (this is why 'set' is used in the test cases).

(defn split-by-type [s]
  (let [put-in-map (fn [m v]
                     (let [k (-> v type str (clojure.string/split #"\s+") last)]
                       (if (contains? m k)
                         (update-in m [k] conj v)
                         (assoc-in m [k] [v]))))
        process-items (fn [data]
                       (loop [s2 data, result {}]
                         (if (empty? s2)
                           result
                           (recur (rest s2) (put-in-map result (first s2))))))
        ]
    (vals (process-items s))))

;; Tests

(println (= (set (split-by-type [1 :a 2 :b 3 :c])) #{[1 2 3] [:a :b :c]}))

(println (= (set (split-by-type [:a "foo"  "bar" :b])) #{[:a :b] ["foo" "bar"]}))

(println (= (set (split-by-type [[1 2] :a [3 4] 5 6 :b])) #{[[1 2] [3 4]] [:a :b] [5 6]}))