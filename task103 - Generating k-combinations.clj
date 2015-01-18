
;; Generating k-combinations

;; Given a sequence S consisting of n elements generate all k-combinations of S, i. e. 
;; generate all possible sets consisting of k distinct elements taken from S. 
;; The number of k-combinations for a sequence is equal to the binomial coefficient.

(defn generate-k-combinations [k s]
  (let [create-power-set (fn [s]
                           (let [create-set (fn [s code]
                                              (let [v (vec s)
                                                    r (range (count code))
                                                    code2 (vec (map #(-> % str Integer/parseInt) (seq code)))]
                                                (into #{} (for [i r :when (= 1 (code2 i))] (v i)))))
                                 c (count s)
                                 fp (if (> c 0) (str "%0" c "d") "%d")
                                 combinations (int (Math/pow 2 c))
                                 decimals (range combinations)
                                 binaries (map #(Integer/toBinaryString %) decimals)
                                 binaries2 (map #(format fp (Integer/parseInt %)) binaries)]
                             (into #{} (for [x binaries2] (create-set s x)))
                             ))]
    (set (filter #(= k (count %)) (create-power-set s)))
    ))

;; Tests

(println (= (generate-k-combinations 1 #{4 5 6}) #{#{4} #{5} #{6}}))

(println (= (generate-k-combinations 10 #{4 5 6}) #{}))

(println (= (generate-k-combinations 2 #{0 1 2}) #{#{0 1} #{0 2} #{1 2}}))

(println (= (generate-k-combinations 3 #{0 1 2 3 4}) #{#{0 1 2} #{0 1 3} #{0 1 4} #{0 2 3} #{0 2 4}
                         #{0 3 4} #{1 2 3} #{1 2 4} #{1 3 4} #{2 3 4}}))

(println (= (generate-k-combinations 4 #{[1 2 3] :a "abc" "efg"}) #{#{[1 2 3] :a "abc" "efg"}}))

(println (= (generate-k-combinations 2 #{[1 2 3] :a "abc" "efg"}) #{#{[1 2 3] :a} #{[1 2 3] "abc"} #{[1 2 3] "efg"}
                                    #{:a "abc"} #{:a "efg"} #{"abc" "efg"}}))