
;; Sum Some Set Subsets

;; Given a variable number of sets of integers, create a function which returns true 
;; iff all of the sets have a non-empty subset with an equivalent summation.

(defn subset-sum [& sets]
  (let [create-binaries (memoize 
                          (fn [c] 
                            (let [fp (if (> c 0) (str "%0" c "d") "%d")
                                  combinations (int (Math/pow 2 c))
                                  decimals (range 1 combinations)] ;; skip zero
                              (map #(format fp (-> % Integer/toBinaryString Integer/parseInt)) decimals))))
        create-set (fn [s code]
                     (let [v (vec s)
                           r (range (count code))
                           code2 (vec (map #(-> % str Integer/parseInt) (seq code)))]
                       (into #{} (for [i r :when (= 1 (code2 i))] (v i))))) 
        create-power-set (fn [s] 
                           (let [binaries (create-binaries (count s))]
                             (
                              (fn cps [bins]
                                (lazy-seq
                                  (when-not (empty? bins)
                                    (cons 
                                      (create-set s (first bins))
                                      (cps (rest bins))))
                                  ))
                              binaries)
                             ))
        create-subsets-sum (fn [s]
                             (loop [s1 s, result []]
                               (if (empty? s1)
                                 result
                                 (recur 
                                   (rest s1) 
                                   (let [power-set (create-power-set (first s1))]
                                     (conj result (distinct (map #(reduce + %) power-set)))
                                     )))))
        subsets-sums (create-subsets-sum sets)
        compare-seqs (fn [s]
                       (let [in? (fn [s e] (some #(= % e) s))
                             fs (first s)
                             rs (rest s)]
                         (loop [fs1 fs]
                           (if (empty? fs1)
                             false
                             (let [result (for [x rs] (in? x (first fs1)))
                                   positive (filter true? result)]
                               (if (= (count rs) (count positive))
                                 true
                                 (recur (rest fs1))))))))
        ]
    (compare-seqs subsets-sums)
    ))

;; Tests

(println (= true  (subset-sum #{-1 1 99} #{-2 2 888} #{-3 3 7777})))

(println (= false (subset-sum #{1} #{2} #{3} #{4})))

(println (= true  (subset-sum #{1})))

(println (= false (subset-sum #{1 -3 51 9} #{0} #{9 2 81 33})))

(println (= true  (subset-sum #{1 3 5} #{9 11 4} #{-3 12 3} #{-3 4 -2 10})))

(println (= false (subset-sum #{-1 -2 -3 -4 -5 -6} #{1 2 3 4 5 6 7 8 9})))

(println (= true  (subset-sum #{1 3 5 7} #{2 4 6 8})))

(println (= true  (subset-sum #{-1 3 -5 7 -9 11 -13 15} #{1 -3 5 -7 9 -11 13 -15} #{1 -1 2 -2 4 -4 8 -8})))

(println (= true  (subset-sum #{-10 9 -8 7 -6 5 -4 3 -2 1} #{10 -9 8 -7 6 -5 4 -3 2 -1})))