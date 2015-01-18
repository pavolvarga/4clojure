
;; Power Set

;; Write a function which generates the power set of a given set. 
;; The power set of a set x is the set of all subsets of x, including the empty set and x itself.

(defn create-power-set [s]
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
    ))

;; Tests

(println (= (create-power-set #{1 :a}) #{#{1 :a} #{:a} #{} #{1}}))

(println (= (create-power-set #{}) #{#{}}))

(println (= (create-power-set #{1 2 3})
   #{#{} #{1} #{2} #{3} #{1 2} #{1 3} #{2 3} #{1 2 3}}))

(println (= (count (create-power-set (into #{} (range 10)))) 1024))