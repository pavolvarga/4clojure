
;; Trees into tables

;; Because Clojure's for macro allows you to "walk" over multiple sequences in a nested fashion, 
;; it is excellent for transforming all sorts of sequences. 
;; If you don't want a sequence as your final output (say you want a map), you are often still best-off using for, 
;; because you can produce a sequence and feed it into a map, for example.
;; 
;; For this problem, your goal is to "flatten" a map of hashmaps. 
;; Each key in your output map should be the "path"1 that you would have to take in the original map to get to a value, 
;; so for example {1 {2 3}} should result in {[1 2] 3}. 
;; You only need to flatten one level of maps: if one of the values is a map, just leave it alone.
;;
;; That is, (get-in original [k1 k2]) should be the same as (get result [k1 k2])

(defn flatten-map [s]
  (let [combine-map (fn [k s] (for [[x y] s] {[k x] y}))]
    (loop [result {}, coll s]
      (if (empty? coll)
        result
        (let [[i j] (first coll)]
          (recur (into result (combine-map i j)) (rest coll)))))))

;; Tests

(println (= (flatten-map '{a {p 1, q 2}
                           b {m 3, n 4}})
            '{[a p] 1, [a q] 2
              [b m] 3, [b n] 4}))

(println (= (flatten-map '{[1] {a b c d}
                           [2] {q r s t u v w x}})
            '{[[1] a] b, [[1] c] d,
              [[2] q] r, [[2] s] t,
              [[2] u] v, [[2] w] x}))

(println (= (flatten-map '{m {1 [a b c] 3 nil}})
            '{[m 1] [a b c], [m 3] nil}))