
;; Merge with a Function

;; Write a function which takes a function f and a variable number of maps. 
;; Your function should return a map that consists of the rest of the maps conj-ed onto the first. 
;; If a key occurs in more than one map, the mapping(s) from the latter (left-to-right) 
;; should be combined with the mapping in the result by calling (f val-in-result val-in-latter)

(defn merge-with-f [f & ms]
  (let [up-first-with-second (fn [f m1 m2]
                               (loop [result m1, ks (keys m2)]
                                 (if (empty? ks)
                                   result
                                   (let [k (first ks)
                                         r2 (if (contains? m1 k)
                                              (update-in result [k] f (m2 k))
                                              (assoc result k (m2 k)))] 
                                     (recur r2 (rest ks))))))] 
    (loop [result (first ms), s (rest ms)]
      (if (empty? s)
        result
        (recur (up-first-with-second f result (first s)) (rest s))))))

;; Tests

(println (= (merge-with-f * {:a 2, :b 3, :c 4} {:a 2} {:b 2} {:c 5})
   {:a 4, :b 6, :c 20}))

(println (= (merge-with-f - {1 10, 2 20} {1 3, 2 10, 3 15})
   {1 7, 2 10, 3 15}))

(println (= (merge-with-f concat {:a [3], :b [6]} {:a [4 5], :c [8 9]} {:b [7]})
   {:a [3 4 5], :b [6 7], :c [8 9]}))