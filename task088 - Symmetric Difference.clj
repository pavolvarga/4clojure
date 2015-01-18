
;; Symmetric Difference

;; Write a function which returns the symmetric difference of two sets. 
;; The symmetric difference is the set of items belonging to one but not both of the two sets.

(defn sd [s1 s2]
  (let [sd1 (clojure.set/difference s1 s2)
        sd2 (clojure.set/difference s2 s1)]
    (clojure.set/union sd1 sd2)))

;; Tests

(println (= (sd #{1 2 3 4 5 6} #{1 3 5 7}) #{2 4 6 7}))

(println (= (sd #{:a :b :c} #{}) #{:a :b :c}))

(println (= (sd #{} #{4 5 6}) #{4 5 6}))

(println (= (sd #{[1 2] [2 3]} #{[2 3] [3 4]}) #{[1 2] [3 4]}))