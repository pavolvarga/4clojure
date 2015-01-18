
;; Identify keys and values

;; Given an input sequence of keywords and numbers, create a map such that each key in the map is a keyword, 
;; and the value is a sequence of all the numbers (if any) between it and the next keyword in the sequence.

(defn ikv [s]
  (let [create-map (fn [s]
                     (loop [s2 s, result {}]
                       (if (empty? s2)
                         result
                         (recur (rest s2) (conj result [(first s2) []])))))
        proc-pair (fn [pair]
                    (let [ks  (first pair)
                          nss (last pair)
                          m   (create-map ks)
                          lk  (last ks)
                          r (assoc-in m [lk] nss)]
                            (assoc-in m [lk] nss)))
        s2 (partition-by keyword? s)
        s3 (partition-all 2 s2)]
    (loop [s4 s3, result {}]
      (if (empty? s4)
        result
        (recur (rest s4) (merge result (proc-pair (first s4))))
        ))))

;; Tests

(println (= {} (ikv [])))

(println (= {:a [1]} (ikv [:a 1])))

(println (= {:a [1], :b [2]} (ikv [:a 1, :b 2])))

(println (= {:a [1 2 3], :b [], :c [4]} (ikv [:a 1 2 3 :b :c 4])))