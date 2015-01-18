
;; Map Defaults

;; When retrieving values from a map, you can specify default values in case the key is not found:
;; (println (= 2 (:foo {:bar 0, :baz 1} 2))
;; However, what if you want the map itself to contain the default values? Write a function which takes a default value and a sequence of keys and constructs a map.

(defn put-defs-into-map [v kks]
  (loop [coll kks, result {}]
    (if (empty? coll)
      result
      (let [k (first coll)]
        (recur (rest coll) (assoc result k v))))))

;; Tests

(println (= (put-defs-into-map 0 [:a :b :c]) {:a 0 :b 0 :c 0}))

(println (= (put-defs-into-map "x" [1 2 3]) {1 "x" 2 "x" 3 "x"}))

(println (= (put-defs-into-map [:a :b] [:foo :bar]) {:foo [:a :b] :bar [:a :b]}))