
;; Map Construction

;; Write a function which takes a vector of keys and a vector of values and constructs a map from them.

(defn mc [ks, vs] 
  (into {} 
        (for [k ks
              :let [ki (.indexOf ks k)] 
              :while (contains? vs ki)] (conj {} {k (nth vs ki)}))
 ))

;; Tests

(println (= (mc [:a :b :c] [1 2 3]) {:a 1, :b 2, :c 3}))

(println (= (mc [1 2 3 4] ["one" "two" "three"]) {1 "one", 2 "two", 3 "three"}))

(println (= (mc [:foo :bar] ["foo" "bar" "baz"]) {:foo "foo", :bar "bar"}))