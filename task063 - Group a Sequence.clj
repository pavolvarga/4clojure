
;; Group a Sequence

;; Given a function f and a sequence s, write a function which returns a map. 
;; The keys should be the values of f applied to each item in s. 
;; The value at each key should be a vector of corresponding items in the order they appear in s.

(defn own-group-by [f v]
  (let 
    [ct-pairs   (fn [f v]  (map vector (map f v) v))
     find-keys  (fn [f v]  (distinct (map f v)))
     get-entry  (fn [k ps] (hash-map k (map last (filter #(= k (first %)) ps))))
     ct-entries (fn [ks pairs] (for [k ks] (get-entry k pairs)))
     pairs (ct-pairs f v)
     ks    (find-keys f v)
     entries (ct-entries ks pairs)]
    (into {} entries)))

;; Tests

(println (= (own-group-by #(> % 5) [1 3 6 8]) {false [1 3], true [6 8]}))

(println (= (own-group-by #(apply / %) [[1 2] [2 4] [4 6] [3 6]])
   {1/2 [[1 2] [2 4] [3 6]], 2/3 [[4 6]]}))

(println (= (own-group-by count [[1] [1 2] [3] [1 2 3] [2 3]])
   {1 [[1] [3]], 2 [[1 2] [2 3]], 3 [[1 2 3]]}))