
;; Levenshtein Distance
;;
;; Given two sequences x and y, calculate the Levenshtein distance of x and y, i. e. 
;; the minimum number of edits needed to transform x into y. The allowed edits are:
;;
;; - insert a single item
;; - delete a single item
;; - replace a single item with another item
;;
;; WARNING: Some of the test cases may timeout if you write an inefficient solution!

;; implementation of recursive algorithm: https://en.wikipedia.org/wiki/Levenshtein_distance
;; too slow - it took 1022 seconds for all tests to pass
(defn cld1 [a b]
  (letfn [
    (lev-distance [a len-a b len-b]
      (println a " " len-a " " b " " len-b)
      (cond
       (zero? len-a) len-b
       (zero? len-b) len-a
       :else
        (let [cost (if (= (a (dec len-a)) (b (dec len-b))) 0 1)
              try-1 (lev-distance a (dec len-a) b len-b)
              try-2 (lev-distance a len-a       b (dec len-b))
              try-3 (lev-distance a (dec len-a) b (dec len-b))]
          (min 
            (inc try-1)
            (inc try-2)
            (+   try-3 cost)))))]
    (lev-distance (vec a) (count a) (vec b) (count b))))

;; Accepted
;; Wagner–Fischer algorithm: https://en.wikipedia.org/wiki/Levenshtein_distance
(defn cld [a b]
  (let [m (count a) ; number of columns
        n (count b) ; number of rows
        init-mtx  (fn init-mtx [a b]
                    (let [init-mtx-col  (fn [mtx len]
                                          (loop [x 1]
                                            (when (<= x len)
                                              (aset-long mtx 0 x x)
                                              (recur (inc x)))))
                          init-mtx-row  (fn [mtx len]
                                          (loop [x 1]
                                            (when (<= x len)
                                              (aset-long mtx x 0 x)
                                              (recur (inc x)))))
                          d (make-array Long/TYPE (inc n) (inc m))
                          _ (init-mtx-col d m)
                          _ (init-mtx-row d n)]
                      d))
        d (init-mtx a b)
        s (vec a)
        t (vec b)]
    (loop [j 1]
      (when (<= j n)
        (loop [i 1]
          (when (<= i m)
            (if (= (s (dec i)) (t (dec j)))
              (aset-long d j i (aget d (dec j) (dec i))) ; no operation required
              (aset-long d j i 
                (min
                  (inc (aget d j       (dec i))) ; a deletion
                  (inc (aget d (dec j) i      )) ; an insertion
                  (inc (aget d (dec j) (dec i))) ; a substition
                )))
            (recur (inc i))))
        (recur (inc j))))
    (aget d n m)))

;; Tests

(println (= (cld "kitten" "sitting") 3))

(println (= (cld "closure" "clojure") (cld "clojure" "closure") 1))

(println (= (cld "xyx" "xyyyx") 2))

(println (= (cld "" "123456") 6))

(println (= (cld "Clojure" "Clojure") (cld "" "") (cld [] []) 0))

(println (= (cld [1 2 3 4] [0 2 3 4 5]) 2))

(println (= (cld '(:a :b :c :d) '(:a :d)) 2))

(println (= (cld "ttttattttctg" "tcaaccctaccat") 10))

(println (= (cld "gaattctaatctc" "caaacaaaaaattt") 9))