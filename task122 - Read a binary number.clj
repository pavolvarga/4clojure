
;; Read a binary number

;; Convert a binary number, provided in the form of a string, to its numerical value.

(defn rb [s]
  (let [indexes (range 0 (count s))
       values (for [x indexes] (Math/pow 2 x))
       s2 (map #(Integer/parseInt (str %)) (reverse s))]
   (int (reduce + (map * values s2)))))

;; Tests

(println (= 0     (rb "0")))

(println (= 7     (rb "111")))

(println (= 8     (rb "1000")))

(println (= 9     (rb "1001")))

(println (= 255   (rb "11111111")))

(println (= 1365  (rb "10101010101")))

(println (= 65535 (rb "1111111111111111")))