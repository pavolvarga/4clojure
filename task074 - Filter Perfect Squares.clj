
;; Filter Perfect Squares

;; Given a string of comma separated integers, write a function which returns 
;; a new comma separated string that only contains the numbers which are perfect squares.

(defn filter-perf-squares [s]
  (let [
        pow (fn [y] (int (Math/pow y 2)))
        ints (map #(Integer/parseInt %) (clojure.string/split s #","))
        is-perf-square? (fn [n] (loop [x 2]
                                  (cond
                                    (> x n) false
                                    (> (pow x) n) false        
                                    (= (pow x) n) true
                                    :else (recur (inc x)))))
        perf-squares (filter is-perf-square? ints)
        ]
    (clojure.string/join "," perf-squares)
    ))

;; Tests

(println (= (filter-perf-squares "4,5,6,7,8,9") "4,9"))

(println (= (filter-perf-squares "15,16,25,36,37") "16,25,36"))