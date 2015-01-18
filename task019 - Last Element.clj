
;; Last Element

;; Write a function which returns the last element in a sequence.

(defn get-last [s]
  (first (take 1 (reverse s))))

;; Tests

(println (= (get-last [1 2 3 4 5]) 5))

(println (= (get-last '(5 4 3)) 3))

(println (= (get-last ["b" "c" "d"]) "d"))