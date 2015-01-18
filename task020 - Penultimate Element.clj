
;; Penultimate Element

;; Write a function which returns the second to last element from a sequence.

(defn get-second-to-last [x] (first (take 1 (take-last 2 x))))

;; Tests

(println (= (get-second-to-last (list 1 2 3 4 5)) 4))

(println (= (get-second-to-last ["a" "b" "c"]) "b"))

(println (= (get-second-to-last [[1 2] [3 4]]) [1 2]))