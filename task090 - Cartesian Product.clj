
;; Cartesian Product

;; Write a function which calculates the Cartesian product of two sets.

(defn cp [c1 c2]
  (loop [s2 c2, result []]
    (if (empty? s2)
      (set (apply concat result))
      (recur (rest s2) (conj result (for [x c1] [x (first s2)]))))))

;; Tests

(println (= (cp #{"ace" "king" "queen"} #{"♠" "♥" "♦" "♣"})
   #{["ace"   "♠"] ["ace"   "♥"] ["ace"   "♦"] ["ace"   "♣"]
     ["king"  "♠"] ["king"  "♥"] ["king"  "♦"] ["king"  "♣"]
     ["queen" "♠"] ["queen" "♥"] ["queen" "♦"] ["queen" "♣"]}))
   
(println (= (cp #{1 2 3} #{4 5})
   #{[1 4] [2 4] [3 4] [1 5] [2 5] [3 5]}))

(println (= 300 (count (cp (into #{} (range 10))
                  (into #{} (range 30))))))