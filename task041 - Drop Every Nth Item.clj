
;; Drop Every Nth Item

;; Write a function which drops every Nth item from a sequence.

(defn drop-every [coll i] 
  (keep-indexed #(if (= 0 (rem (inc %1) i)) nil %2) coll))

;; Tests

(println (= (drop-every [1 2 3 4 5 6 7 8] 3) [1 2 4 5 7 8]))

(println (= (drop-every [:a :b :c :d :e :f] 2) [:a :c :e]))

(println (= (drop-every [1 2 3 4 5 6] 4) [1 2 3 5 6]))