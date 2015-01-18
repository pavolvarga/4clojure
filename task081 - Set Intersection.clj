
;; Set Intersection

;; Write a function which returns the intersection of two sets. The intersection is the sub-set of items that each set has in common.

(defn inter [s1 s2]
   (into #{} (map first (for [x s1, y s2 :when (= x y)] [x y]))))

;; Tests

(println (= (inter #{0 1 2 3} #{2 3 4 5}) #{2 3}))

(println (= (inter #{0 1 2} #{3 4 5}) #{}))

(println (= (inter #{:a :b :c :d} #{:c :e :a :f :d}) #{:a :c :d}))