
;; Interpose a Seq

;; Write a function which separates the items of a sequence by an arbitrary value.

(defn inter [s coll] 
  (drop-last (flatten (for [x coll] [x s]))))

;; Tests

(println (= (inter 0 [1 2 3]) [1 0 2 0 3]))

(println (= (apply str (inter ", " ["one" "two" "three"])) "one, two, three"))

(println (= (inter :z [:a :b :c :d]) [:a :z :b :z :c :z :d]))