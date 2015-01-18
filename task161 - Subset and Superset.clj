
;; Subset and Superset

;; Set A is a subset of set B, or equivalently B is a superset of A, if A is "contained" inside B. A and B may coincide.

(def result #{1 2})

;; Tests

(println (clojure.set/superset? result #{2}))

(println (clojure.set/subset? #{1} result))

(println (clojure.set/superset? result #{1 2}))

(println (clojure.set/subset? #{1 2} result))