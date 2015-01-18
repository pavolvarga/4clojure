
;; Intro to Sets

;; Sets are collections of unique values.

(def result #{:a :b :c :d})

;; Tests

(println (= result (set '(:a :a :b :c :c :c :c :d :d))))

(println (= result (clojure.set/union #{:a :b :c} #{:b :c :d})))