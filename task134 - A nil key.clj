
;; A nil key

;; Write a function which, given a key and map, returns true iff the map contains an entry with that key and its value is nil.

(defn val-is-nil? [k m] (if (contains? m k) (nil? (k m)) false))

;; Tests

(println (true?  (val-is-nil? :a {:a nil :b 2})))

(println (false? (val-is-nil? :b {:a nil :b 2})))

(println (false? (val-is-nil? :c {:a nil :b 2})))