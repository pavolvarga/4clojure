
;; Regular Expressions

;; Regex patterns are supported with a special reader macro.

(def result "ABC")

;; Tests

(println (= result (apply str (re-seq #"[A-Z]+" "bA1B3Ce "))))