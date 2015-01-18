
;; Intro to Strings

;; Clojure strings are Java strings. This means that you can use any of the Java string methods on Clojure strings.

(def result "HELLO WORLD")

;; Tests

(println (= result (.toUpperCase "hello world")))