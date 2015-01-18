
;; Word Sorting

;; Write a function that splits a sentence up into a sorted list of words. 
;; Capitalization should not affect sort order and punctuation should be ignored.

(defn sort-words [s]
  (sort 
    #(compare (clojure.string/lower-case %1) (clojure.string/lower-case %2)) 
    (map #(clojure.string/replace % #"[\.!]" "") (clojure.string/split s #"\s"))
    ))

;; Tests

(println (= (sort-words  "Have a nice day.")
   ["a" "day" "Have" "nice"]))

(println (= (sort-words  "Clojure is a fun language!")
   ["a" "Clojure" "fun" "is" "language"]))

(println (= (sort-words  "Fools fall for foolish follies.")
   ["fall" "follies" "foolish" "Fools" "for"]))