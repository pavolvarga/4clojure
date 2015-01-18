
;; Palindrome Detector

;; Write a function which returns true if the given sequence is a palindrome.

(defn palindrome? [inp]
  (= inp (if (string? inp)
           (clojure.string/join "" (reverse inp))
           (reverse inp))))

;; Tests

(println (false? (palindrome? '(1 2 3 4 5))))

(println (true? (palindrome? "racecar")))

(println (true? (palindrome? [:foo :bar :foo])))

(println (true? (palindrome? '(1 1 3 3 1 1))))

(println (false? (palindrome? '(:a :b :c))))