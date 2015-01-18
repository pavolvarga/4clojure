
;; Happy numbers

;; Happy numbers are positive integers that follow a particular formula: 
;; take each individual digit, square it, and then sum the squares to get a new number. 
;; Repeat with the new number and eventually, you might get to a number whose squared sum is 1. 
;; This is a happy number. 
;; An unhappy number (or sad number) is one that loops endlessly. 
;; Write a function that determines if a number is happy or not.

(defn is-happy? [x]
  (let [calc (fn [x]
               (int (reduce + (map #(-> % str Integer/parseInt (Math/pow 2)) (seq (str x))))))]
    (loop [numbers #{x}, n x]
      (let [r (calc n)]
        (cond
          (= 1 r) true
          (contains? numbers r) false
          :else 
          (recur (conj numbers r) r))))))

;; Tests

(println (= (is-happy? 7) true))

(println (= (is-happy? 986543210) true))

(println (= (is-happy? 2) false))

(println (= (is-happy? 3) false))