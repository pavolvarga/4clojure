
;; Simple closures

;; Lexical scope and first-class functions are two of the most basic building blocks of a functional language like Clojure. 
;; When you combine the two together, you get something very powerful called lexical closures. 
;; With these, you can exercise a great deal of control over the lifetime of your local bindings, 
;; saving their values for use later, long after the code you're running now has finished.
;;
;; It can be hard to follow in the abstract, so let's build a simple closure. 
;; Given a positive integer n, return a function (f x) which computes xn. 
;; Observe that the effect of this is to preserve the value of n for use outside the scope in which it is defined.

(defn sc [n] 
  (let [n2 n]
    (fn [x] (int (Math/pow x n2)))))

;; Tests

(println (= 256 ((sc 2) 16),
       ((sc 8) 2)))

(println (= [1 8 27 64] (map (sc 3) [1 2 3 4])))

(println (= [1 2 4 8 16] (map #((sc %) 2) [0 1 2 3 4])))