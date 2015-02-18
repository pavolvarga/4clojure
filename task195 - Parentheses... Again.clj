
;; Parentheses... Again
;;
;; In a family of languages like Lisp, having balanced parentheses is a defining feature of the language. 
;; Luckily, Lisp has almost no syntax, except for these "delimiters" -- and that hardly qualifies as "syntax", 
;; at least in any useful computer programming sense.
;;
;; It is not a difficult exercise to find all the combinations of well-formed parentheses if we only have N pairs to work with. 
;; For instance, if we only have 2 pairs, we only have two possible combinations: "()()" and "(())". 
;; Any other combination of length 4 is ill-formed. Can you see why?
;;
;; Generate all possible combinations of well-formed parentheses of length 2n (n pairs of parentheses). 
;; For this problem, we only consider '(' and ')', but the answer is similar if you work with only {} or only [].
;;
;; There is an interesting pattern in the numbers!

;; http://stackoverflow.com/questions/3172179/valid-permutation-of-parenthesis
(defn generate-combs [n]
  (letfn [(generate [open close temp result]
            (when (and (zero? open) (zero? close))  
              (swap! result conj (clojure.string/join temp)))
            (when (pos? open)
              (generate (dec open) (inc close) (conj temp "(") result))
            (when (pos? close) 
              (generate open       (dec close) (conj temp ")") result))
            @result)]
    (generate n 0 [] (atom #{}))))

;; Tests

(println (= [#{""} #{"()"} #{"()()" "(())"}] (map (fn [n] (generate-combs n)) [0 1 2])))

(println (= #{"((()))" "()()()" "()(())" "(())()" "(()())"} (generate-combs 3)))

(println (= 16796 (count (generate-combs 10))))

(println (= (nth (sort (filter #(.contains ^String % "(()()()())") (generate-combs 9))) 6) "(((()()()())(())))"))

(println (= (nth (sort (generate-combs 12)) 5000) "(((((()()()()()))))(()))"))