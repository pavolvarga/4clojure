
;; Balancing Brackets

;; When parsing a snippet of code it's often a good idea to do a sanity check to see if all the brackets match up. 
;; Write a function that takes in a string and returns truthy if all square [ ] round ( ) and curly { } brackets are 
;; properly paired and legally nested, or returns falsey otherwise.

(defn balanced-brackets? [s]
  (let [
        filter-brackets (fn [s]
                          (let [pred (fn [c]
                                       (let [c1 (str c)]
                                         (if (or (= c1 "(") (= c1 ")") 
                                                 (= c1 "[") (= c1 "]")
                                                 (= c1 "{") (= c1 "}"))
                                           true
                                           false)))]
                            (vec (filter #(pred %) s))
                            ))
        is-pair? (fn [a b]
                   (let [a1 (str a)
                         b1 (str b)]
                     (cond
                       (and (= a1 "(") (= b1 ")")) true
                       (and (= a1 "[") (= b1 "]")) true
                       (and (= a1 "{") (= b1 "}")) true
                       :else false)))
        remove-pair (fn [v i]
                      (vec (concat (subvec v 0 i) (subvec v (+ 2 i)))))                 
        strip-pairs (fn [v]
                      (cond
                        (and (= 2 (count v)) (true? (is-pair? (v 0) (v 1)))) ""
                        (= 1 (count v)) v
                        :else      
                        (loop [v1 v 
                               i 0 
                               size (count v1)]
                          ; end when i is on the last position
                          (if (= i (-> v1 count dec))
                            v1
                            (let [a (v1 i)
                                  b (-> i inc v1)
                                  pair (is-pair? a b)]
                              (if (true? pair)
                                (recur (remove-pair v1 i) i (- size 2))
                                (recur v1 (inc i) size))
                              )))))
        processed (filter-brackets s)]
    (if (empty? processed)
      true
      (loop [v processed]
        (let [v1 (strip-pairs v)]
          (cond
            (= (count v) (count v1)) false
            (empty? v1) true
            :else (recur v1))
          )))))

;; Tests

(println (balanced-brackets? "This string has no brackets."))

(println (balanced-brackets? "class Test {
                             public static void main(String[] args) {
                             System.out.println(\"Hello world.\");
                             }
                             }"))

(println (not (balanced-brackets? "(start, end]")))

(println (not (balanced-brackets? "())")))

(println (not (balanced-brackets? "[ { ] } ")))

(println (balanced-brackets? "([]([(()){()}(()(()))(([[]]({}()))())]((((()()))))))"))

(println (not (balanced-brackets? "([]([(()){()}(()(()))(([[]]({}([)))())]((((()()))))))")))

(println (not (balanced-brackets? "[")))