
;; Black Box Testing

;; Clojure has many sequence types, which act in subtly different ways. 
;; The core functions typically convert them into a uniform "sequence" type and work with them that way, 
;; but it can be important to understand the behavioral and performance differences 
;; so that you know which kind is appropriate for your application.
;;
;; Write a function which takes a collection and returns one of :map, :set, :list, or :vector - describing the type of collection it was given.
;; You won't be allowed to inspect their class or use the built-in predicates like list? - the point is to poke at them and understand their behavior.

(defn bbt [s]
  (let [is-map? (fn [s]
                  (if (empty? s)
                    (let [s2 (conj s [:a :a])
                          r (get s2 :a)]
                      (if (nil? r)
                        false
                        true))	  
                    (let [f (flatten (cons (first s) '()))]
                      (if (= 1 (count f))
                        false
                        true))
                    ))
        is-set? (fn [s]
                  (if (empty? s)
                    (let [s2 (conj s :a :a)]
                      (if (= 1 (count s2))
                        true
                        false))
                    (let [f (first s)
                          s2 (conj s f)
                          r (filter #(= % f) s2)]
                      (if (= 1 (count r))
                        true
                        false))
                    ))
        is-vector? (fn [s]
                     (if (empty? s)
                       (let [s2 (conj s 1 2)]
                         (if (= 1 (first s2))
                           true
                           false))
                       (let [f (first s)
                             s2 (conj s f)]
                         (if (= f (last s2))
                           true
                           false))
                       ))
        ]
    (cond
      (is-map? s) :map
      (is-set? s) :set
      (is-vector? s) :vector
      :else :list
      )))

;; Tests

(println (= :map (bbt {:a 1, :b 2})))

(println (= :list (bbt (range (rand-int 20)))))

(println (= :vector (bbt [1 2 3 4 5 6])))

(println (= :set (bbt #{10 (rand-int 5)})))

(println (= [:map :set :vector :list] (map bbt [{} #{} [] ()])))