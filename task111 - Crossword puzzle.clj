
;; Crossword puzzle
;;
;; Write a function that takes a string and a partially-filled crossword puzzle board, and determines if the input string can be legally placed onto the board.
;;
;; The crossword puzzle board consists of a collection of partially-filled rows. 
;; Empty spaces are denoted with an underscore (_), unusable spaces are denoted with a hash symbol (#), 
;; and pre-filled spaces have a character in place; the whitespace characters are for legibility and should be ignored.
;;
;; For a word to be legally placed on the board:
;; - It may use empty spaces (underscores)
;; - It may use but must not conflict with any pre-filled characters.
;; - It must not use any unusable spaces (hashes).
;; - There must be no empty spaces (underscores) or extra characters before or after the word (the word may be bound by unusable spaces though).
;; - Characters are not case-sensitive.
;; - Words may be placed vertically (proceeding top-down only), or horizontally (proceeding left-right only).

(defn is-valid? [word puzzle]
  (let [remove-spaces (fn [puzzle]
                        (map (fn [string] (apply str (filter #(or (Character/isLetter %) (= % \_) (= \# %)) string))) puzzle))
        get-positions (fn [puzzle]
                        (concat puzzle (apply map str puzzle)))
        split-by-barrier  (fn [barrier word]
                            (let [[ftemp stemp] (vec (split-at (.indexOf (vec word) barrier) word))
                                  remove-barrier  (fn [w barrier]
                                                    (cond
                                                      (empty? w) ""
                                                      (= -1 (.indexOf (vec w) barrier)) (apply str w)
                                                      :elsev (apply str (remove #(= barrier %) w))))]
                              (filter #(not (empty? %)) [(remove-barrier ftemp barrier) (remove-barrier stemp barrier)])))
        is-word-applicable? (fn [word1 word2]
                              (let [n (count word1)
                                    is-char-applicable? (fn [char1 char2]
                                                          (cond
                                                            (= char1 char2) true
                                                            (or (= \_ char1) (= \_ char2)) true
                                                            :else false))]
                                (loop [i 0 w1 (vec word1) w2 (vec word2) good-chars 0]
                                  (if (= i n)
                                    (= n good-chars)
                                    (recur 
                                      (inc i) 
                                      (rest w1) 
                                      (rest w2)
                                      (if (is-char-applicable? (first w1) (first w2)) (inc good-chars) good-chars))))))
        positions1 (-> puzzle remove-spaces get-positions)
        positions2 (flatten (map #(split-by-barrier \# %) positions1))
        positions3 (filter #(= (count word) (count %)) positions2)
        positions4 (filter #(not (= -1 (.indexOf (vec %) \_))) positions3)]
    (not (empty? (filter #(true? (is-word-applicable? % word)) positions4)))))

;; Tests

(println (= true  (is-valid? "the" ["_ # _ _ e"])))

(println (= false (is-valid? "the" ["c _ _ _"
                                    "d _ # e"
                                    "r y _ _"])))

(println (= true  (is-valid? "joy" ["c _ _ _"
                                    "d _ # e"
                                    "r y _ _"])))

(println (= false (is-valid? "joy" ["c o n j"
                                    "_ _ y _"
                                    "r _ _ #"])))

(println (= true  (is-valid? "clojure" ["_ _ _ # j o y"
                                        "_ _ o _ _ _ _"
                                        "_ _ f _ # _ _"])))