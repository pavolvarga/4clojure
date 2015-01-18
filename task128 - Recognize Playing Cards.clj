
;; Recognize Playing Cards

;; A standard American deck of playing cards has four suits - spades, hearts, diamonds, and clubs - and thirteen cards in each suit. 
;; Two is the lowest rank, followed by other integers up to ten; then the jack, queen, king, and ace.
;;
;; It's convenient for humans to represent these cards as suit/rank pairs, such as H5 or DQ: the heart five and diamond queen respectively. 
;; But these forms are not convenient for programmers, so to write a card game you need some way to parse an input string into meaningful components. 
;; For purposes of determining rank, we will define the cards to be valued from 0 (the two) to 12 (the ace)
;;
;; Write a function which converts (for example) the string "SJ" into a map of {:suit :spade, :rank 9}. 
;; A ten will always be represented with the single character "T", rather than the two characters "10".

(defn cd [x]
  (let [suits {"S" :spade, "H" :heart, "D" :diamond, "C" :club}
        s (suits (str (first x)))
        ranks (zipmap (flatten 
                        (conj [] 
                              (for [x (range 2 10)] (str x)) 
                              (for [y '("T" "J" "Q" "K" "A")] y))) 
                      (range 0 13))
        r (ranks (str (last x)))]
    (into (hash-map :suit s) (hash-map :rank r))))

;; Tests

(println (= {:suit :diamond :rank 10} (cd "DQ")))

(println (= {:suit :heart :rank 3} (cd "H5")))

(println (= {:suit :club :rank 12} (cd "CA")))

(println (= (range 13) (map (comp :rank cd str)
                   '[S2 S3 S4 S5 S6 S7
                     S8 S9 ST SJ SQ SK SA])))