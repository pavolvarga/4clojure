
;; Best Hand
;;
;; Following on from Recognize Playing Cards, determine the best poker hand that can be made with five cards. 
;; The hand rankings are listed below for your convenience.
;;
;;  1. Straight flush: All cards in the same suit, and in sequence
;;  2. Four of a kind: Four of the cards have the same rank
;;  3. Full House: Three cards of one rank, the other two of another rank
;;  4. Flush: All cards in the same suit
;;  5. Straight: All cards in sequence (aces can be high or low, but not both at once)
;;  6. Three of a kind: Three of the cards have the same rank
;;  7. Two pair: Two pairs of cards have the same rank
;;  8. Pair: Two cards have the same rank
;;  9. High card: None of the above conditions are met

(defn aph [hand]
  (letfn [
    (extract-suit [hand]
      (map #(first (seq %)) hand))
    (extract-rank [hand]
      (map #(second (seq %)) hand))
    (convert-rank-ace-highest [rank]
      (map 
        (fn [r]
          (cond
            (= \T r) 10
            (= \J r) 11
            (= \Q r) 12
            (= \K r) 13
            (= \A r) 14
            :else (Integer/parseInt (str r))))
        rank))
    (convert-rank-ace-lowest [rank]
      (map 
        (fn [r]
          (cond
            (= \T r) 10
            (= \J r) 11
            (= \Q r) 12
            (= \K r) 13
            (= \A r) 1
            :else (Integer/parseInt (str r))))
        rank))
    (is-in-order? [rank]
      (let [temp (set
                    (map 
                      (fn [t] (- (second t) (first t)))
                      (partition 2 (-> (map #(repeat 2 %) rank) flatten rest drop-last))))]
        (and (= 1 (count temp)) (= 1 (Math/abs (first temp))))))
    (is-straight-flush? [hand]
      (let [suit (extract-suit hand)
            rank (convert-rank-ace-highest (extract-rank hand))]
        (and 
          (is-in-order? rank)
          (= 1 (count (group-by identity suit)))
          (or (= 14 (first rank)) (= 14 (last rank))))))
    (is-four-of-kind? [hand]
      (let [rank (extract-rank hand)
            grouped-rank (group-by identity rank)]
        (and
          (= 2 (count grouped-rank)) 
          (or 
            (= 4 (-> grouped-rank vals first count)) 
            (= 4 (-> grouped-rank vals second count))))))
    (is-full-house? [hand]
      (let [rank (extract-rank hand)
            grouped-rank (group-by identity rank)
            ordered-rank (sort-by count (vals grouped-rank))]
      (and 
        (= 2 (count ordered-rank))
        (= 2 (-> ordered-rank first count))
        (= 3 (-> ordered-rank second count)))))
    (is-flush? [hand]
      (let [suit (extract-suit hand)]
        (= 1 (count (group-by identity suit)))))
    (is-straight? [hand]
      (let [extracted-rank   (extract-rank hand)
            rank-ace-highest (convert-rank-ace-highest extracted-rank)
            rank-ace-lowest  (convert-rank-ace-lowest  extracted-rank)]
        (or (is-in-order? rank-ace-highest) (is-in-order? rank-ace-lowest))))
    (is-three-of-kind? [hand]
      (let [rank (extract-rank hand)
            grouped-rank (group-by identity rank)
            ordered-rank (vec (sort-by count (vals grouped-rank)))]
        (and
          (= 3 (count ordered-rank))
          (= 3 (count (ordered-rank 2))))))
    (is-two-pair? [hand]
      (let [rank (extract-rank hand)
            grouped-rank (group-by identity rank)
            ordered-rank (vec (sort-by count (vals grouped-rank)))]
        (and
          (= 3 (count ordered-rank))
          (= 2 (count (ordered-rank 1)))
          (= 2 (count (ordered-rank 2))))))
    (is-pair? [hand]
      (let [rank (extract-rank hand)
            grouped-rank (group-by identity rank)
            ordered-rank (sort-by count (vals grouped-rank))]
        (and
          (= 4 (count ordered-rank))
          (= 2 (count (last ordered-rank))))))]
    (cond
      (is-straight-flush? hand) :straight-flush
      (is-four-of-kind?   hand) :four-of-a-kind
      (is-full-house?     hand) :full-house
      (is-flush?          hand) :flush
      (is-straight?       hand) :straight
      (is-three-of-kind?  hand) :three-of-a-kind
      (is-two-pair?       hand) :two-pair
      (is-pair?           hand) :pair
      :else :high-card)))

;; Tests

(println (= :high-card (aph ["HA" "D2" "H3" "C9" "DJ"])))

(println (= :pair (aph ["HA" "HQ" "SJ" "DA" "HT"])))

(println (= :two-pair (aph ["HA" "DA" "HQ" "SQ" "HT"])))

(println (= :three-of-a-kind (aph ["HA" "DA" "CA" "HJ" "HT"])))

(println (= :straight (aph ["HA" "DK" "HQ" "HJ" "HT"])))

(println (= :straight (aph ["HA" "H2" "S3" "D4" "C5"])))

(println (= :flush (aph ["HA" "HK" "H2" "H4" "HT"])))

(println (= :full-house (aph ["HA" "DA" "CA" "HJ" "DJ"])))

(println (= :four-of-a-kind (aph ["HA" "DA" "CA" "SA" "DJ"])))

(println (= :straight-flush (aph ["HA" "HK" "HQ" "HJ" "HT"])))