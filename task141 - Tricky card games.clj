
;; Tricky card games

;; In trick-taking card games such as bridge, spades, or hearts, cards are played in groups known as "tricks" 
;; - each player plays a single card, in order; the first player is said to "lead" to the trick. 
;; After all players have played, one card is said to have "won" the trick. 
;; How the winner is determined will vary by game, but generally the winner is the highest card played in the suit that was led. 
;; Sometimes (again varying by game), a particular suit will be designated "trump", 
;; meaning that its cards are more powerful than any others: if there is a trump suit, 
;; and any trumps are played, then the highest trump wins regardless of what was led. 

;; Your goal is to devise a function that can determine which of a number of cards has won a trick. 
;; You should accept a trump suit, and return a function winner. 
;; Winner will be called on a sequence of cards, and should return the one which wins the trick. 
;; Cards will be represented in the format returned by Problem 128, 
;; Recognize Playing Cards: a hash-map of :suit and a numeric :rank. Cards with a larger rank are stronger. 

(defn make-winner [i]
  (cond 
    (nil? i)
      (fn win-from-rank [c]
        (loop [c1 (rest c), win (first c)]
          (if (empty? c1)
            win
            (let [f (first c1)]
              (cond
                (and (= (win :suit) (f :suit)) (> (f :rank) (win :rank)))        (recur (rest c1) f) 
                (and (= (win :suit) (f :suit)) (not (> (f :rank) (win :rank))))  (recur (rest c1) win) 
                (and (not= (win :suit) (f :suit)))                               (recur (rest c1) win))))))
    (or (= i :spade) (= i :club) (= i :heart) (= i :diamond))
      (fn win-from-suit [c]
        (loop [c1 c, win nil]
          (if (empty? c1)
            win
            (let [f (first c1)]
              (cond
                (and (= nil win)    (= i (f :suit)))                                 (recur (rest c1) f)
                (and (= nil win)    (not= i (f :suit)))                              (recur (rest c1) win)
                (and (not= nil win) (= i (f :suit)) (> (f :rank) (win :rank)))       (recur (rest c1) f)
                (and (not= nil win) (= i (f :suit)) (not (> (f :rank) (win :rank)))) (recur (rest c1) win)
                (and (not= nil win) (not= i (f :suit)))                              (recur (rest c1) win))))))
    (map? i)
      (fn win-from-map [c]
        (let [{suit :suit, rank :rank} i]
          (loop [c1 c, win nil]
            (if (empty? c1)
              win
              (let [f (first c1)]
                (cond
                  (and (= nil win)    (= suit (f :suit)))                           (recur (rest c1) f)
                  (and (= nil win)    (not= suit (f :suit)))                        (recur (rest c1) win) 
                  (and (not= nil win) (= suit (f :suit))     (= (f :rank) rank))    (recur (rest c1) f)
                  (and (not= nil win) (= suit (f :suit))     (not= (f :rank) rank)) (recur (rest c1) win)
                  (and (not= nil win) (not= suit (f :suit)))                        (recur (rest c1) win)))))))))

;;Tests

(println (let [notrump (make-winner nil)]
           (and (= {:suit :club :rank 9}  (notrump [{:suit :club  :rank 4} {:suit :club :rank 9}]))
                (= {:suit :spade :rank 2} (notrump [{:suit :spade :rank 2} {:suit :club :rank 10}])))))

(println (= {:suit :club :rank 10} ((make-winner :club) [{:suit :spade :rank 2} {:suit :club :rank 10}])))

(println (= {:suit :heart :rank 8} ((make-winner :heart) [{:suit :heart :rank 6} {:suit :heart :rank 8} {:suit :diamond :rank 10} {:suit :heart :rank 4}])))