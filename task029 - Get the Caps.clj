
;; Get the Caps

;; Write a function which takes a string and returns a new string containing only the capital letters.

(defn fc [inp] 
 (apply str (filter #(Character/isUpperCase %) inp)))

;; Tests

(println (= (fc "HeLlO, WoRlD!") "HLOWRD"))

(println (empty? (fc "nothing")))

(println (= (fc "$#A(*&987Zf") "AZ"))