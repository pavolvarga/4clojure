
;; Hello World

;; Write a function which returns a personalized greeting.

(defn create-pers-greating [s]
  (format  "Hello, %s!" s))

;; Tests

(println (= (create-pers-greating "Dave") "Hello, Dave!"))

(println (= (create-pers-greating "Jenn") "Hello, Jenn!"))

(println (= (create-pers-greating "Rhea") "Hello, Rhea!"))