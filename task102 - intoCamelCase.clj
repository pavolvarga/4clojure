
;; intoCamelCase

;; When working with java, you often need to create an object with fieldsLikeThis, 
;; but you'd rather work with a hashmap that has :keys-like-this until it's time to convert. 
;; Write a function which takes lower-case hyphen-separated strings and converts them to camel-case strings.

(defn into-camel-case [s]
  (let [words (clojure.string/split s #"-")]
    (if (= 1 (count words))
      s
      (let [fw (first words)
            ow (map clojure.string/capitalize (rest words))]
        (apply str (into [fw] ow))
      ))))

;; Tests

(println (= (into-camel-case "something") "something"))

(println (= (into-camel-case "multi-word-key") "multiWordKey"))

(println (= (into-camel-case "leaveMeAlone") "leaveMeAlone"))