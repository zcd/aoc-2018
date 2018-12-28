(ns aoc-2018.day-05
  (:use [aoc-2018.core])
  (:use [clojure.core.reducers :as r])
  (:use [clojure.string :as s]))

(def sample-polymer "dabAcCaCBAcCcaDA")

(defn are-opposite-case?
  [c1 c2]
  (and (not= c1 c2)
     (= (Character/toUpperCase c1)
        (Character/toUpperCase c2))))

(defn reaction-pass
  [polymer]
  (loop [pending polymer
         acc []]
    (let [[next-char & rest-chars] pending]
      (cond (empty? pending)
            acc

            (empty? acc)
            (recur rest-chars
                   [next-char])

            (are-opposite-case? (peek acc) next-char)
            (recur rest-chars
                   (pop acc))

            :else
            (recur rest-chars
                   (conj acc next-char))))))

(def polymer (->> (slurp "resources/input-5")
                (clojure.string/trim)))

(->> polymer
   (reaction-pass)
   (count))

(->> (range (int \a) (int \z))
   (r/map char)
   (r/map (fn [ch]
            (s/replace polymer
                       (re-pattern (str "[" ch (Character/toUpperCase ch) "]"))
                       "")))
   (r/map reaction-pass)
   (r/map count)
   (r/reduce conj)
   (apply min))
