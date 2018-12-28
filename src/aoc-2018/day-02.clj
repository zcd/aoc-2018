(ns aoc-2018.day-02
  (:use [aoc-2018.core])
  (:use [clojure.math.combinatorics :only [cartesian-product]]))

(defn twoseq-and-threeseq
  [sequence]
  (let [token-counts (->> sequence
                        (group-by identity)
                        (map (fn [[key vals]]
                               [key (count vals)]))
                        (group-by (fn [[key count]]
                                    count)))]
    [(if (contains? token-counts 2) 1 0)
     (if (contains? token-counts 3) 1 0)]))

(twoseq-and-threeseq "wsehyudplumeqvajtbiosngkxc")

(->> (read-lines "resources/input-02")
   (map twoseq-and-threeseq)
   (reduce (fn [[twos-l threes-l]
               [twos-r threes-r]]
             [(+ twos-l twos-r)
              (+ threes-l threes-r)]))
   (apply *))

(defn char-diff
  [sl sr]
  (apply + (map (fn [cl cr]
                  (if (not= cl cr) 1 0))
                sl sr)))

(char-diff "abcde" "axcye")
(char-diff "fguij" "fghij")

(->> (read-lines "resources/input-02")

   ;; Cartesian product with itself
   (repeat)
   (take 2)
   (apply cartesian-product)

   ;; find the right pair of labels
   (filter (comp (partial = 1)
                 (partial apply char-diff)))
   (first)

   ;; find the matching characters in the pair
   (apply map vector)
   (filter (partial apply =))
   (map first)
   (apply str))
