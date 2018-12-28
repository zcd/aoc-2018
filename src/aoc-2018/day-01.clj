(ns aoc-2018.day-01
  (:use [aoc-2018.core]))

(->> (read-lines "resources/input-1")
   (map #(Integer/parseInt %))
   (apply +))

(defn first-cumulative-repeat
  [sequence]
  (loop [freqs (cycle sequence)
         seen #{}
         total 0]
    (let [front (first freqs)
          total (+ total front)]
      (if (contains? seen total)
        total
        (recur (rest freqs)
               (conj seen total)
               total)))))

(->> (read-lines "resources/input-1")
   (map #(Integer/parseInt %))
   (first-cumulative-repeat))
