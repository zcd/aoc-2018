(ns aoc-2018.day-03
  (:use [aoc-2018.core])
  (:use [clojure.math.combinatorics :only [cartesian-product]])
  (:require [instaparse.core :as insta]))

(def parse-claim
  (insta/parser
   "S := id offset dims;
    id := '#' number ;
    offset := ' @ ' from-left ',' from-top ;
    dims := ': ' width 'x' height ;
    from-left := number ;
    from-top := number ;
    width := number ;
    height: number ;
    number := digit+ ;
    <digit> := #'[0-9]' ;"))

(defn interpret-claim
  [claim-string]
  (->> claim-string
     (parse-claim)
     (insta/transform
      {:number (comp #(Integer/parseInt %) str)
       :id (fn [_ id]
             {:id id})
       :offset (fn [_ horiz _ verti]
                 (conj {} horiz verti))
       :dims (fn [_ w _ h]
               (conj {}  w h))
       :S conj})))

(interpret-claim "#123 @ 3,2: 5x4")

(def example-3
  ["#1 @ 1,3: 4x4"
   "#2 @ 3,1: 4x4"
   "#3 @ 5,5: 2x2"])

;; really dumb solution
(defn get-search-space
  [claims]
  (let [leftmost (->> claims
                    (map :from-left)
                    (apply min))
        rightmost (->> claims
                     (map #(+ (:from-left %) (:width %)))
                     (apply max))
        topmost (->> claims
                   (map :from-top)
                   (apply min))
        bottommost (->> claims
                      (map #(+ (:from-top %) (:height %)))
                      (apply max))]
    (cartesian-product (range leftmost (+ rightmost 1))
                       (range topmost (+ bottommost 1)))))

(defn is-within-claim
  [[x y] claim]
  (and (< (:from-left claim) x)
     (<= x (+ (:from-left claim) (:width claim)))
     (< (:from-top claim) y)
     (<= y (+ (:from-top claim) (:height claim)))))

(defn num-enclosing-claims
  [p claims]
  (apply + (map #(if (is-within-claim p %) 1 0) claims)))

;; TODO: git gud and solve this efficiently
(defn overlapping-area
  [claims]
  (->> claims
     (get-search-space)
     (filter (fn [p]
               (<= 2 (num-enclosing-claims p claims))))
     (count)))

(defn are-claims-overlapping?
  [claim1 claim2]
  (and (< (:from-left claim2) (+ (:from-left claim1)
                                 (:width claim1)))
     (< (:from-top claim2) (+ (:from-top claim1)
                              (:height claim1)))

     (< (:from-left claim1) (+ (:from-left claim2)
                               (:width claim2)))
     (< (:from-top claim1) (+ (:from-top claim2)
                              (:height claim2)))))

;; Find the claim that only overlaps with itself.
(defn find-non-overlapping-claim
  [claims]
  (filter (fn [c]
            (empty? (filter #(and (not= c %)
                                (are-claims-overlapping? c %))
                            claims)))
          claims))

(->> (read-lines "resources/input-03")
   (map interpret-claim)
   find-non-overlapping-claim)
