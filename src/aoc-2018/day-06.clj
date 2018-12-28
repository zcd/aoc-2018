(ns aoc-2018.day-06
  (:use [aoc-2018.core])
  (:require [clojure.string :as s]))

(defn read-coord
  [line]
  (let [[x y] (s/split line #", ")]
    [(Integer/parseInt (s/trim x))
     (Integer/parseInt (s/trim y))]))

(def sample-coords
  (->> "1, 1
      1, 6
      8, 3
      3, 4
      5, 5
      8, 9"
     (s/split-lines)
     (map read-coord)))

(defn manhattan-dist
  [a b]
  (apply +
         (map (comp #(Math/abs %) -) a b)))

(defn euclidean-dist
  [[a b] [x y]]
  (Math/sqrt (+ (Math/pow (- a x) 2)
                (Math/pow (- b y) 2))))

(defn minkowski-dist
  [n [a b] [x y]]
  (Math/pow (+ (Math/abs (Math/pow (- a x) n))
               (Math/abs (Math/pow (- b y) n)))
            (/ 1 (double n))))

(defn tag-coord
  [coord label]
  {:label label
   :coord coord})

(defn alphanum-tag-coords
  [coords]
  (map tag-coord
       coords
       (map (comp str char)
            (range (int \A) (int \Z)))))

(defn closest-landmarks
  [metric point tagged-landmarks]
  (->> tagged-landmarks
     (group-by (fn [{lm-loc :coord}]
                 (metric lm-loc point)))
     (apply min-key first)))

(defn make-formatter
  ([tagged-landmarks]
   (make-formatter manhattan-dist
                   tagged-landmarks))
  ([metric tagged-landmarks]
   (fn [point]
     (->> tagged-landmarks
        (closest-landmarks metric point)
        ((fn [[dist [{label :label} & more]]]
           (cond (not (empty? more)) "."
                 (= dist 0) label
                 :else (clojure.string/lower-case label))))))))

(defn materialize-grid
  [[xi xf]
   [yi yf]
   formatter]
  (apply str
         (for [y (range yi yf)
               x (range xi xf)]
           (str (formatter [x y])
                (if (= xf (inc x)) "\n" "")))))

(def input-coords
  (->> (read-lines "resources/input-06")
     (map read-coord)))

(defn range-heuristic
  [coords]
  (let* [dimensionless (apply concat coords)
         lb (apply min dimensionless)
         ub (apply max dimensionless)
         scaled-spread (quot (- ub lb) 3)]
    [(- lb scaled-spread) (+ ub scaled-spread)]))

(defn interesting-plots
  [coords]
  (doseq [[description metric] {"manhattan" manhattan-dist
                                "minkowski-1.5" (partial minkowski-dist 1.5)
                                "euclidean" euclidean-dist
                                "minkowski-3" (partial minkowski-dist 3)
                                "minkowski-9" (partial minkowski-dist 9)
                                "minkowski-99" (partial minkowski-dist 99)
                                "minkowski-999" (partial minkowski-dist 999)}]
    (let [output-path (str "outputs/output-6-" description)
          landmarks (alphanum-tag-coords coords)]
      (when-not (.exists (clojure.java.io/file output-path))
        (->> landmarks
           (make-formatter metric)
           (materialize-grid
            (update 
             (tight-range (comp first :coord) landmarks) 1 inc)
            (update
             (tight-range (comp peek :coord) landmarks) 1 inc))
           (spit output-path))
        (println "wrote" output-path)))))

;; enough horsing around and assume manhattan metric from here on
;; also note that there may be more than 26 input landmarks

(def input-landmarks
  (map tag-coord
       input-coords
       (map (comp str char)
            (range))))

(defn tight-range
  [getter coords]
  (let [xs (map getter coords)]
    [(apply min xs) (apply max xs)]))

(defn tight-border-set
  [landmarks]
  (let [[xlb xub] (tight-range (comp first :coord)
                               landmarks)
        [ylb yub] (tight-range (comp peek :coord)
                               landmarks)]
    (concat (for [x (range (inc xlb) xub)]
              [x ylb])
            (for [x (range xlb xub)]
              [x yub])
            (for [y (range ylb yub)]
              [xlb y])
            (for [y (range ylb (inc yub))]
              [xub y]))))

(defn infinite-landmarks
  [tagged]
  (->> tagged
     (tight-border-set)
     (map (fn [point]
            (closest-landmarks manhattan-dist
                               point
                               tagged)))
     (map peek)
     (filter (comp #(= 1 %) count))
     (map (comp :label peek))
     (into #{})))

(defn interior-domain
  [tagged]
  (let [[xlb xub] (tight-range (comp first :coord)
                               tagged)
        [ylb yub] (tight-range (comp peek :coord)
                               tagged)]
    (for [x (range (inc xlb) xub)
          y (range (inc ylb) yub)]
      [x y])))

(defn finite-areas
  [landmarks]
  (let* [infinites (infinite-landmarks landmarks)]
    (->> (interior-domain landmarks)
       (map (fn [point]
              (closest-landmarks manhattan-dist
                                 point
                                 landmarks)))
       (map (fn [[_ candidates]]
              (map :label candidates)))
       (filter (comp #(= 1 %) count))
       (map first)
       (filter #(not (contains? infinites %)))
       (group-by identity)
       (reduce-kv (fn [m k v]
                    (assoc m k (count v)))
                  {}))))

(comment apply max-key peek (finite-areas input-landmarks))

(defn distance-to-all-landmarks
  [landmarks point]
  (->> landmarks
     (map (fn [{coord :coord}]
            (manhattan-dist coord point)))
     (apply +)))

(defn hot-zone
  [landmarks cooling-distance]
  (->> (interior-domain landmarks)
     (filter (comp #(< % cooling-distance)
                   (partial distance-to-all-landmarks
                            landmarks)))
     (count)))

(comment hot-zone input-landmarks 1e4)
