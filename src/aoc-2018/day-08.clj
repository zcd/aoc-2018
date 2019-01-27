(ns aoc-2018.day-08)

(defrecord tree
    [metadata
     children])

(def sample-input
  (->> (clojure.string/split
      "2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2"
      #" ")
     (map read-string)))

(def full-input
    (->> (clojure.string/split
      (slurp "resources/input-08")
      #" ")
     (map read-string)))

(defn parse
  [contents]
  (first (parse-tree contents)))

(defn- parse-tree
  [[num-children
    num-metadata
    & contents]]
  (if (= num-children 0)
    (let [[metadata leftovers] (split-at num-metadata
                                         contents)]
      [(tree. metadata []) leftovers])
    (let [[children leftovers] (parse-subtrees num-children
                                               contents)
          [metadata leftovers] (split-at num-metadata
                                         leftovers)]
      [(tree. metadata children) leftovers])))

(defn- parse-subtrees
  [num-trees contents]
  (loop [n num-trees
         trees []
         stream contents]
    (if (= n 0)
      [trees stream]
      (let [[subtree leftovers] (parse-tree stream)]
        (recur (dec n)
               (conj trees subtree)
               leftovers)))))

(defn sum-metadata
  [{:keys [metadata children]}]
  (+ (apply + (map sum-metadata children))
     (apply + metadata)))

(defn- node-value
  [{:keys [metadata children]}]
  (if (empty? children)
    (apply + metadata)
    (let [num-children (count children)]
      (->> metadata
         (map dec)
         (filter #(< % num-children))
         (map (partial nth children))
         (map node-value)
         (apply +)))))

(def full-tree (parse full-input))
