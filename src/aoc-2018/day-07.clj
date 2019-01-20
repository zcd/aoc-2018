(ns aoc-2018.day-07
  (:use [aoc-2018.core]))

(defn read-edge
  [line]
  (let [tokens (clojure.string/split line #" ")]
    [(nth (nth tokens 1) 0)
     (nth (nth tokens 7) 0)]))

(def sample-input
  (->>
   "Step C must be finished before step A can begin.
Step C must be finished before step F can begin.
Step A must be finished before step B can begin.
Step A must be finished before step D can begin.
Step B must be finished before step E can begin.
Step D must be finished before step E can begin.
Step F must be finished before step E can begin."
   (clojure.string/split-lines)
   (map read-edge)))

(def input-edges
  (->> "resources/input-07"
     (read-lines)
     (map read-edge)))

(def back-edges
  (memoize (comp
            (partial reduce-kv
                     (fn [m k v]
                       (assoc m k
                              (set (map first v))))
                     {})
            (partial group-by peek))))
(def fwd-edges
  (memoize (comp
            (partial reduce-kv
                     (fn [m k v]
                       (assoc m k
                              (set (map peek v))))
                     {})
            (partial group-by first))))

(defn initial-horizon
  [edges]
  (->> edges
     (map first)
     (filter (complement #(contains?
                           (back-edges edges)
                           %)))
     (into (sorted-set))))

(defn serialize-dag
  [edges]
  (loop [horizon (initial-horizon edges)
         acc []]
    (if (empty? horizon)
      acc
      (let [curr (->> horizon
                    (filter (fn [candidate]
                              (empty? (clojure.set/difference
                                       ((back-edges edges) candidate)
                                       (set acc)))))
                    (first))]
        (recur (into (clojure.set/difference horizon
                                             #{curr})
                     ((fwd-edges edges) curr))
               (conj acc curr))))))

(defn node-cost
  [node]
  (+ (int node)
     (- (int \A))
     61))

(defn build-timeline
  [edges num-tasks]

  (defn are-all-deps-complete?
    [complete]
    (fn [node]
      (empty? (clojure.set/difference
               ((back-edges edges) node)
               complete))))

  (loop [horizon (initial-horizon edges)
         tasks #{}
         timestamp 0
         acc []]

    (let [num-free-workers (- num-tasks (count tasks))
          eligible-nodes (filter (are-all-deps-complete?
                                  (into #{} (map :node acc)))
                                 horizon)
          nodes-to-enqueue (take num-free-workers eligible-nodes)]

      (cond
        ;; done
        (and (empty? horizon) (empty? tasks))
        acc

        ;; there are free workers, so enqueue the next available task
        (not (empty? nodes-to-enqueue))
        (recur (clojure.set/difference horizon
                                       nodes-to-enqueue)
               (into tasks (map (fn [node]
                                  {:node node
                                   :eta (+ timestamp (node-cost node))})
                                nodes-to-enqueue))
               timestamp
               acc)

        :else
        (let [{earliest-eta :eta
               node :node
               :as soonest-task} (apply min-key :eta tasks)]
          (recur (into horizon (clojure.set/difference ((fwd-edges edges) node)
                                                       ;; assume no cycles
                                                       (into #{} (map :node tasks))))
                 (clojure.set/difference tasks #{soonest-task})
                 earliest-eta
                 (conj acc soonest-task)))))))
