(ns aoc-2018.core
  (:use [clojure.string :only [split-lines]]))

(defn read-lines
  [file-path]
  (-> (slurp file-path)
     (split-lines)))

