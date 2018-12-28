(ns aoc-2018.day-04
  (:use [aoc-2018.core])
  (:require [instaparse.core :as insta]))

(def ex4-input
  (->> "
[1518-11-01 00:00] Guard #10 begins shift
[1518-11-01 00:05] falls asleep
[1518-11-01 00:25] wakes up
[1518-11-01 00:30] falls asleep
[1518-11-01 00:55] wakes up
[1518-11-01 23:58] Guard #99 begins shift
[1518-11-02 00:40] falls asleep
[1518-11-02 00:50] wakes up
[1518-11-03 00:05] Guard #10 begins shift
[1518-11-03 00:24] falls asleep
[1518-11-03 00:29] wakes up
[1518-11-04 00:02] Guard #99 begins shift
[1518-11-04 00:36] falls asleep
[1518-11-04 00:46] wakes up
[1518-11-05 00:03] Guard #99 begins shift
[1518-11-05 00:45] falls asleep
[1518-11-05 00:55] wakes up
"
     (clojure.string/split-lines)
     (filter (complement empty?))))

(def parse-log
  (insta/parser
   "S := timestamp event ;
    timestamp := <'['> number <'-'> number <'-'> number <' '> number <':'> number <']'> ;
    event := <' '> ( start | sleep | wake );
    start := <'Guard #'> guard-id <' begins shift'> ;
    guard-id := number ;
    sleep := <'falls asleep'> ;
    wake := <'wakes up'> ;
    number := digit+ ;
    <digit> := #'[0-9]' ;"))

(defn interpret-log
  [log-string]
  (->> log-string
     (parse-log)
     (insta/transform
      {:number (comp #(Integer/parseInt %) str)
       :timestamp (fn [y m d hr min]
                    (. java.time.LocalDateTime of
                       y m d hr min))
       :event identity
       :start (fn [[_ id]] id)
       :sleep (fn [] :sleep)
       :wake (fn [] :wake)
       :S vector})))

(defn group-logs
  [log-lines]
  (->> log-lines
     (sort)

     ;; delimit by arrival of new guard
     (partition-by (comp number? peek))
     (partition 2)

     ;; group into a single guard's sleeping habits
     (map (fn [[[start-event] circadian-cycle]]
            [start-event circadian-cycle]))
     (into (sorted-map))))

(def per-shift-logs
  (->> (read-lines "resources/input-4")
     (map interpret-log)
     (group-logs)))

(defn validate-one-shift
  [events]
  (->> events
     (partition 2)
     (every? (fn [[start end]]
               (and (= :sleep (peek start))
                  (= :wake (peek end)))))))

(defn validate-whole-log
  "Make sure that :sleep and :wake are alternating."
  [shift-logs]
  (every? (fn [[_ events]]
            (validate-one-shift events))
          shift-logs))

(->> per-shift-logs
   (validate-whole-log)
   (assert))

(defn sanitize-sleep-intervals
  [events]
  (->> events
     (partition 2)
     (map (fn [[[ts-lb _] [ts-ub _]]]
            [(.getMinute ts-lb) (.getMinute ts-ub)]))))

(def sleep-interval-mins
 (->> per-shift-logs

    (map (fn [[[_ guard-id] shift-events]]
           [guard-id
            (sanitize-sleep-intervals shift-events)]))

    ;; sleep intervals (mins) by guard id
    (group-by first)
    (reduce-kv (fn [m k v]
                 (assoc m k (->> v
                               (map peek)
                               (apply concat))))
               {})))

(def sleepiest-guard
  (->> sleep-interval-mins
     (apply max-key
            (fn [[_ sleep-intervals]]
              (->> sleep-intervals
                 (map (fn [[lb ub]] (- ub lb)))
                 (apply +))))
     (first)))

(defn- process-sleep-intervals-dbg
  [intervals]
  (loop [depth 0
         acc '()
         openers (sort-by first intervals)
         closers (sort-by peek intervals)]
    (let [[[lb _ :as next-opener] & rest-openers] openers
          [[_ ub :as next-closer] & rest-closers] closers]
      (cond (or (nil? lb) (nil? ub)) acc
            (< lb ub) (recur (inc depth)
                             (cons {:depth (inc depth)
                                    :lower-bound lb
                                    :upper-bound nil
                                    :encountered next-opener}
                                   acc)
                             rest-openers
                             closers)
            (> lb ub) (recur (dec depth)
                             (cons {:depth (dec depth)
                                    :lower-bound (:lower-bound (first acc))
                                    :upper-bound ub
                                    :encountered next-closer}
                                   acc)
                             openers
                             rest-closers)
            (= lb ub) (recur depth
                             (cons {:depth depth
                                    :lower-bound lb
                                    :upper-bound nil
                                    :encountered [next-opener
                                                  next-closer]}
                                   acc)
                             rest-openers
                             rest-closers)))))

(->> (sleep-interval-mins sleepiest-guard)
   (process-sleep-intervals)
   (apply max-key :depth)
   (:lower-bound)
   (* sleepiest-guard))


(->> sleep-interval-mins
   (map (fn [[guard-id intervals]]
          (assoc 
           (->> intervals
              (process-sleep-intervals)
              (apply max-key :depth))
           :id guard-id)))
   (apply max-key :depth)
   (#(* (:lower-bound %)
         (:id %))))

