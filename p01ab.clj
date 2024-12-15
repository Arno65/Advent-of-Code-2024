;;;;    Advent of Code 2024 - Day 1 part One and Two
;;;;    https://adventofcode.com/2024/day/1
;;;;
;;;;    Solutions in Clojure
;;;;    (Ter leering ende vermaeck...)
;;;;
;;;;    The total distance between the lists:     2031679
;;;;    Their similarity score:                  19678534
;;;;
;;;;    (cl) by Arno Jacobs, 2024-12-04
;;;;    

(ns p01ab
  (:require [clojure.string :as str]))

;;; The function "merge-sort" is in this file
(load-file "caoc/caoc.clj")

;;;; Read the data-set
(defn get-lines 
  [file]
  (->> file
       slurp
       (str/split-lines)))

(defn parse-line 
  [line]
  (mapv parse-long (str/split (str/trim line) #" ")))

(defn first-number
  [line]
  (->> line
       parse-line
       first))

(defn last-number 
  [line]
  (->> line
       parse-line
       last))

(def data-set (get-lines "./data/inputDay01_2024.txt"))
(def list1 (caoc/merge-sort (mapv first-number data-set)))
(def list2 (caoc/merge-sort (mapv last-number data-set)))


;;;  Part one

(defn total-distances
  [xs ys]
  (cond (empty? xs) 0
        (empty? ys) 0
        :else (let [d (abs (- (first xs) (first ys)))]
    (+ d (total-distances (rest xs) (rest ys))))))


;;; Part two

(defn tallyX
  [x ys]
  (* x (count (filter #(= x %) ys))))

(defn similarity-score
  [xs ys]
  (cond (empty? xs) 0
        :else (+ (tallyX (first xs) ys) (similarity-score (rest xs) ys))))


;;; The 'main' program - - -
(defn program []
  (println "Advent of Code 2024 - day 1  (Clojure)")
  (print   "Part one: The total distance between the lists:  ")
  (println (total-distances list1 list2))
  (print   "Part two: Their similarity score:               ")
  (println (similarity-score list1 list2))
  (println "0K.\n"))

;; Run in terminal via: clojure -M p01ab.clj
(program)

