;;;;    Advent of Code 2024 - Day 2 part One and Two
;;;;    https://adventofcode.com/2024/day/2
;;;;
;;;;    Solutions in Clojure
;;;;    (Ter leering ende vermaeck...)
;;;;
;;;;    The number of safe reports is:           624
;;;;    The number of safe dampened reports is:  658 
;;;;
;;;;    (cl) by Arno Jacobs, 2024-12-05
;;;;    

(ns p02ab
  (:require [clojure.string :as str]))

;;;; Read the data-set
(defn get-lines 
  [file]
  (->> file
       slurp
       (str/split-lines)))

(defn parse-line 
  [line]
  (mapv parse-long (str/split (str/trim line) #" ")))


(def data-set (get-lines "./data/inputDay02_2024.txt"))
(def numbers (mapv parse-line data-set))

;;;  Part 1

;;; Counting helper
;;; If up   - return '1' for a safe-up numbers list and '0' if it's not
;;; If down - return '1' for a safe-down numbers list and '0' if it's not
(defn safe-up-or-down
  [direction numbers]
  (if (< (count numbers) 2)
    1
    (let [delta (- (* direction (nth numbers 1)) (* direction (first numbers)))]
      (if (and (> delta 0) (< delta 4))
        (safe-up-or-down direction (rest numbers))
        0))))
    
;;; Return '1' for a safe numbers list and '0' if it's not
;;; A list of numbers is either safe-up (exclusive) or safe-down
(defn safe
  [numbers]
  (+ (safe-up-or-down 1 numbers) (safe-up-or-down -1 numbers)))

(defn count-safe 
  [numbers]
  (reduce + (mapv safe numbers)))


;;; Part 2

;;; Remove one element from a list of 'elements' at index 'ix'
(defn remove-by-index
  [ix elements]
  (concat (take ix elements) (drop (+ ix 1) elements)))

(defn one-dampened-safe
  [ix numbers]
  (if (< ix 0)
    0                                              ;;; NO dampened safe
    (let [partial (remove-by-index ix numbers) 
          sub-safe (+ (safe-up-or-down 1 partial) (safe-up-or-down -1 partial))]
      (if (= 0 sub-safe)
        (one-dampened-safe (- ix 1) numbers)       ;;; NOT safe, check next dampened
        1))))                                      ;;; At least one safe - 0K. done.

;;; Check for safe list with removing one element per safe-test
;;; Return '1' for any dampened safe numbers list and '0' if there is none
(defn dampened-safe
  [numbers]
  (one-dampened-safe (- (count numbers) 1) numbers))

(defn count-dampened-safe
  [numbers]
  (reduce + (mapv dampened-safe numbers)))

;;; The 'main' program - - -
(defn program []
  (println "Advent of Code 2024 - day 2  (Clojure)")
  (print   "Part one: The number of safe reports is:          ")
  (println (count-safe numbers))
  (print   "Part two: The number of dampened safe reports is: ")
  (println (count-dampened-safe numbers))
  (println "0K.\n"))

;; Run in terminal via: clojure -M p02ab.clj
(program)

