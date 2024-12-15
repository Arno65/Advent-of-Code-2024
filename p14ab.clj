;;;;    Advent of Code 2024 - Day 14 part One and Two
;;;;    https://adventofcode.com/2024/day/14
;;;;
;;;;    Solutions in Clojure
;;;;    (Ter leering ende vermaeck...)
;;;; 
;;;;    Part one: safety factor after 100 seconds is:        218433348
;;;;    Part two: number of seconds for first easter egg is:      6512
;;;;
;;;;    (cl) by Arno Jacobs, 2024-12-15
;;;;    

(ns p14ab
  (:require [clojure.string :as str]))

;;; The function "get-all-integers" is in this file
(load-file "caoc/caoc.clj")

;;;; Read the data-set
(defn get-lines 
  [file]
  (->> file
       slurp
       (str/split-lines)))

(def robots (map caoc/get-all-integers (get-lines "./data/inputDay14_2024.txt")))


(def maxx 101)
(def halfx (int (/ maxx 2)))
(def maxy 103)
(def halfy (int (/ maxy 2)))
(def seconds 100)

;;;;  --- Part one --- --- --- --- --- --- --- --- --- --- ---

(defn walkies 
  [s robot]
  (let [px (nth robot 0)
        py (nth robot 1)
        vx (nth robot 2)
        vy (nth robot 3)]
    (list (mod (+ px (* s vx)) maxx) 
          (mod (+ py (* s vy)) maxy))))

(defn quadrant-count
  [locations q1 q2 q3 q4]
  (if (empty? locations)
    (list q1 q2 q3 q4)
    (let [xy (first locations)
          x (nth xy 0)
          y (nth xy 1)
          dq1 (if (and (< x halfx) (< y halfy)) 1 0)
          dq2 (if (and (< x halfx) (> y halfy)) 1 0)
          dq3 (if (and (> x halfx) (< y halfy)) 1 0)
          dq4 (if (and (> x halfx) (> y halfy)) 1 0)]
      (quadrant-count (rest locations) 
                      (+ q1 dq1) (+ q2 dq2) 
                      (+ q3 dq3) (+ q4 dq4)))))

(defn safety-factor
  [rs s]
  (let [rsw (map #(walkies s %) rs)]
  (reduce * (quadrant-count rsw 0 0 0 0))))


;;;;  --- Part two --- --- --- --- --- --- --- --- --- --- ---

(defn in-list 
  [xys xy]
  (if (empty? xys)
    false
    (if (= xy (first xys))
       true
       (in-list (rest xys) xy))))

(defn all-unique
  [rs]
  (if (empty? rs)
    true
    (if (in-list (rest rs) (first rs))
      false 
      (all-unique (rest rs)))))

(defn easter-egg
  [rs s]
  (let [rls (map #(walkies s %) rs)]
    (if (all-unique rls)
    s
    (easter-egg rs (+ s 1)))))


;;;;  --- main --- --- --- --- --- --- --- --- --- --- ---

(defn program []
  (println "Advent of Code 2024 - day 14  (Clojure)")
  (print   "Part one: safety factor after 100 seconds is:        ")
  (println (safety-factor robots seconds))
  (print   "Part two: number of seconds for first easter egg is:      ")
  (println (easter-egg robots 0)) 
  (println "0K.\n"))


;; Run in terminal via: clojure -M p14ab.clj
(program)

;; end of code
