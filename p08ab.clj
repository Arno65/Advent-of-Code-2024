;;;;    Advent of Code 2024 - Day 8 part One and Two
;;;;    https://adventofcode.com/2024/day/8
;;;;
;;;;    Solutions in Clojure
;;;;    (Ter leering ende vermaeck...)
;;;; 
;;;;     Part one: The number of unique antinode locations is:  392
;;;;     Part two: The number of unique antinode locations is: 1235
;;;;
;;;;    (cl) by Arno Jacobs, 2024-12-09
;;;;    

(ns p08ab
  (:require [clojure.string :as str]))

;;;; Read the data-set
(defn get-lines 
  [file]
  (->> file
       slurp
       (str/split-lines)))

(def data-set (get-lines "./data/inputDay08_2024.txt"))
(def maxx (count (first data-set)))  ;; maximum columns range
(def maxy (count data-set))          ;; maximum rows range
(def no-antenna \.)

(defn get-antenna-data
  [roof x y]
  (let [antenna (nth (nth roof y) x)]
    (if (= antenna no-antenna) 
      nil 
      [antenna [x y]])))

;;; scan the roof for all antennas
;;; store antenna 'name' or 'frequency' and [x y] location 
(defn cummulate-antenna-data 
  [roof x y]
  (if (>= y maxy)
    nil
    (if (>= x maxx)
      (cummulate-antenna-data roof 0 (+ y 1)) 
      (let [antenna (get-antenna-data roof x y)] 
        (if (empty? antenna)
          (cummulate-antenna-data roof (+ x 1) y) 
          (cons antenna
                (cummulate-antenna-data roof (+ x 1) y)))))))

(def antenna-data (cummulate-antenna-data data-set 0 0))

;; helper code - range checking
(defn on-roof
  [x y]
  (if (and (>= x 0) 
           (< x maxx)
           (>= y 0)
           (< y  maxy))
    true
    false))

(defn smaller-pair
  [x1 y1 x2 y2]
  (if (< x1 x2)
    true
    (if (and (= x1 x2) (< y1 y2))
      true
      false)))

(defn merge-to-one
  [locations]
  (if (empty? locations)
    nil
    (concat (first locations) (merge-to-one (rest locations)))))

;;;  Part one --------------------------------------------------------------------

(defn two-antinodes 
  [x y dx dy]
    (let [nx1 (+ x (* dx 2))    ;; antinode at the lower region
          ny1 (+ y (* dy 2))
          nx2 (- x dx)          ;; antinode at the upper region
          ny2 (- y dy)]
          (cons (if (on-roof nx1 ny1)
                  [ nx1 ny1 ] 
                  nil) 
                (cons (if (on-roof nx2 ny2) 
                        [nx2 ny2] 
                        nil) nil))))

(defn generate-two-antinodes
  [antenna x y all-antennas]
  (if (empty? all-antennas)
    nil
     (let [check (first all-antennas)
           ca (first check)                 ;; 'ca' as check antenna
           cx (first (last check))          ;; 'cx' as check x-coordinate
           cy (last (last check))]          ;; 'cy' as check y-coordinate
       (if (and (= antenna ca) (smaller-pair x y cx cy))
         (cons (two-antinodes x y (- cx x) (- cy y)) 
               (generate-two-antinodes antenna x y (rest all-antennas)))
         (generate-two-antinodes antenna x y (rest all-antennas))))))

(defn generate-first-antinodes
  [scan-antennas all-antennas]
  (if (empty? scan-antennas)
    nil
    (let [check (first scan-antennas)
          antenna (first check)
          x (first (last check))
          y (last (last check))]
      (concat (generate-two-antinodes antenna x y all-antennas) 
            (generate-first-antinodes (rest scan-antennas) all-antennas)))))

(defn find-first-antinodes
  [antenna-data]
  (remove nil? (merge-to-one (generate-first-antinodes antenna-data antenna-data))))


;;;  Part two --------------------------------------------------------------------

(defn antinodes 
  [x y dx dy i]
  (if (= i maxx)
    nil
    (let [nx (+ x (* dx i))
          ny (+ y (* dy i))]
          (if (on-roof nx ny)
            (cons [nx ny] (antinodes x y dx dy (+ i 1)))
            (antinodes x y dx dy (+ i 1))))))

(defn generate-antinodes
  [antenna x y all-antennas]
  (if (empty? all-antennas)
    nil
     (let [check (first all-antennas)
           ca (first check)                 ;; 'ca' as check antenna
           cx (first (last check))          ;; 'cx' as check x-coordinate
           cy (last (last check))]          ;; 'cy' as check y-coordinate
       (if (and (= antenna ca) (smaller-pair x y cx cy))
         (concat (antinodes x y (- cx x) (- cy y) (- maxx)) 
                 (generate-antinodes antenna x y (rest all-antennas)))
         (generate-antinodes antenna x y (rest all-antennas))))))

(defn generate-all-antinodes
  [scan-antennas all-antennas]
  (if (empty? scan-antennas)
    nil
    (let [check (first scan-antennas)
          antenna (first check)
          x (first (last check))
          y (last (last check))]
      (cons (generate-antinodes antenna x y all-antennas) 
            (generate-all-antinodes (rest scan-antennas) all-antennas)))))

(defn find-all-antinodes
 [antenna-data] 
  (merge-to-one (generate-all-antinodes antenna-data antenna-data)))

(defn find-antinodes
  [part]
  (if (= part 1)
    (distinct (find-first-antinodes antenna-data))
    (distinct (find-all-antinodes   antenna-data))))


;;; The 'main' program - - -
(defn program []
  (println "Advent of Code 2024 - day 8  (Clojure)")
  (print   "Part one: The number of unique antinode locations is:  ")
  (println (count (find-antinodes 1)))
  (print   "Part two: The number of unique antinode locations is: ")
  (println (count (find-antinodes 2))) 
  (println "0K.\n"))


;; Run in terminal via: clojure -M p08ab.clj
(program)

