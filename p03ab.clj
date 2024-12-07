;;;;    Advent of Code 2024 - Day 3 part One and Two
;;;;    https://adventofcode.com/2024/day/3
;;;;
;;;;    Solutions in Clojure
;;;;    (Ter leering ende vermaeck...)
;;;;
;;;;    Part one: sum of all results of multiplications: 159892596
;;;;    Part two: sum of all results of multiplications:  92626942
;;;;
;;;;    (cl) by Arno Jacobs, 2024-12-08
;;;;    

(ns p03ab)

(def mul-start "mul(")
(def do-string "do()")
(def dont-string "don't()")

;;;; Read the data-set
(def data-set (slurp "./data/inputDay03_2024.txt"))

;;; Part one and two -----------------------------------------------------------

;;; Some string manipulations 

(defn string-after-helper
  [sub sln line]
  (if (empty? line) 
    nil 
    (if (= sub (take sln line))
      (drop sln line)
      (string-after-helper sub sln (rest line)))))

(defn string-after
  [sub line]
  (apply str (string-after-helper (seq sub) (count sub) line)))

(defn string-before-helper
  [sub sln line]
  (if (empty? line)
    nil
    (if (= sub (take sln line))
      nil
      (cons (first line) (string-before-helper sub sln (rest line))))))

(defn string-before
  [sub line]
  (apply str (string-before-helper (seq sub) (count sub) line)))

;;; Characters to numbers code 

(defn is-digit [c] (and (>= 0 (compare \0 c))
                        (>= 0 (compare c \9))))

(defn get-digits
  [line]
  (if (empty? line)
    nil
    (let [c (first line)]
        (if (is-digit c) 
        (cons c (get-digits (rest line)))
        nil))))

(defn to-Natural
  [digits]
  (if (empty? digits)
    0
    (+ (* 10 (to-Natural (drop-last 1 digits)))
       (- (int (last digits)) 48))))   ;; character-digit to integer
    
;;; Start of string should countain
;;; digits - comma - digits - closing parentheses 
;;; for example: "42,99)"
;;; If   correct  Return  the product of the two integers
;;; Else          Return  0  
(defn legal-product
  [line]
  (let [number1            (get-digits line)
        nat1               (to-Natural number1)
        lnn1               (count number1)
        comma-check        (= (nth line lnn1) \,)
        number2            (get-digits (drop (+ 1 lnn1) line))
        nat2               (to-Natural number2)
        lnn2               (inc (+ lnn1 (count number2)))
        parentheses-check  (= (nth line lnn2) \) )]
        (if (and comma-check parentheses-check)
          (* nat1 nat2)
          0)))

;; Create a list of multiplies
(defn extract-multipliers-helper
  [line]
  (if (empty? line)
    nil
    (let [mul-str-after (string-after mul-start line)]
      (if (is-digit (first mul-str-after))
        (cons (legal-product mul-str-after) (extract-multipliers-helper mul-str-after))
        (extract-multipliers-helper mul-str-after)))))

(defn extract-multipliers
  [line]
  (reduce + (extract-multipliers-helper line)))   ;; sum all multipliers


;;; Strip the data between all the "don't()" and "do()" combo's

(defn strip-donts-helper
  [line]
  (if (empty? line)
    nil
    (let [do-str-for   (string-before dont-string line)
          do-helper    (string-after  dont-string line)
          do-str-after (string-after  do-string   do-helper)]
      (concat do-str-for (strip-donts-helper do-str-after)))))

(defn strip-donts
  [line]
  (apply str (strip-donts-helper line)))


;;; The 'main' program - - -
(defn program []
  (println "Advent of Code 2024 - day 3  (Clojure)")
  (print   "Part one: sum of all results of multiplications: ")
  (println (extract-multipliers data-set))
  (print   "Part one: sum of all results of multiplications:  ")
  (println (extract-multipliers (strip-donts data-set)))
  (println "0K.\n"))

;; Run in terminal via: clojure -M p03ab.clj
(program)

