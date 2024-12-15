;;;;    Advent of Code - helper functions
;;;;    https://adventofcode.com/
;;;;
;;;;    Current available functions:
;;;;
;;;;       (is-digit char)               check is a character is a digit [0..9]
;;;;       (get-digits string)           get a list of digits
;;;;       (to-Natural list)             convert digits to a Natural (so positive)
;;;;       (strip-digits string)         strip all digits at the start of a string
;;;;       (get-integer string)          get the first integer (+/-) from a string
;;;;       (get-all-integers string)     get all integers from a string (into a list)
;;;;
;;;;       (halve list)                  split 'list' into two halves
;;;;       (merge-lists list1 list2)     merge two lists ordered
;;;;       (merge-sort list)             divide & conquer sort algorithm
;;;;    
;;;;
;;;;    (cl) by Arno Jacobs, 2024-12-15
;;;;    
;;;;    version 1.0     2024-12-04       first draft with merge-sort
;;;;    version 2.0     2024-12-15       added string to int conversions
;;;;                                     added namespace 'caoc/'
;;;;
;;;;
;;;;

;;; ------------------------------------------------------------------------------
;;; Integers from a string line

(ns caoc)

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
    
(defn strip-digits 
  [line]
  (if (empty? line)
    nil
    (if (is-digit (first line))
      (strip-digits (rest line))
      (apply str line))))
      
(defn get-integer
  [line]
  (if (empty? line)
    nil
    (let [c  (first line)
          rl (rest line)]
      (if (= c \-)
        (if (empty? rl)
          nil
          (cons (- (to-Natural (get-digits rl))) (strip-digits rl)))
        (if (is-digit c)
          (cons (to-Natural (get-digits line)) (strip-digits rl))
          (get-integer rl))))))
          
(defn get-all-integers
  [line]
  (if (empty? line)
    nil
    (let [pair (get-integer line)
          i (first pair)
          rl (apply str (rest pair))]
      (cons i (get-all-integers rl)))))



;;; ------------------------------------------------------------------------------
;;; Merge sort code

(defn halve 
  [some-list]
  (let [half-way (/ (count some-list) 2)]
    (list (take half-way some-list) 
          (drop half-way some-list))))

(defn merge-lists
  [left-list right-list]
  (cond (empty? left-list) right-list
        (empty? right-list) left-list
        :else (let [left-element (first left-list)
                    right-element (first right-list)] 
                (if (<= left-element right-element)      ;; For values only, probably faster
;;;                (if (< (compare left-element right-element) 0)
                  (conj (merge-lists (rest left-list) right-list) left-element)
                  (conj (merge-lists left-list (rest right-list)) right-element)))))

;;; Divide and Conquer algorithm
;;; https://en.wikipedia.org/wiki/Divide-and-conquer_algorithm
(defn merge-sort 
  [some-list]
  (if (< (count some-list) 2)
    some-list
    (let [two-halves (halve some-list)
          left-part (first two-halves)
          right-part (nth two-halves 1)]
      (merge-lists
       (merge-sort left-part)
       (merge-sort right-part)))))
