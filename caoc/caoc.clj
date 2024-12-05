;;;;    Advent of Code - helper functions
;;;;    https://adventofcode.com/
;;;;
;;;;    Current available functions:
;;;;       (halve list)                  split 'list' into two halves
;;;;       (merge-lists list1 list2)     merge two lists ordered
;;;;       (merge-sort list)             divide & conquer sort algorithm
;;;;    
;;;;
;;;;    (cl) by Arno Jacobs, 2024-12-04
;;;;    


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
